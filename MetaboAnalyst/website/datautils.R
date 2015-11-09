##################################################
## R script for MetaboAnalyst
## Description: data I/O
##
## Author: Jeff Xia, jeff.xia@mcgill.ca
## McGill University, Canada
##
## License: GNU GPL (>= 2)
###################################################

# create objects for storing data
# data type: list, conc, specbin, pktable, nmrpeak, mspeak, msspec
# anal type: stat, pathora, pathqea, msetora, msetssp, msetqea, ts, cmpdmap, smpmap
InitDataObjects <- function(dataType, analType, paired=F){
    dataSet <<- list();
    dataSet$type <<- dataType;
    dataSet$design.type <<- "regular"; # one factor to two factor
    dataSet$cls.type <<- "disc"; # default until specified otherwise
    dataSet$format <<- "rowu";
    dataSet$paired <<- paired;
    analSet <<- list();
    analSet$type <<- analType;
    imgSet <<- list();
    msg.vec <<- vector(mode="character");

    current.msetlib <<- NULL;
    conc.db <<- NULL;
    cmpd.db <<- NULL;

    # record the current name(s) to be transferred to client
    require('Cairo'); # plotting required by all

    # fix Mac font issue
    CairoFonts("Arial:style=Regular","Arial:style=Bold","Arial:style=Italic","Helvetica","Symbol")

    print("R objects intialized ...");
}

# for two factor time series only
SetDesignType <-function(design){
    dataSet$design.type <<- tolower(design);
}

# Read in the user uploaded CSV or TXT data,
# format: rowp, rowu, colp, colu
# label type: disc (for discrete) or cont (for continuous)
Read.TextData<-function(filePath, format="rowu", lbl.type="disc"){

    dataSet$cls.type <<- lbl.type;
    dataSet$format <<- format;

    formatStr <- substr(filePath, nchar(filePath)-2, nchar(filePath))
    if(formatStr == "txt"){
        dat <-try(read.table(filePath,header=TRUE,check.names=F, as.is=T));
    }else{ # note, read.csv is more than read.table with sep=","
        dat <-try(read.csv(filePath,header=TRUE,check.names=F, as.is=T));
    }

    # try to guess column numers and class labels (starts with #) from the top 20 rows

    if(class(dat) == "try-error") {
        AddErrMsg("Data format error. Failed to read in the data!");
        AddErrMsg("Please check the followings: ");
        AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
        AddErrMsg("We recommend using a combination of English letters, underscore, and numbers for naming purpose");
        AddErrMsg("Make sure sample names and feature (peak, compound) names are unique;");
        AddErrMsg("Missing values should be blank or NA without quote.");
        return(0);
    }

    if(ncol(dat) == 1){
        AddErrMsg("Error: Make sure the data table is saved as comma separated values (.csv) format!");
        AddErrMsg("Please also check the followings: ");
        AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
        AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose.");
        AddErrMsg("Make sure sample names and feature (peak, compound) names are unique.");
        AddErrMsg("Missing values should be blank or NA without quote.");
        return(0);
    }

    msg <- NULL;

    if(substring(format,4,5)=="ts"){
        # two factor time series data
        if(substring(format,1,3)=="row"){ # sample in row
            msg<-c(msg, "Samples are in rows and features in columns");
            smpl.nms <-dat[,1];
            all.nms <- colnames(dat);
            facA.lbl <- all.nms[2];
            cls.lbl<-facA <- dat[,2]; # default assign facA to cls.lbl in order for one-factor analysis
            facB.lbl <- all.nms[3];
            facB <- dat[,3];
            conc <- dat[,-c(1:3)];
            var.nms <- colnames(conc);
        }else{ # sample in col
            msg<-c(msg, "Samples are in columns and features in rows.");
            all.nms <- dat[,1];
            facA.lbl <- all.nms[1];
            cls.lbl <- facA <- dat[1,-1];
            facB.lbl <- all.nms[2];
            facB <- dat[2,-1];
            var.nms <- dat[-c(1:2),1];
            conc<-t(dat[-c(1:2),-1]);
            smpl.nms <- rownames(conc);
        }

        facA <- as.factor(as.character(facA));
        facB <- as.factor(as.character(facB));

        if(dataSet$design.type =="time"){
            # determine time factor
            if(tolower(facA.lbl) == "time"){
                dataSet$time.lbl <<- "facA";
            }else if(tolower(facB.lbl) == "time"){
                dataSet$time.lbl <<- "facB";
            }else{
                AddErrMsg("No time points found in your data");
                AddErrMsg("The time points group must be labeled as <b>Time</b>");
                return(0);
            }
        }
    }else{
        if(substring(format,1,3)=="row"){ # sample in row
            msg<-c(msg, "Samples are in rows and features in columns");
            smpl.nms <-dat[,1];
            dat[,1] <- NULL;
            if(lbl.type == "qc"){
                rownames(dat) <- smpl.nms;
                dataSet$orig<<-dat;
                dataSet$cmpd<<-colnames(dat);
                return(1);
            }

            cls.lbl <- dat[,1];
            conc <- dat[,-1];
            var.nms <- colnames(conc);
        }else{ # sample in col
            msg<-c(msg, "Samples are in columns and features in rows.");
            var.nms <- dat[-1,1];
            dat[,1] <- NULL;
            smpl.nms <- colnames(dat);
            cls.lbl <- dat[1,];
            conc<-t(dat[-1,]);
        }
    }

    # free memory
    dat <- NULL;

    msg<-c(msg, "The uploaded file is in comma separated values (.csv) format.");

    # try to remove empty line if present
    # identified if no sample names provided

    empty.inx <- is.na(smpl.nms) | smpl.nms == ""
    if(sum(empty.inx) > 0){
          msg<-c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty rows</font> were detected and excluded from your data."));
          smpl.nms <- smpl.nms[!empty.inx];
          cls.lbl <-  cls.lbl[!empty.inx];
          conc <- conc[!empty.inx, ];
    }

    # try to check & remove empty lines if class label is empty
    # Added by B. Han
    empty.inx <- is.na(cls.lbl) | cls.lbl == ""
    if(sum(empty.inx) > 0){
        if(analSet$type != "roc"){
            msg<-c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty labels</font> were detected and excluded from your data."));
            smpl.nms <- smpl.nms[!empty.inx];
            cls.lbl <-  cls.lbl[!empty.inx];
            conc <- conc[!empty.inx, ];
        }else{
            # force all NA to empty string, otherwise NA will become "NA" class label
            cls.lbl[is.na(cls.lbl)] <- "";
            msg<-c(msg, paste("<font color=\"orange\">", sum(empty.inx), "new samples</font> were detected from your data."));
        }
    }

    # try to remove check & remove empty line if sample name is empty
    empty.inx <- is.na(smpl.nms) | smpl.nms == "";
    if(sum(empty.inx) > 0){
        msg<-c(msg,paste("<font color=\"red\">", sum(empty.inx), "empty samples</font> were detected and excluded from your data."));
        smpl.nms <- smpl.nms[!empty.inx];
        cls.lbl <-  cls.lbl[!empty.inx];
        conc <- conc[!empty.inx, ];
    }

    # check for uniqueness of dimension name
    if(length(unique(smpl.nms))!=length(smpl.nms)){
            dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");;
            AddErrMsg("Duplicate sample names are not allowed!");
            AddErrMsg(dup.nm);
            return(0);
    }

    # try to remove check & remove empty line if feature name is empty
    empty.inx <- is.na(var.nms) | var.nms == "";
    if(sum(empty.inx) > 0){
        msg<-c(msg,paste("<font color=\"red\">", sum(empty.inx), "empty features</font> were detected and excluded from your data."));
        var.nms <- var.nms[!empty.inx];
        conc <- conc[,!empty.inx];
    }

    if(length(unique(var.nms))!=length(var.nms)){
            dup.nm <- paste(var.nms[duplicated(var.nms)], collapse=" ");
            AddErrMsg("Duplicate feature names are not allowed!");
            AddErrMsg(dup.nm);
            return(0);
    }

    # now check for special characters in the data labels
    if(sum(is.na(iconv(smpl.nms)))>0){
            AddErrMsg("No special letters (i.e. Latin, Greek) are allowed in sample names!");
            return(0);
    }

    if(sum(is.na(iconv(var.nms)))>0){
            AddErrMsg("No special letters (i.e. Latin, Greek) are allowed in feature names!");
            return(0);
    }

    # now assgin the dimension names
    rownames(conc) <- smpl.nms;
    colnames(conc) <- var.nms;

    cls.lbl <- ClearStrings(as.vector(cls.lbl));
    # check if paired or not
    if(dataSet$paired){
        label<-as.numeric(cls.lbl);
        dataSet$orig.cls<<-as.factor(ifelse(label>0,1,0));
        dataSet$pairs<<-label;
    }else{
        if(lbl.type == "disc"){
            # check for class labels at least two replicates per class
            if(min(table(cls.lbl)) < 3){
                AddErrMsg(paste ("A total of", length(levels(as.factor(cls.lbl))), "groups found with", length(smpl.nms), "samples."));
                AddErrMsg("At least three replicates are required in each group!");
                AddErrMsg("Or maybe you forgot to specify the data format?");
                return(0);
            }
            dataSet$orig.cls <<-dataSet$cls <<-as.factor(as.character(cls.lbl));
            if(substring(format,4,5)=="ts"){
                dataSet$orig.facA <<-dataSet$facA <<- as.factor(as.character(facA));
                dataSet$facA.lbl <<- facA.lbl;
                dataSet$orig.facB <<-dataSet$facB <<- as.factor(as.character(facB));
                dataSet$facB.lbl <<- facB.lbl;
            }
        }else{ # continuous
            dataSet$orig.cls <<- dataSet$cls <<- as.numeric(cls.lbl);
        }
    }

    # for the current being to support MSEA and MetPA
    if(dataSet$type == "conc"){
        dataSet$cmpd <<- var.nms;
    }

    dataSet$orig<<-conc; # copy to be processed in the downstream
    dataSet$read.msg<<-c(msg, paste("The uploaded data file contains ", nrow(conc),
                " (samples) by ", ncol(conc), " (", tolower(GetVariableLabel()), ") data matrix.", sep=""));
    return(1);
}

# Read peak list files
# NMR peak list input should be two-column numeric value (ppm, int), change ppm to mz and add dummy 'rt'
# MS peak list can be 2-col (mz, int), add dummy 'rt'
# MS can also be 3-col (mz, rt, int)
Read.PeakList<-function(foldername){
    suppressMessages(require(xcms));
    msg <- c("The uploaded files are peak lists and intensities data.");

    # the "upload" folder should contain several subfolders (groups)
    # each of the subfolder contains samples (.csv files)
    files<-dir(foldername, pattern=".[Cc][Ss][Vv]$", recursive=T, full.name=TRUE)
    if (length(files) == 0) {
        AddErrMsg("No peak list files (.csv) were found.");
        return(0);
    }

    snames <- gsub("\\.[^.]*$", "", basename(files));
    msg<-c(msg, paste("A total of ", length(files), "samples were found."));

    sclass <- gsub("^\\.$", "sample", dirname(files));

    scomp <- strsplit(substr(sclass, 1, min(nchar(sclass))), "");
    scomp <- matrix(c(scomp, recursive = TRUE), ncol = length(scomp));
    i <- 1
    while(all(scomp[i,1] == scomp[i,-1]) && i < nrow(scomp)){
        i <- i + 1;
    }
    i <- min(i, tail(c(0, which(scomp[1:i,1] == .Platform$file.sep)), n = 1) + 1)
    if (i > 1 && i <= nrow(scomp)){
        sclass <- substr(sclass, i, max(nchar(sclass)))
    }

    # some sanity check before proceeds
    sclass <- as.factor(sclass);
    if(length(levels(sclass))<2){
        AddErrMsg("You must provide classes labels (at least two classes)!");
        return(0);
    }

    # check for class labels at least three replicates per class
    if(min(table(sclass)) < 3){
        AddErrMsg("At least three replicates are required in each group!");
        return(0);
    }

    # check for unique sample names
    if(length(unique(snames))!=length(snames)){
        AddErrMsg("Duplcate sample names are not allowed!");
        dup.nm <- paste(snames[duplicated(snames)], collapse=" ");;
        AddErrMsg("Duplicate sample names are not allowed!");
        AddErrMsg(dup.nm);
        return(0);
    }

    # change sample names to numbers
    samp.num<-seq(1:length(snames));
    names(samp.num)<-snames;

    # create a matrix all.peaks compatible with xcmsSet@peaks matrix, so that grouping algorithm can be used directly
    # the matrix should have "mz", "rt", "into", "sample" 4 columns used for grouping
    # check 2 or 3 column

    ############## use try block to catch any error ##############
    pks<-try(as.matrix(read.csv(files[1], header=T)));

    if(class(pks) == "try-error") {
            AddErrMsg("The CSV file is not formatted correctly!");
            return(0);
     };
     ########################################################

    n.col<-ncol(pks);
    if(n.col==2){
	   add=TRUE;
    }else if(n.col==3){
	   add=FALSE;
    }else{
       AddErrMsg("Peak list file can only be 2 or 3 columns.");
	   return(0);
    }

    all.peaks<-NULL;

    for(i in 1:length(files)){
        print(files[i]);
        pks<-as.matrix(read.csv(files[i], header=T));
        if(ncol(pks)!=n.col){
            AddErrMsg("Columns in each file are not the same!");
            return(0);
        }

        if(add){ # NMR ppm+int or MS mz+int
            pks<-cbind(pks[,1], 1000, pks[,2],samp.num[i]);
        }else{
            pks<-cbind(pks,samp.num[i]);
        }
        all.peaks<-rbind(all.peaks, pks);
    }

    msg<-c(msg, paste("These samples contain a total of ", dim(all.peaks)[1], "peaks." ));
    msg<-c(msg, paste("with an average of ", round(dim(all.peaks)[1]/length(files), 1), "peaks per sample" ));

    colnames(all.peaks)<-c("mz","rt","int","sample");
    peakSet<-list(
        peaks = all.peaks,
        ncol = n.col,
        sampclass = sclass,
        sampnames = snames
    );
    dataSet$peakSet<<-peakSet;
    dataSet$read.msg<<-msg;
    return (1);
}


# read LC/GC-MS spectra(.netCDF, .mzXML, mzData)
# use functions in XCMS package
Read.MSspec<-function(folderName, profmethod='bin', fwhm=30, bw=30){
    suppressMessages(require(xcms));
	msfiles <- list.files(folderName, recursive=T, full.names=TRUE);

    # first do some sanity check b4 spending more time on that
    # note the last level is the file names, previous one should be the class label

    dir.table <- t(data.frame(strsplit(msfiles, "/")));
    cls.all<-dir.table[,ncol(dir.table)-1];
    smpl.all <- dir.table[,ncol(dir.table)];

   # check for groups
   if(length(levels(as.factor(cls.all))) < 2){
        dataSet$read.msg <<- "<font color='red'>At least two groups are required!</font>";
        return(0);
    }

    # check for min samples in each group
    if(min(table(cls.all)) < 3){
        dataSet$read.msg <<- "<font color='red'>At least three replicates are required in each group!</font>";
        return(0);
    }

    # check for unique sample names
    if(length(unique(smpl.all))!=length(smpl.all)){
        dataSet$read.msg <<- "<font color='red'>Duplcate sample names are not allowed!</font>";
        return(0);
    }

	xset <- xcmsSet(msfiles, profmethod = profmethod, fwhm=fwhm);
    msg<-c(paste("In total,", length(xset@filepaths), "sample files were detected. "),
            paste("They are divided into ", length(levels(xset@phenoData[,1]))," classes: ", paste(levels(xset@phenoData[,1]), collapse=', '), ".", sep=""));

    xset<-group(xset, bw=bw);
    dataSet$xset.orig<<-xset;
    dataSet$read.msg<<-msg;
    return(1);
}

# peak list or spectra files can be paired, the pair information
# is stored in a file with each line is a pair and names are separated by :,
ReadPairFile<-function(filePath="pairs.txt"){
    all.pairs<-scan(filePath, what='character', strip.white = T);
    labels<-as.vector(rbind(1:length(all.pairs), -(1:length(all.pairs))));
    all.names <- NULL;
    for(i in 1:length(all.pairs)){
        all.names=c(all.names, unlist(strsplit(all.pairs[i],":"), use.names=FALSE));
    }
    names(labels)<-all.names;
    labels;
}


# save the processed data with class names
SaveTransformedData<-function(){

    if(!is.null(dataSet$orig)){
        lbls <- NULL;
        tsFormat <- substring(dataSet$format,4,5)=="ts";
        if(tsFormat){
            lbls <- cbind(as.character(dataSet$orig.facA),as.character(dataSet$orig.facB));
            colnames(lbls) <- c(dataSet$facA.lbl, dataSet$facB.lbl);
        }else{
            lbls <- cbind("Label"= as.character(dataSet$orig.cls));
        }
        orig.data<-cbind(lbls, dataSet$orig);
        if(dim(orig.data)[2]>200){
            orig.data<-t(orig.data);
        }
        write.csv(orig.data, file="data_original.csv");
        if(!is.null(dataSet$proc)){
            if(tsFormat){
                lbls <- cbind(as.character(dataSet$proc.facA),as.character(dataSet$proc.facB));
                colnames(lbls) <- c(dataSet$facA.lbl, dataSet$facB.lbl);
            }else{
                lbls <- cbind("Label"= as.character(dataSet$proc.cls));
            }
            proc.data<-cbind(lbls, dataSet$proc);
            if(dim(proc.data)[2]>200){
                proc.data<-t(proc.data);
            }
            write.csv(proc.data, file="data_processed.csv");
            if(!is.null(dataSet$norm)){
                if(tsFormat){
                    lbls <- cbind(as.character(dataSet$facA),as.character(dataSet$facB));
                    colnames(lbls) <- c(dataSet$facA.lbl, dataSet$facB.lbl);
                }else{
                    lbls <- cbind("Label"= as.character(dataSet$cls));
                }

                # for ms peaks with rt and ms, insert two columns, without labels
                # note in memory, features in columns

                if(!is.null(dataSet$three.col)){ 
                    ids <- matrix(unlist(strsplit(colnames(dataSet$norm), "/")),ncol=2, byrow=T);
                    colnames(ids) <- c("mz", "rt");
                    new.data <- data.frame(ids, t(dataSet$norm));
                    write.csv(new.data, file="peak_normalized_rt_mz.csv");
                }

                norm.data<-cbind(lbls, dataSet$norm);
                if(dim(norm.data)[2]>200){
                    norm.data<-t(norm.data);
                }
                
                write.csv(norm.data, file="data_normalized.csv");

            }
        }
    }
}

AddErrMsg<-function(msg){
    if(!exists('msg.vec')){
        msg.vec <<- vector(mode="character");     # store error messages
    }
    msg.vec <<- c(msg.vec, msg);
}

GetErrMsg<-function(){
    return (msg.vec);
}

GetKEGG.PathNames<-function(){
    return(names(metpa$path.ids));
}

# given a vector of KEGGID, return a vector of KEGG compound names
KEGGID2Name<-function(ids){
    hit.inx<- match(ids, cmpd.db$kegg);
    return(cmpd.db[hit.inx, 3]);
}

# given a vector of KEGG pathway ID, return a vector of SMPDB IDs (only for hsa)
KEGGPATHID2SMPDBIDs<-function(ids){
     hit.inx<-match(ids, path.map[,1]);
     return(path.map[hit.inx, 3]);
}

# given a vector of HMDBID, return a vector of HMDB compound names
HMDBID2Name<-function(ids){
    hit.inx<- match(ids, cmpd.db$hmdb);
    return(cmpd.db[hit.inx, "name"]);
}

# given a vector of KEGGID, return a vector of HMDB ID
KEGGID2HMDBID<-function(ids){
    hit.inx<- match(ids, cmpd.db$kegg);
    return(cmpd.db[hit.inx, "hmdb_id"]);
}

# given a vector of HMDBID, return a vector of KEGG ID
HMDBID2KEGGID<-function(ids){
    hit.inx<- match(ids, cmpd.db$hmdb);
    return(cmpd.db[hit.inx, "kegg_id"]);
}


# save compound name for mapping
Setup.MapData<-function(qvec){
    dataSet$cmpd <<- qvec;
}

# save concentration data
Setup.ConcData<-function(conc){
    dataSet$norm <<- conc;
}

# save biofluid type for SSP
Setup.BiofluidType<-function(type){
    dataSet$biofluid <<- type;
}

GetLiteralGroupNames <- function(){
    as.character(dataSet$proc.cls);
}

# all groups
GetGroupNames <- function(){
    cls.lbl <- dataSet$proc.cls;
    if(analSet$type=="roc"){
        empty.inx <- is.na(cls.lbl) | cls.lbl == "";
        # make sure re-factor to drop level
        lvls <- levels(factor(cls.lbl[!empty.inx]));
    }else{
        lvls <- levels(cls.lbl);
    }
    return(lvls);
}

# groups entering analysis
GetNormGroupNames <- function(){
    levels(dataSet$cls);
}

SetOrganism <- function(org){
    inmex.org <<- org;
}


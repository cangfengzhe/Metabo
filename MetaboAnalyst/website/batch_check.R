##################################################
## R script for MetaboAnalyst
## Description: data I/O for batch effect checking
##
## Author: Jeff Xia, jeff.xia@mcgill.ca
## McGill University, Canada
##
## License: GNU GPL (>= 2)
###################################################

# Read multiple user uploaded CSV data one by one
# format: row, col
Read.BatchCSVdata<-function(filePath, format, label){

    dat<-try(read.csv(filePath, sep=",", header=T, check.names=F, as.is=T));
    if(class(dat) == "try-error") {
        AddErrMsg("Data format error. Failed to read in the data!");
        AddErrMsg("Please check the followings: ");
        AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
        AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose");
        AddErrMsg("Make sure sample names and feature (peak, compound) names are unique;");
        AddErrMsg("Missing values should be blank or NA without quote.");
        return("F");
    }

    if(ncol(dat) == 1){
        AddErrMsg("Error: Make sure the data table is saved as comma separated values (.csv) format!");
        AddErrMsg("Please also check the followings: ");
        AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
        AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose.");
        AddErrMsg("Make sure sample names and feature (peak, compound) names are unique.");
        AddErrMsg("Missing values should be blank or NA without quote.");
        return("F");
    }

    if(format=="row"){ # sample in row
        smpl.nms <-dat[,1];
        dat[,1] <- NULL;
        conc <- dat;
        var.nms <- colnames(conc);
    }else{ # sample in col
        var.nms <- dat[-1,1];
        dat[,1] <- NULL;
        smpl.nms <- colnames(dat);
        conc<-t(dat);
    }

    #checking and make sure QC labels are unique
    qc.inx <- toupper(substr(smpl.nms, 0, 2)) == "QC";

    qc.nms <- smpl.nms[qc.inx];
    qc.len <- sum(qc.inx);
    if(length(unique(qc.nms))!=length(qc.nms)){
        smpl.nms[qc.inx] <- paste("QC", length(dataSet$batch) + 1, 1:qc.len, sep="");
    }

    if(length(unique(smpl.nms))!=length(smpl.nms)){
            dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");;
            AddErrMsg("Duplicate sample names (except QC) are not allowed!");
            AddErrMsg(dup.nm);
            print(GetErrMsg());
            return("F");
    }

    if(length(unique(var.nms))!=length(var.nms)){
            dup.nm <- paste(var.nms[duplicated(var.nms)], collapse=" ");;
            AddErrMsg("Duplicate feature names are not allowed!");
            AddErrMsg(dup.nm);
            print(GetErrMsg());
            return("F");
    }
    # now check for special characters in the data labels
    if(sum(is.na(iconv(smpl.nms)))>0){
            AddErrMsg("No special letters (i.e. Latin, Greek) are allowed in sample names!");
            return("F");
    }

    if(sum(is.na(iconv(var.nms)))>0){
            AddErrMsg("No special letters (i.e. Latin, Greek) are allowed in feature names!");
            return("F");
    }

    # now assgin the dimension names
    rownames(conc) <- smpl.nms;
    colnames(conc) <- var.nms;

    label <- gsub("[/-]", "_",  label);

    if(nchar(label) > 10){
        label <- toupper(paste(substr(label, 0, 5), substr(label, nchar(label)-5, nchar(label)), sep=""));
    }

    # store the data into list of list with the name order index
    # first clean the label to get rid of unusually chars
    if(label %in% names(dataSet$batch) || label=="F"){
        label <- paste("Dataset", length(dataSet$batch) + 1, sep="");
    }

    # check numerical matrix
    int.mat <- conc;
    rowNms <- rownames(int.mat);
    colNms <- colnames(int.mat);
    naNms <- sum(is.na(int.mat));
    num.mat<-apply(int.mat, 2, as.numeric)

    msg<-NULL;
    if(sum(is.na(num.mat)) > naNms){
        # try to remove "," in thousand seperator if it is the cause
        num.mat <- apply(int.mat,2,function(x) as.numeric(gsub(",", "", x)));
        if(sum(is.na(num.mat)) > naNms){
             msg<-c(msg,"<font color=\"red\">Non-numeric values were found and replaced by NA.</font>");
        }else{
             msg<-c(msg,"All data values are numeric.");
        }
    }else{
        msg<-c(msg,"All data values are numeric.");
    }

    int.mat <- num.mat;

    rownames(int.mat)<-rowNms;
    colnames(int.mat)<-colNms;

    # replace NA
    minConc<-min(int.mat[int.mat>0], na.rm=T)/5;
    int.mat[is.na(int.mat)] <- minConc;

    dataSet$batch[[label]] <<- int.mat;

    # free memory
    gc();

    return(label);
}


# set up two matrix
# one is a batch containing summed concentration of each sample
# the other is contains the features aligned across all samples

SetupBatchData <- function(){

    # first get the common features from all batches and
    # set up class(or batch) labels for each samples after merge
    nms <- names(dataSet$batch);
    nm.list <- vector(length=length(dataSet$batch), mode="list");
    batch.lbls <- NULL; # record all batch labels
    for (i in 1:length(dataSet$batch)){
        batch <- dataSet$batch[[i]];
        nm.list[[i]] <- colnames(batch);
        batch.lbls <- c(batch.lbls, rep(nms[i], length=nrow(batch)));
    }
    cm.nms <- Reduce(intersect, nm.list); # get common names

    # now align all the batches
    dataSet$batch <- lapply(dataSet$batch, function(x){x[,cm.nms]});
    commonMat <- do.call(rbind, dataSet$batch);

    dataSet$commonMat <<- commonMat;
    dataSet$batch.lbls <<- factor(batch.lbls,levels=names(dataSet$batch), ordered=T);
}

GetQCCompoundNames <- function(){
    colnames(dataSet$commonMat);
}

GetAllBatchNames <- function(){
    if(is.null(dataSet$batch)){
        return(0);
    }
    names(dataSet$batch);
}

PlotHeatmapCheck <- function(imgName, format="png", dpi=72, width=NA){

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w  <- 10;
    }else{
        w <- width;
    }
    h <- w*9/10;

    if(format=="png"){
        bg="transparent";
    }else{
        bg="white";
    }
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg=bg);
    breaks = seq(-4,4,0.03125);
    colors <- topo.colors(256);

    rowTree <- NA;
    colTree <- NA;

    row.cols <- as.numeric(dataSet$batch.lbls) + 1;

    hc.dat <- as.matrix(dataSet$commonMat);
    rownames(hc.dat) <- rep("", nrow(hc.dat));
    plot.batchheatmap(hc.dat, Rowv = NA, Colv=NA, dendrogram="none", col = colors, breaks=breaks, keysize = 1.0, scale="column",
            trace="none", RowSideColors = row.cols, key=TRUE, legend.cols=unique(row.cols),
            legend.lbls = names(dataSet$batch), symkey=FALSE, density.info="none");
    dev.off();
}

# box and scatter plot colored by different batches
PlotBoxScatterCheck <- function(imgName, format="png", dpi=72, width=NA){

    smpl.sums <- apply(dataSet$commonMat, 1, sum, na.rm=T);
    smpl.nms <- rownames(dataSet$commonMat);
    qcInxs <- substr(smpl.nms, 0, 2) == "QC";

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w  <- 9;
    }else{
        w <- width;
    }
    h <- w*12/9;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");

    par(mfrow=c(2,1));

    # box plot
    par(xpd=T);

    boxplot.with.outlier.label(smpl.sums~dataSet$batch.lbls, smpl.nms, col="#0000ff22", boxwex=c(0.5, 0.5), las=2);

    if(sum(qcInxs) > 0){
        stripchart(smpl.sums[qcInxs]~dataSet$batch.lbls[qcInxs], ylim=ylim.ext, vertical=T,
                add = T, col="red", lwd=1.8);
    }

    # scatter plot
    cols <- CreateSemiTransColors(dataSet$batch.lbls);
    plot(smpl.sums, col=cols, pch=15, axes=F, ylab="Conc. Sum", xlab=NA);
    axis(2)

    # add QC points
    if(sum(qcInxs) > 0){
        x <- which(qcInxs);
        points(x, smpl.sums[qcInxs], pch="+", cex=1.8);
    }
    batch.lens <- unlist(lapply(dataSet$batch, nrow));
    mid.points <- batch.lens/2;
    axis(1, at=cumsum(batch.lens)-mid.points, labels=unique(dataSet$batch.lbls), las=2, xlim=c(0, sum(batch.lens)));
    box();

    dev.off();
}

PlotQCCmpd <- function(cmpdName, format="png", dpi=72, width=NA){

    nsmpls <- dataSet$commonMat[, cmpdName];
    nlbls <- dataSet$batch.lbls;

    smpl.nms <- rownames(dataSet$commonMat);
    qcInxs <- substr(smpl.nms, 0, 2) == "QC";

    imgName <- gsub("\\/", "_",  cmpdName);
    imgName = paste(imgName, "_dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w  <- 9;
    }else{
        w <- width;
    }
    h <- w*12/9;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mar=c(4,5,2,2), mfrow=c(2,1), oma=c(0,0,2,0));

    # box plot
    par(xpd=T);
    boxplot.with.outlier.label(nsmpls~nlbls, smpl.nms, col="#0000ff22", boxwex=c(0.5, 0.5), las=2);

    if(sum(qcInxs) > 0){
        stripchart(nsmpls[qcInxs]~nlbls[qcInxs], ylim=ylim.ext, vertical=T,
                add = T, col="red", lwd=1.8);
    }

    # scatter plot
    cols <- CreateSemiTransColors(dataSet$batch.lbls);
    plot(nsmpls, col=cols, axes=F, pch=15, ylab=NA, xlab=NA);
    axis(2, las=2)

    # add QC points
    if(sum(qcInxs) > 0){
        x <- which(qcInxs);
        points(x, nsmpls[qcInxs], pch="+", cex=1.8);
    }

    batch.lens <- unlist(lapply(dataSet$batch, nrow));
    mid.points <- batch.lens/2;
    axis(1, at=cumsum(batch.lens)-mid.points, labels=unique(nlbls), las=2, xlim=c(0, sum(batch.lens)));
    box();

    title(main=cmpdName, out=T);

    dev.off();
}

# scatter plot colored by different batches
PlotPCA.overview <- function(imgName, format="png", dpi=72, width=NA){

    nlbls <- dataSet$batch.lbls;
    pca <- prcomp(dataSet$commonMat, center=T, scale=T);
    sum.pca<-summary(pca);
    var.pca<-sum.pca$importance[2,]; # variance explained by each PC

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w  <- 9;
    }else{
        w <- width;
    }
    h <- w;
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");

    ## plot score plot
    pc1 = pca$x[, 1];
    pc2 = pca$x[, 2];

    xlabel = paste("PC1", "(", round(100*var.pca[1],1), "%)");
    ylabel = paste("PC2", "(", round(100*var.pca[2],1), "%)");

    semi.cols <- CreateSemiTransColors(dataSet$batch.lbls);
    plot(pc1, pc2, xlab=xlabel, ylab=ylabel, pch=21, bg=semi.cols, col="gray", cex=1.6, main="Score Plot");
    legend("topright", legend=unique(nlbls), pch=15, col=unique(semi.cols));

    qcInx <- substr(names(pc1), 0, 2) == "QC";
    if(sum(qcInx) > 0){
        points(pc1[qcInx], pc2[qcInx], pch=3, cex=2, lwd=2);
        #points(pc1[qcInx], pc2[qcInx], pch=3, cex=1.2, col=cols[qcInx]);
    }
    dev.off();
    analSet$pca <<-append(pca, list(variance=var.pca));
}

PlotPCA.batchview <- function(imgName, format="png", dpi=72, width=NA, batch.nm){

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w  <- 9;
    }else{
        w <- width;
    }
    h <- w;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    ## plot score plot
    pc1 = analSet$pca$x[, 1];
    pc2 = analSet$pca$x[, 2];
    xlabel = paste("PC1", "(", round(100*analSet$pca$variance[1],1), "%)");
    ylabel = paste("PC2", "(", round(100*analSet$pca$variance[2],1), "%)");

    hitInx <- dataSet$batch.lbls == batch.nm;

    plot(pc1, pc2, xlab=xlabel, ylab=ylabel, col="gray", main= batch.nm);
    points(pc1[hitInx], pc2[hitInx], col="blue", cex=1.2, pch=16);
    qcInx <- substr(names(pc1), 0, 2) == "QC";
    qcInx <- qcInx & hitInx;
    if(sum(qcInx) > 0){
        points(pc1[qcInx], pc2[qcInx], pch=3, cex=2, col="red", lwd=2);
    }
    dev.off();
}

##################################################
## R script for MetaboAnalyst
## Description: perform various normalization
##
## Author: Jeff Xia, jeff.xia@mcgill.ca
## McGill University, Canada
##
## License: GNU GPL (>= 2)
###################################################

###############################################################
# remove the sample or feature from data 
# Note: this should happen after processing and before normalization 
# dataSet$proc dataSet$proc.cls (make a copy of this pair for restore)
########################################################

UpdateGroupItems<-function(){
    if(!exists("grp.nm.vec")){
        current.msg <<- "Cannot find the current group names!";
        return (0);
    }

    hit.inx <- dataSet$proc.cls %in% grp.nm.vec;

    dataSet$prenorm <<- dataSet$proc[hit.inx,];
    dataSet$prenorm.cls <<- factor(dataSet$proc.cls[hit.inx], levels=grp.nm.vec);

    if(substring(dataSet$format,4,5)=="ts"){
        dataSet$prenorm.facA <<- factor(dataSet$proc.facA[hit.inx],levels=grp.nm.vec);
        dataSet$prenorm.facB <<- factor(dataSet$proc.facB[hit.inx],levels=grp.nm.vec);
    }

    current.msg <<- "Successfully updated the group items!";
    return (1);
}

UpdateSampleItems<-function(){
    if(!exists("smpl.nm.vec")){
        current.msg <<- "Cannot find the current sample names!";
        return (0);
    }

    hit.inx <- rownames(dataSet$proc) %in% smpl.nm.vec;
    dataSet$prenorm <<- dataSet$proc[hit.inx,];
    dataSet$prenorm.cls <<- dataSet$proc.cls[hit.inx];
    if(substring(dataSet$format,4,5)=="ts"){
        dataSet$prenorm.facA <<- dataSet$proc.facA[hit.inx];
        dataSet$prenorm.facB <<- dataSet$proc.facB[hit.inx];
    }
    current.msg <<- "Successfully updated the sample items!";
    return (1);
}

UpdateFeatureItems<-function(){
    if(!exists("feature.nm.vec")){
        current.msg <<- "Cannot find the selected feature names!";
        return (0);
    }

    hit.inx <- colnames(dataSet$proc) %in% feature.nm.vec;
    dataSet$prenorm <<- dataSet$proc[,hit.inx];
    dataSet$prenorm.cls <<- dataSet$proc.cls; # this is the same
    current.msg <<- "Successfully updated the sample items!";
    return (1);
}


Normalization<-function(rowNorm, transNorm, scaleNorm, ref=NULL, grpRef=NULL, ratio=FALSE, ratioNum=20){
    
    # now do actual filter if indicated
    if(!is.null(dataSet$remain)){
        remain <- dataSet$remain; 
        if(rowNorm == "CompNorm"){
            # make sure the ref is there, not filtered out
            hit.inx <- match(ref, colnames(dataSet$proc));
            remain[hit.inx] <- TRUE;
        }
        proc <- dataSet$proc[,remain];
    }else{
        proc <- dataSet$proc;
    }

    if(is.null(dataSet$prenorm)){
        data<- proc;
        cls <- dataSet$proc.cls;
        if(substring(dataSet$format,4,5)=="ts"){
            dataSet$facA <- dataSet$proc.facA;
            dataSet$facB <- dataSet$proc.facB;
            cls <- dataSet$facA;
        }
    }else{
        data<- dataSet$prenorm;
        cls <- dataSet$prenorm.cls;
        if(substring(dataSet$format,4,5)=="ts"){
            dataSet$facA <- dataSet$prenorm.facA;
            dataSet$facB <- dataSet$prenorm.facB;
            cls <- dataSet$facA;
        }
    }

    # note, samples may not be sorted by group labels
    ord.inx <- order(cls);
    data<-data[ord.inx, ];
    cls <-cls[ord.inx];

    if(substring(dataSet$format,4,5)=="ts"){
         nfacA <- dataSet$facA[ord.inx];
         nfacB <- dataSet$facB[ord.inx];
         dataSet$facA <<- nfacA;
         dataSet$facB <<- nfacB;

        if(dataSet$design.type =="time"){
            # determine time factor
            if(dataSet$time.lbl == "facA"){ 
                time.fac <- nfacA;
                exp.fac <- nfacB;
            }else{
                time.fac <- nfacB;
                exp.fac <- nfacA;
            }
            dataSet$time.fac <<- time.fac;
            dataSet$exp.fac <<- exp.fac;
        }
    }

    colNames <- colnames(data);
    rowNames <- rownames(data);
    
    # row-wise normalization
    if(rowNorm=="SpecNorm"){
        if(!exists("norm.vec")){
            norm.vec <- rep(1,nrow(data)); # default all same weight vec to prevent error
            print("No sample specific information were given, all set to 1.0");
         }
         rownm<-"Normalization by sample-specific factor";
         data<-data/norm.vec;
    }else if(rowNorm=="ProbNorm"){
        if(!is.null(ref)){
            if(grpRef == 'T'){
                grp.inx <- cls == ref;
                ref.smpl <- apply(proc[grp.inx, ], 2, mean);
            }else{
                ref.smpl <- proc[ref,];
            }
            data<-t(apply(data, 1, ProbNorm, ref.smpl));
            rownm<-"Probabilistic Quotient Normalization";
        }
    }else if(rowNorm=="CompNorm"){
        if(!is.null(ref)){
            data<-t(apply(data, 1, CompNorm, ref));
            rownm<-"Normalization by a reference feature";
        }
    }else if(rowNorm=="SumNorm"){
         data<-t(apply(data, 1, SumNorm));
         rownm<-"Normalization to constant sum";
    }else if(rowNorm=="MedianNorm"){
         data<-t(apply(data, 1, MedianNorm));
         rownm<-"Normalization to sample median";
    }else{
        # nothing to do
        rownm<-"N/A";
    }

   # use apply will lose dimesion info (i.e. row names and colnames)
   rownames(data)<-rowNames;
   colnames(data)<-colNames;

   # note: row-normed data is based on biological knowledge, since the previous
   # replacing zero/missing values by half of the min positive (a constant) 
   # now may become different due to different norm factor, which is artificial
   # variance and should be corrected again
   #
   # stopped, this step cause troubles
   # minConc<-round(min(data)/2, 5);
   # data[dataSet$fill.inx]<-minConc;

   # if the reference by feature, the feature column should be removed, since it is all 1
    if(rowNorm=="CompNorm" && !is.null(ref)){
        inx<-match(ref, colnames(data));
        data<-data[,-inx];
        colNames <- colNames[-inx];
    }

   # record row-normed data for fold change analysis (b/c not applicable for mean-centered data)
   dataSet$row.norm<<-as.data.frame(CleanData(data));

   # this is for biomarker analysis only (for compound concentraion data)
    if(ratio){
        min.val <- min(abs(data[data!=0]))/2;
        norm.data <- log2((data + sqrt(data^2 + min.val))/2);
        transnm<-"Log Normalization";
        ratio.mat <- CalculatePairwiseDiff(norm.data);

        fstats <- Get.Fstat(ratio.mat, dataSet$proc.cls);
        hit.inx <- rank(-fstats) < ratioNum;  # get top n
        ratio.mat <- ratio.mat[, hit.inx];

        data <- cbind(norm.data, ratio.mat);
        colNames <- colnames(data);
        rowNames <- rownames(data);
    }

    if(!ratio){
        # transformation
        if(transNorm=='LogNorm'){
            min.val <- min(abs(data[data!=0]))/10;
            data<-apply(data, 2, LogNorm, min.val);
            transnm<-"Log Normalization";
        }else if(transNorm=='CrNorm'){
            norm.data <- abs(data)^(1/3);
            norm.data[data<0] <- - norm.data[data<0];
            data <- norm.data;
            transnm<-"Cubic Root Transformation";
        }else{
            transnm<-"N/A";
        }
    }

    # scaling
    if(scaleNorm=='AutoNorm'){
            data<-apply(data, 2, AutoNorm);
            scalenm<-"Autoscaling";
    }else if(scaleNorm=='ParetoNorm'){
            data<-apply(data, 2, ParetoNorm);
            scalenm<-"Pareto Scaling";
    }else if(scaleNorm=='RangeNorm'){
            data<-apply(data, 2, RangeNorm);
            scalenm<-"Range Scaling";
    }else{
            scalenm<-"N/A";
    }

    # need to do some sanity check, for log there may be Inf values introduced
    data <- CleanData(data, T, F);

    # note after using "apply" function, all the attribute lost, need to add back
    rownames(data)<-rowNames;
    colnames(data)<-colNames;

    dataSet$norm <<- as.data.frame(data);
    dataSet$cls <<- cls;

    dataSet$rownorm.method<<-rownm;
    dataSet$trans.method<<-transnm;
    dataSet$scale.method<<-scalenm;
    dataSet$combined.method<<-FALSE;
    dataSet$norm.all <<- NULL; # this is only for biomarker ROC analysis
    return(1);
}

########################################
###row-wise norm methods, x is a row ###
########################################

# normalize by a sum of each sample, assume constant sum (1000)
# return: normalized data
SumNorm<-function(x){
	1000*x/sum(x, na.rm=T);
}

# normalize by median
MedianNorm<-function(x){
	x/median(x, na.rm=T);
}

# normalize by a reference sample (probability quotient normalization)
# ref should be the name of the reference sample
ProbNorm<-function(x, ref.smpl){
    x/median(as.numeric(x/ref.smpl), na.rm=T)
}

# normalize by a reference reference (i.e. creatinine)
# ref should be the name of the cmpd
CompNorm<-function(x, ref){
	1000*x/x[ref];
}

##############################################
###column-wise norm methods, x is a column ###
##############################################

# generalize log, tolerant to 0 and negative values
LogNorm<-function(x,min.val){
	 log2((x + sqrt(x^2 + min.val^2))/2)
}

# normalize to zero mean and unit variance
AutoNorm<-function(x){
	(x - mean(x))/sd(x, na.rm=T);
}

# normalize to zero mean but varaince/SE
ParetoNorm<-function(x){
	(x - mean(x))/sqrt(sd(x, na.rm=T));
}

# normalize to zero mean but varaince/SE
RangeNorm<-function(x){
    if(max(x) == min(x)){
        x;
    }else{
        (x - mean(x))/(max(x)-min(x));
    }
}

#######################################
####### Combined approach #############
#######################################
QuantileNormalize <- function(){
    data<-dataSet$proc;
    cls <- dataSet$proc.cls;
    cls.lvl <- levels(cls);

    # first log normalize
    data <- glog(data);

    require('preprocessCore');

    # normalize within replicates
    #for (lv in cls.lvl){
    #    sub.inx <- dataSet$proc.cls == lv;
    #    data[sub.inx, ] <- t(normalize.quantiles(t(data[sub.inx, ]), copy=FALSE));
    #}
    data <- t(normalize.quantiles(t(data), copy=FALSE));

    dataSet$norm <<- as.data.frame(data);
    dataSet$cls <<- cls;
    dataSet$rownorm.method<<-NULL;
    dataSet$colnorm.method<<-NULL;
    dataSet$combined.method<<-TRUE;
}

##############################################
################## Summary plot ##############
##############################################

# plot two summary plot, one b4 normalization, one after
# for each plot top is box plot, bottom is a density plot
PlotNormSummary<-function(imgName, format="png", dpi=72, width=NA){

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 10.5; h <- 12;
    }else if(width == 0){
        w <- 7.2;h <- 9;
        imgSet$norm<<-imgName;
    }else{
        w <- 7.2; h <- 9;
    }

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    layout(matrix(c(1,1,1,2,3,3,3,4), 4, 2, byrow = FALSE))

    # since there may be too many compounds, only plot a subsets (50) in box plot
    # but density plot will use all the data

    pre.inx<-GetRandomSubsetIndex(ncol(dataSet$proc), sub.num=50);
    namesVec <- colnames(dataSet$proc[,pre.inx]);

    # only get common ones
    nm.inx <- namesVec %in% colnames(dataSet$norm)
    namesVec <- namesVec[nm.inx];
    pre.inx <- pre.inx[nm.inx];

    norm.inx<-match(namesVec, colnames(dataSet$norm));
    namesVec <- substr(namesVec, 1, 12); # use abbreviated name

    rangex.pre <- range(dataSet$proc[, pre.inx], na.rm=T);
    rangex.norm <- range(dataSet$norm[, norm.inx], na.rm=T);

    x.label<-GetValueLabel();
    y.label<-GetVariableLabel();

    # fig 1
    op<-par(mar=c(0,7,4,0), xaxt="n");
    boxplot(dataSet$proc[,pre.inx], names= namesVec, ylim=rangex.pre, las = 2, col="lightgreen", horizontal=T);
	mtext("Before Normalization",3, 1)

    # fig 2
    op<-par(mar=c(7,7,0,0), xaxt="s");
    plot(density(apply(dataSet$proc, 2, mean, na.rm=TRUE)), col='darkblue', las =2, lwd=2, main="", xlab="", ylab="");
	mtext("Density", 2, 5);
	mtext(x.label, 1, 5);

    # fig 3
	op<-par(mar=c(0,7,4,2), xaxt="n");

    boxplot(dataSet$norm[,norm.inx], names=namesVec, ylim=rangex.norm, las = 2, col="lightgreen", horizontal=T);
	mtext("After Normalization",3, 1);

    # fig 4
  	op<-par(mar=c(7,7,0,2), xaxt="s");
    plot(density(apply(dataSet$norm, 2, mean, na.rm=TRUE)), col='darkblue', las=2, lwd =2, main="", xlab="", ylab="");
	mtext(paste("Normalized",x.label),1, 5);

    dev.off();
}
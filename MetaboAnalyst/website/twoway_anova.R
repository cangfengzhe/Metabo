#########################################################
## R script for MetaboAnalyst
## Description: perform two-way within/between subjects ANOVA
##
## Author: Jeff Xia, jeff.xia@mcgill.ca
## McGill University, Canada
##
## License: GNU GPL (>= 2)
###################################################

###############################################
################ Two-way ANOVA ################
###############################################

# perform within-subjects anova
aov.within <- function(x, time.fac) {
   # note: the error model time is the only within-subjects factor
   # the treatment is a subject level observation, each subject
   # only have one level of it
   unlist(summary(aov(x ~ (dataSet$facA*dataSet$facB) + Error(dataSet$sbj/time.fac))), use.names=F)[c(9,23,24)];
}

# perform between-subjects anova
aov.between <- function(x) {
   # note: the error model time is the only within-subjects factor
   # the treatment is a subject level observation, each subject
   # only have one level of it
   unlist(summary(aov(x ~ dataSet$facA*dataSet$facB)), use.names=F)[c(17,18,19)];
}

# p.value corrrection - bonferroni, holm, fdr,
# type b for between subjects, w for within
ANOVA2.Anal<-function(thresh=0.05, p.cor="fdr", type="b"){

    if(type=="w"){

        # first create the subjects that being measured under different time
        # points (under same phenotype/ exp. condition). The design must be balanced

        # first check if balanced
        res <- table (dataSet$facA, dataSet$facB);
        res.mean <- apply(res, 2, mean);
        all.res <- res/res.mean;
        #if(sum(all.res != 1) > 0){
        #    msg <- "Experiment design is not balanced!";
        #    AddErrMsg(msg);
        #    print(msg);
        #    return(0);
        #}

        time.fac <- dataSet$time.fac;
        exp.fac <- dataSet$exp.fac;

        sbj <- vector(mode="character", length=nrow(dataSet$norm));
        k = 1;
        len = 0;
        for(lv1 in levels(exp.fac)){
            # same subjects must in the same exp. condition
            inx1 <- exp.fac == lv1;
            for (lv2 in levels(time.fac)){
                inx2 <- time.fac == lv2;
                len <- sum(inx1 & inx2);

                # same subjects must not in the same time points
                # so in a balanced design and ordered by the time points
                # all samples in each time points will sweep all subjects (one go)
                sbj[inx1 & inx2] <- paste("S", k:(k+len-1), sep="");
            }
            k = k + len;
        }

        dataSet$sbj <<- as.factor(sbj);
        aov.mat<-t(apply(as.matrix(dataSet$norm), 2, aov.within, time.fac));
        fileName <- "anova_within_sbj.csv";
    }else{
        aov.mat<-t(apply(as.matrix(dataSet$norm), 2, aov.between));
        fileName <- "anova_between_sbj.csv";
    }
    rownames(aov.mat)<-colnames(dataSet$norm);

    if(p.cor != "none"){
        aov.mat <- cbind (p.adjust(aov.mat[,1], p.cor),
                          p.adjust(aov.mat[,2], p.cor),
                          p.adjust(aov.mat[,3], p.cor));
    }

    sig.facA <-(aov.mat[,1] <= thresh);
    sig.facB <-(aov.mat[,2] <= thresh);
    sig.intr <-(aov.mat[,3] <= thresh);

    all.match <- cbind(sig.facA, sig.facB, sig.intr);
    colnames(all.match) <- colnames(aov.mat) <- c(dataSet$facA.lbl, dataSet$facB.lbl, "Interaction");

    vennC <- getVennCounts(all.match);
    inx.imp <- sig.facA | sig.facB | sig.intr;
    aov.mat <- aov.mat[inx.imp, ,drop=F];

    # default sort first by main effect: treatment, then by ...
    ord.inx <- order(aov.mat[,1], aov.mat[,2], aov.mat[,3], decreasing = FALSE);

    aov.mat <- signif(aov.mat[ord.inx,,drop=F], 5);

    write.csv(aov.mat,file=fileName);

    aov2<-list (
          type = type,
          sig.nm = fileName,
          thresh = thresh,
          multi.c = p.cor,
          sig.mat = aov.mat,
          vennC = vennC
      );

    analSet$aov2<<-aov2;
}

# plot ven diagram for ANOVA results
PlotANOVA2<-function(imgName, format="png", dpi=72, width=NA){
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 7;
    }else if(width == 0){
        w <- 7;
        imgSet$anova2<<-imgName;
    }else{
        w <- width;
    }
    h <- w;

    title <- ifelse(analSet$aov2$type == "b", "Two-way ANOVA (between subjects)", "Two-way ANOVA (within subject)");
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    plotVennDiagram(analSet$aov2$vennC, circle.col=c("red", "blue", "green"), mar=c(0,0,2,0));
    mtext(title, NORTH<-3, line=0.25, cex=1.5);
	dev.off();
}

GetAov2SigFileName <-function(){
    analSet$aov2$sig.nm;
}

GetAov2SigMat<-function(){
   return(CleanNumber(as.matrix(analSet$aov2$sig.mat)));
}

GetAov2SigRowNames<-function(){
    rownames(analSet$aov2$sig.mat);
}

GetAov2SigColNames<-function(){
    colnames(analSet$aov2$sig.mat);
}

GetSigTable.Aov2<-function(){
    GetSigTable(analSet$aov2$sig.mat,
                "Significant features identified by two-way ANOVA");
}


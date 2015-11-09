#########################################################
## R script for MetaboAnalyst
## Description: perform fold change, t-tests, volcano plot
##
## Author: Jeff Xia, jeff.xia@mcgill.ca
## McGill University, Canada
##
## License: GNU GPL (>= 2)
###################################################


#####################################
########### Fold Change #############
#####################################

# fold change analysis, method can be mean or median
# note: since the interface allow user to change all parameters
# the fold change has to be re-calculated each time
FC.Anal.unpaired<-function(fc.thresh=2, cmp.type = 0){
    
    # make sure threshold is above 1
    fc.thresh = ifelse(fc.thresh>1, fc.thresh, 1/fc.thresh);
    max.thresh = fc.thresh;
    min.thresh = 1/fc.thresh;

    res <-GetFC(F, cmp.type);
    fc.all <- res$fc.all;
    fc.log <- res$fc.log;

    imp.inx <- fc.all > max.thresh | fc.all < min.thresh;
    sig.mat <- cbind(fc.all[imp.inx, drop=F], fc.log[imp.inx, drop=F]);
    colnames(sig.mat)<-c("Fold Change", "log2(FC)");

    # order by absolute log value (since symmetrical in pos and neg)
    inx.ord <- order(abs(sig.mat[,2]), decreasing=T);
    sig.mat <- sig.mat[inx.ord,,drop=F];

    fileName <- "fold_change.csv";
    write.csv(sig.mat,file=fileName);

    # create a list object to store fc
    fc<-list (
            paired = FALSE,
            raw.thresh = fc.thresh,
            max.thresh = max.thresh,
            min.thresh = min.thresh,
            fc.all = fc.all, # note a vector
            fc.log = fc.log,
            inx.imp = imp.inx,
            sig.mat = sig.mat
     );
     analSet$fc<<-fc;
}

FC.Anal.paired<-function(fc.thresh=2, percent.thresh=0.75, cmp.type=0){

    # make sure threshold is above 1
    fc.thresh = ifelse(fc.thresh>1, fc.thresh, 1/fc.thresh);
    max.thresh = fc.thresh;
    min.thresh = 1/fc.thresh;

    fc.mat <-GetFC(T, cmp.type);

    count.thresh<-round(nrow(dataSet$norm)/2*percent.thresh);
	mat.up <- fc.mat >= log(max.thresh,2);
	mat.down <- fc.mat <= log(min.thresh,2);

	count.up<-apply(mat.up, 2, sum);
 	count.down<-apply(mat.down, 2, sum);
	fc.all<-rbind(count.up, count.down);

 	inx.up <- count.up>=count.thresh;
 	inx.down <- count.down>=count.thresh;

    colnames(fc.all)<-colnames(dataSet$norm);
    rownames(fc.all)<-c("Count (up)", "Count (down)");
    sig.var <- t(fc.all[,(inx.up|inx.down), drop=F]);

    # sort sig.var using absolute difference between count(up)-count(down)
    sig.dff<-abs(sig.var[,1]-sig.var[,2])
    inx<-order(sig.dff, decreasing=T);
    sig.var<-sig.var[inx,,drop=F];

    fileName <- "fold_change.csv";
    write.csv(signif(sig.var,5),file=fileName);

    # create a list object to store fc
    fc<-list (
        paired = TRUE,
        fc.mat = fc.mat,
        raw.thresh = fc.thresh,
        max.thresh = count.thresh,
        min.thresh = -count.thresh,
        fc.all = fc.all, # note: a 2-row matrix!
        inx.up = inx.up,
        inx.down = inx.down,
        sig.mat = sig.var
    );
    analSet$fc<<-fc;
}

PlotFC<-function(imgName, format="png", dpi=72, width=NA){
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 8;
    }else if(width == 0){
        w <- 7;
        imgSet$fc<<-imgName;
    }else{
        w <- width;
    }
    h <- w*6/8;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");

    par(mar=c(5,5,2,3));

    fc = analSet$fc;
    if(fc$paired){
        ylim<-c(-nrow(dataSet$norm)/2, nrow(dataSet$norm)/2);
        xlim<-c(0, ncol(dataSet$norm));
        plot(NULL, xlim=xlim, ylim=ylim, xlab = GetVariableLabel(),
                ylab=paste("Count with FC >=", fc$max.thresh, "or <=", fc$min.thresh));
        for(i in 1:ncol(fc$fc.all)){
            segments(i,0, i, fc$fc.all[1,i], col= ifelse(fc$inx.up[i],"magenta", "darkgrey"),
                     lwd= ifelse(fc$inx.up[i], 2, 1));
            segments(i,0, i, -fc$fc.all[2,i], col= ifelse(fc$inx.down[i], "magenta", "darkgrey"),
                     lwd= ifelse(fc$inx.down[i], 2, 1));
        }
        abline(h=fc$max.thresh, lty=3);
        abline(h=fc$min.thresh, lty=3);
        abline(h=0, lwd=1);
    }else{
        if(fc$raw.thresh > 0){
            # be symmetrical
            topVal <- max(abs(fc$fc.log));
            ylim <- c(-topVal, topVal);
            plot(fc$fc.log,  ylab="Log2 (FC)", ylim = ylim, xlab = GetVariableLabel(), pch=19, axes=F,
                col= ifelse(fc$inx.imp, "magenta", "darkgrey"));
            axis(2);
            axis(4); # added by Beomsoo
            abline(h=log(fc$max.thresh,2), lty=3);
            abline(h=log(fc$min.thresh,2), lty=3);
            abline(h=0, lwd=1);
        }else{ # plot side by side

            dat1 <- dataSet$norm[as.numeric(dataSet$cls) == 1, ];
            dat2 <- dataSet$norm[as.numeric(dataSet$cls) == 2, ];

            mns1 <- apply(dat1, 2, mean);
            mn1 <- mean(mns1);
            sd1 <- sd(mns1);
            msd1.top <- mn1 + 2*sd1;
            msd1.low <- mn1 - 2*sd1;

            mns2 <- apply(dat2, 2, mean);
            mn2 <- mean(mns2);
            sd2 <- sd(mns2);
            msd2.top <- mn2 + 2*sd2;
            msd2.low <- mn2 - 2*sd2;

            ylims <- range(c(mns1, mns2, msd1.top, msd2.top, msd1.low, msd2.low));
            new.mns <- c(mns1, rep(NA, 5), mns2);
            cols <- c(rep("magenta", length(mns1)), rep(NA, 5), rep("blue", length(mns2)));
            pchs <- c(rep(15, length(mns1)), rep(NA, 5), rep(19, length(mns2)));
            plot(new.mns, ylim=ylims, pch = pchs, col = cols, cex = 1.25, axes=F, ylab="");
            axis(2);
            axis(4); # added by Beomsoo
            abline(h=mn1, col="magenta", lty=3, lwd=2);
            abline(h=msd1.low, col="magenta", lty=3, lwd=1);
            abline(h=msd1.top, col="magenta", lty=3, lwd=1);
            abline(h=mn2, col="blue", lty=3, lwd=2);
            abline(h=msd2.low, col="blue", lty=3, lwd=1);
            abline(h=msd2.top, col="blue", lty=3, lwd=1);
            # abline(h=mean(all.mns), col="darkgrey", lty=3);
            axis(1, at=1:length(new.mns), labels=c(1:length(mns1),rep(NA, 5),1:length(mns2)));
        }
    }
    dev.off();
}

GetSigTable.FC<-function(){
    GetSigTable(analSet$fc$sig.mat, "fold change analysis");
}

GetFCSigMat<-function(){
    return(CleanNumber(analSet$fc$sig.mat));
}

GetFCSigRowNames<-function(){
    rownames(analSet$fc$sig.mat);
}

GetFCSigColNames<-function(){
    colnames(analSet$fc$sig.mat);
}

# utility method to calculate FC
GetFC <- function(paired=FALSE, cmpType){
    if(paired){
        if(dataSet$combined.method){
            data <- dataSet$norm;
        }else{
            data <- log(dataSet$row.norm,2);
        }

        G1 <- data[which(dataSet$cls==levels(dataSet$cls)[1]), ]
        G2 <- data[which(dataSet$cls==levels(dataSet$cls)[2]), ]

        if(cmpType == 0){
            fc.mat <- G1-G2;
        }else{
            fc.mat <- G2-G1;
        }
        return (fc.mat);
    }else{
        if(dataSet$combined.method){
            data <- dataSet$norm;
            m1 <- colMeans(data[which(dataSet$cls==levels(dataSet$cls)[1]), ]);
            m2 <- colMeans(data[which(dataSet$cls==levels(dataSet$cls)[2]), ]);

            # create a named matrix of sig vars for display
            if(cmpType == 0){
                fc.log <- signif (m1-m2, 5);
            }else{
                fc.log <- signif (m2-m1, 5);
            }
            fc.all <- signif(2^fc.log, 5);
        }else{
            data <- dataSet$row.norm;
            m1 <- colMeans(data[which(dataSet$cls==levels(dataSet$cls)[1]), ]);
            m2 <- colMeans(data[which(dataSet$cls==levels(dataSet$cls)[2]), ]);

            # create a named matrix of sig vars for display
            if(cmpType == 0){
                ratio <- m1/m2;
            }else{
                ratio <- m2/m1;
            }
            fc.all <- signif(ratio, 5);
            fc.log <- signif(log2(ratio), 5);
        }
        names(fc.all)<-names(fc.log)<-colnames(dataSet$norm);
        return(list(fc.all = fc.all, fc.log = fc.log));
    }
}

#####################################
########### t-Tests ################
####################################

Ttests.Anal<-function(nonpar=F, threshp=0.05, paired=FALSE, equal.var=TRUE){

        p.value <- GetTtestP(paired, equal.var, nonpar);
        names(p.value)<-colnames(dataSet$norm);

        p.log <- -log10(p.value);
        fdr.p <- p.adjust(p.value, "fdr");

        inx.imp <- p.value <= threshp;
        sig.value <- p.value[inx.imp];
        fdr.p <-fdr.p[inx.imp];

        ord.inx <- order(sig.value);
        sig.p <- sig.value[ord.inx];
        sig.fdr <- fdr.p[ord.inx];
        lod<- -log10(sig.p);
        sig.mat <- cbind(sig.p, lod, sig.fdr);
        sig.mat <- signif(sig.mat, 5);
        colnames(sig.mat)<-c("p.value", "-log10(p)", "FDR");

        if(nonpar){
            tt.nm = "Wilcoxon Rank Test";
            fileName <- "Wilcoxon_rank.csv";            
        }else{
            tt.nm = "T-Tests";
            fileName <- "t_test.csv";
        }
        write.csv(sig.mat,file=fileName);

        ## generate univariate report file (univAnalReport.csv).
        ## mixed with t-test and wilcoxon test depend on each metabolite's distribution
        univAnal.mat <- getUnivAnal(paired);
        note.str <- paste("\n Univariate Analysis Result for each variable/metabolite\n\n",
                          "[NOTE]\n", 
                          "    p-value is calculated with t-test as a default.\n",
                          "    p-value with (W) is calculated by the Wilcoxon Mann Whitney test\n\n\n", sep='');

        cat(note.str, file="univAnalReport.csv", append=FALSE);
        write.table(univAnal.mat, file="univAnalReport.csv", append=TRUE, sep=",", row.names=FALSE);


        tt<-list (
            tt.nm = tt.nm,
            raw.thresh = threshp,
            p.value = sort(p.value),
            p.log = p.log,
            thresh = -log10(threshp), # only used for plot threshold line
            inx.imp = inx.imp,
            sig.mat = sig.mat,
            univAnal.mat = univAnal.mat
        );
        analSet$tt<<-tt;
}

PlotTT<-function(imgName, format="png", dpi=72, width=NA){
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 8;
    }else if(width == 0){
        w <- 7;
        imgSet$tt<<-imgName;
    }else{
        w <- width;
    }
    h <- w*6/8;
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    plot(analSet$tt$p.log, ylab="-log10(p)", xlab=GetVariableLabel(), main=analSet$tt$tt.nm, pch=19,
                 col= ifelse(analSet$tt$inx.imp, "magenta", "darkgrey"));
    abline (h=analSet$tt$thresh, lty=3);
    axis(4); 
    dev.off();
}

GetSigTable.TT<-function(){
    GetSigTable(analSet$tt$sig.mat, "t-tests");
}

# return a double matrix with 2 columns - p values and lod
GetTTSigMat<-function(){
    return(CleanNumber(analSet$tt$sig.mat));
}

GetTTSigRowNames<-function(){
    rownames(analSet$tt$sig.mat);
}

GetTTSigColNames<-function(){
    colnames(analSet$tt$sig.mat);
}

GetTtUpMat<-function(){
    lod <- analSet$tt$p.log;
    red.inx<- which(analSet$tt$inx.imp);
    as.matrix(cbind(red.inx, lod[red.inx]));
}

GetTtDnMat<-function(){
    lod <- analSet$tt$p.log;
    blue.inx <- which(!analSet$tt$inx.imp);
    as.matrix(cbind(blue.inx, lod[blue.inx]));
}

GetTtLnMat<-function(){
    lod <- analSet$tt$p.log;
    as.matrix(rbind(c(0, analSet$tt$thresh), c(length(lod)+1,analSet$tt$thresh)));
}

GetTtCmpds<-function(){
    names(analSet$tt$p.log);
}

GetMaxTtInx <- function(){
    which.max(analSet$tt$p.log);
}

# utility method to get p values
GetTtestP <- function(paired=FALSE, equal.var=TRUE, nonpar=F){
    if(nonpar){
         inx1 <- which(dataSet$cls==levels(dataSet$cls)[1]);
         inx2 <- which(dataSet$cls==levels(dataSet$cls)[2]);
         p.value <- apply(as.matrix(dataSet$norm), 2, function(x) {
                tmp <- try(wilcox.test(x[inx1], x[inx2], paired = paired));
                if(class(tmp) == "try-error") {
                    return(NA);
                }else{
                    return(tmp$p.value);
                }
        })
    }else{
        if(ncol(dataSet$norm) < 1000){
            inx1 <- which(dataSet$cls==levels(dataSet$cls)[1]);
            inx2 <- which(dataSet$cls==levels(dataSet$cls)[2]);
            p.value <- apply(as.matrix(dataSet$norm), 2, function(x) {
                tmp <- try(t.test(x[inx1], x[inx2], paired = paired, var.equal = equal.var));
                if(class(tmp) == "try-error") {
                    return(NA);
                }else{
                    return(tmp$p.value);
                }
            })
        }else{ # use fast version
            require(genefilter);
            p.value <- try(rowttests(t(as.matrix(dataSet$norm)), dataSet$cls)$p.value);
            if(class(p.value) == "try-error") {
               p.value <- NA;
            }
        }
    }
    return(p.value);
}

# utility method to perform the univariate analysis automatically
getUnivAnal <- function(paired=FALSE){
    inx1 <- which(dataSet$cls==levels(dataSet$cls)[1]);
    inx2 <- which(dataSet$cls==levels(dataSet$cls)[2]);

    # output list (mean(sd), mean(sd), p-value, FoldChange, Up/Down) 
    univStat.mat <- apply(as.matrix(dataSet$norm), 2, function(x) {
        # normality check        
        ks <- ks.test(x[inx1], x[inx2]); 
        method <- ifelse(ks$p.value <= 0.05, "(W)","")

        if (ks$p.value <= 0.05) {
            # wilcoxon test
            tmp <- try(wilcox.test(x[inx1], x[inx2], paired = paired));
        } else {
            # t-test
            equal.var <- TRUE;
            if(var(cbind(x[inx1], x[inx2]), na.rm=TRUE) != 0) {
                anal.var <- var.test(x[inx1], x[inx2]);
                equal.var <- ifelse(anal.var$p.value <= 0.05, FALSE, TRUE);
            }

            tmp <- try(t.test(x[inx1], x[inx2], paired = paired, var.equal = equal.var));
        }
        if(class(tmp) == "try-error") {
            return(NA);
        }else{            
            mean1 <- mean(x[inx1]);
            mean2 <- mean(x[inx2]);
            sd1 <- sd(x[inx1]);
            sd2 <- sd(x[inx2]);
            p.value <- paste(ifelse(tmp$p.value < 0.0001, "< 0.0001", sprintf("%.4f", tmp$p.value,4))," ", method, sep="");
            p.value.origin <- tmp$p.value;
            foldChange <- mean1 / mean2;
            foldChange <- round(ifelse( foldChange >= 1, foldChange, (-1/foldChange) ), 2);
            upDown <- ifelse(mean1 > mean2, "Up","Down");

            univStat <- c(
                meanSD1   = sprintf("%.3f (%.3f)", mean1, sd1),
                meanSD2   = sprintf("%.3f (%.3f)", mean2, sd2),
                p.value = p.value,
                foldChange = foldChange,
                upDown  = upDown,
                p.value.origin = p.value.origin
            );
            return(univStat);
        }
    })

    univStat.mat <- as.data.frame(t(univStat.mat));
    
    # add FDR/q-value
    q.value <- sprintf("%.4f", p.adjust(p=as.numeric(levels(univStat.mat$p.value.origin))[univStat.mat$p.value.origin], method='fdr'));
    univStat.mat <- cbind(univStat.mat[, c(1,2,3)], q.value, univStat.mat[, c(4,5)]);
    names(univStat.mat)[1] <- paste("Mean (SD) of \n", levels(dataSet$cls)[1], sep='');
    names(univStat.mat)[2] <- paste("Mean (SD) of \n", levels(dataSet$cls)[2], sep='');
    names(univStat.mat)[3] <- "p-value";
    names(univStat.mat)[4] <- "q-value (FDR)";
    names(univStat.mat)[5] <- "Fold Change";
    names(univStat.mat)[6] <- paste(levels(dataSet$cls)[1],"/", levels(dataSet$cls)[2], sep='');;

    univStat.mat <- cbind(Name=rownames(univStat.mat), univStat.mat);
    rownames(univStat.mat) <- NULL

    return(univStat.mat);
}


ContainInfiniteTT<-function(){
   if(sum(!is.finite(analSet$tt$sig.mat))>0){
        return("true");
   }
    return("false");
}

#####################################
########### Volcano ################
####################################

Volcano.Anal<-function(paired=FALSE,fcthresh,cmpType, percent.thresh, nonpar=F, threshp, equal.var=TRUE){

        #### t-tests
        p.value <- GetTtestP(paired, equal.var, nonpar);
        inx.p <- p.value <= threshp;
        p.log <- -log10(p.value);

        ### fold change analysis
        # make sure threshold is above 1
        fcthresh = ifelse(fcthresh>1, fcthresh, 1/fcthresh);
        max.xthresh <- log(fcthresh,2);
        min.xthresh <- log(1/fcthresh,2);

        if(paired){
            fc.mat <- GetFC(T, cmpType);
            count.thresh<-round(nrow(dataSet$norm)/2*percent.thresh);
            mat.up <- fc.mat >= max.xthresh;
            mat.down <- fc.mat <= min.xthresh;

            count.up<-apply(mat.up, 2, sum);
            count.down<-apply(mat.down, 2, sum);
            fc.all<-rbind(count.up, count.down);

            inx.up <- count.up>=count.thresh;
            inx.down <- count.down>=count.thresh;

            colnames(fc.all)<-colnames(dataSet$norm);
            rownames(fc.all)<-c("Count (up)", "Count (down)");

            fc.log <- NULL; # dummy, not applicable for counts

            # replace the count.thresh for plot
            max.xthresh <- count.thresh;
            min.xthresh <- -count.thresh;

        }else{
           res <- GetFC(F, cmpType);

            # create a named matrix of sig vars for display
            fc.log <- res$fc.log;
            fc.all <- res$fc.all;

            inx.up = fc.log > max.xthresh;
            inx.down = fc.log < min.xthresh;
        }

        # create named sig table for display
        inx.imp<-(inx.up | inx.down) & inx.p;
        if(paired){ 
            sig.var<-cbind(fc.all[1,][inx.imp,drop=F], fc.all[2,][inx.imp, drop=F], p.value[inx.imp, drop=F], p.log[inx.imp, drop=F]);
            colnames(sig.var)<-c("Counts (up)","Counts (down)", "p.value", "-log10(p)");
            # first order by count difference, then by log(p)
            dif.count<-abs(sig.var[,1]-sig.var[,2]);
            ord.inx<-order(dif.count, sig.var[,4], decreasing=T);
            sig.var<-sig.var[ord.inx,,drop=F];
            sig.var[,c(3,4)]<-signif(sig.var[,c(3,4)],5);
        }else{
            sig.var<-cbind(fc.all[inx.imp,drop=F], fc.log[inx.imp,drop=F], p.value[inx.imp,drop=F], p.log[inx.imp,drop=F]);
            colnames(sig.var)<-c("FC", "log2(FC)", "p.value", "-log10(p)");

            # first order by log(p), then by log(FC)
            ord.inx<-order(sig.var[,4], abs(sig.var[,2]), decreasing=T);
            sig.var<-sig.var[ord.inx,,drop=F];
            sig.var<-signif(sig.var,5);
        }

        fileName <- "volcano.csv";
        write.csv(signif(sig.var,5),file=fileName);
        volcano<-list (
            raw.threshx = fcthresh,
            raw.threshy = threshp,
            paired = paired,
            max.xthresh = max.xthresh,
            min.xthresh = min.xthresh,
            thresh.y = -log10(threshp),
            fc.all = fc.all,
            fc.log = fc.log,
            fc.log.uniq = jitter(fc.log),
            inx.up = inx.up,
            inx.down = inx.down,
            p.log = p.log,
            inx.p = inx.p,
            sig.mat = sig.var
        );
        analSet$volcano<<-volcano;
}

# now try to label the interesting points
# it is defined by the following rules
# need to be signficant (sig.inx) and
# or 2. top 5  p
# or 2. top 5 left
# or 3. top 5 right
PlotVolcano<-function(imgName, format="png", dpi=72, width=NA){
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 10;
    }else if(width == 0){
        w <- 8;
        imgSet$volcano<<-imgName;
    }else{
        w <- width;
    }
    h <- w*6/10;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mar=c(5,5,3,4));
    vcn<-analSet$volcano;
    MyGray <- rgb(t(col2rgb("black")), alpha=40, maxColorValue=255);
    MyHighlight <- rgb(t(col2rgb("magenta")), alpha=80, maxColorValue=255);
    if(vcn$paired){
        xlim<-c(-nrow(dataSet$norm)/2, nrow(dataSet$norm)/2)*1.2;

        # merge fc.all two rows into one, bigger one win
        fc.all <- apply(vcn$fc.all, 2, function(x){ if(x[1] > x[2]){return(x[1])}else{return(-x[2])}})

        hit.inx <- vcn$inx.p & (vcn$inx.up | vcn$inx.down);
        plot(fc.all, vcn$p.log, xlim=xlim, pch=20, cex=ifelse(hit.inx, 1.2, 0.8),
                col = ifelse(hit.inx, MyHighlight, MyGray),
                xlab="Count of Significant Pairs", ylab="-log10(p)");

        sig.upInx <- vcn$inx.p & vcn$inx.up;
        p.topInx <- GetTopInx(vcn$p.log, 5, T) & vcn$inx.up;
        fc.rtInx <- GetTopInx(vcn$fc.all[1,], 5, T);
        lblInx <- p.topInx & sig.upInx & fc.rtInx;
        if(sum(lblInx, na.rm=T) > 0){
            text.lbls<-substr(colnames(dataSet$norm)[lblInx],1,14) # some names may be too long
            text(vcn$fc.all[1,lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=4, col="blue", srt=30, xpd=T, cex=0.8);
        }

        sig.dnInx <- vcn$inx.p & vcn$inx.down;
        p.topInx <- GetTopInx(vcn$p.log, 5, T) & vcn$inx.down;
        fc.leftInx <- GetTopInx(vcn$fc.all[2,], 5, T) & vcn$inx.down;
        lblInx <-p.topInx & sig.dnInx & fc.leftInx;
        if(sum(lblInx, na.rm=T) > 0){
            text.lbls<-substr(colnames(dataSet$norm)[lblInx],1,14) # some names may be too long
            text(-vcn$fc.all[2,lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=2, col="blue", srt=-30, xpd=T, cex=0.8);
        }

    }else{
        imp.inx<-(vcn$inx.up | vcn$inx.down) & vcn$inx.p;
    	plot(vcn$fc.log, vcn$p.log, pch=20, cex=ifelse(imp.inx, 1.2, 0.7),
                col = ifelse(imp.inx, MyHighlight, MyGray),
                xlab="log2 (FC)", ylab="-log10(p)");

        sig.inx <- imp.inx;
        p.topInx <- GetTopInx(vcn$p.log, 5, T) & (vcn$inx.down);
        fc.leftInx <- GetTopInx(vcn$fc.log, 5, F);
        lblInx <-  sig.inx & (p.topInx | fc.leftInx);
        if(sum(lblInx, na.rm=T) > 0){
            text.lbls<-substr(colnames(dataSet$norm)[lblInx],1,14) # some names may be too long
            text(vcn$fc.log[lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=2, col="blue", srt=-30, xpd=T, cex=0.8);
        }

        p.topInx <- GetTopInx(vcn$p.log, 5, T) & (vcn$inx.up);
        fc.rtInx <- GetTopInx(vcn$fc.log, 5, T);
        lblInx <- sig.inx & (p.topInx | fc.rtInx);
        if(sum(lblInx, na.rm=T) > 0){
            text.lbls<-substr(colnames(dataSet$norm)[lblInx],1,14) # some names may be too long
            text(vcn$fc.log[lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=4, col="blue", srt=30, xpd=T, cex=0.8);
        }
    }

    abline (v = vcn$max.xthresh, lty=3);
    abline (v = vcn$min.xthresh, lty=3);
    abline (h = vcn$thresh.y, lty=3);
    axis(4); # added by Beomsoo
    dev.off();
}

GetVolcanoDnMat<- function(){
    vcn<-analSet$volcano;
    imp.inx<-(vcn$inx.up | vcn$inx.down) & vcn$inx.p;
    blue.inx<- which(!imp.inx);

    # make sure they are not tied
    xs <- vcn$fc.log.uniq[blue.inx]
    ys <- vcn$p.log[blue.inx];
    as.matrix(cbind(xs, ys));
}

GetVolcanoUpMat<- function(){
    vcn<-analSet$volcano;
    imp.inx<-(vcn$inx.up | vcn$inx.down) & vcn$inx.p;
    red.inx<- which(imp.inx);

    # make sure they are not tied
    xs <- vcn$fc.log.uniq[red.inx]
    ys <- vcn$p.log[red.inx];

    as.matrix(cbind(xs, ys));
}

GetVolcanoVlMat<- function(){
    vcn<-analSet$volcano;
    limy <- GetExtendRange(vcn$fc.log);
    as.matrix(rbind(c(vcn$min.xthresh, limy[1]), c(vcn$min.xthresh,limy[2])));
}

GetVolcanoVrMat<- function(){
    vcn<-analSet$volcano;
    limy <- GetExtendRange(vcn$fc.log);
    as.matrix(rbind(c(vcn$max.xthresh, limy[1]), c(vcn$max.xthresh,limy[2])));
}

GetVolcanoHlMat<- function(){
    vcn<-analSet$volcano;
    limx <- GetExtendRange(vcn$fc.log);
    as.matrix(rbind(c(limx[1], vcn$thresh.y), c(limx[2],vcn$thresh.y)));
}

GetVolcanoRangeX<- function(){
    range(analSet$volcano$fc.log.uniq);
}

GetVolcanoCmpds<- function(){
    names(analSet$volcano$fc.log);
}

GetVolcanoCmpdInxs<-function(){
    analSet$volcano$fc.log.uniq
}

# get indices of top n largest/smallest number
GetTopInx <- function(vec, n, dec=T){
    inx <- order(vec, decreasing = dec)[1:n];
    # convert to T/F vec
    vec<-rep(F, length=length(vec));
    vec[inx] <- T;
    return (vec);
}

GetSigTable.Volcano<-function(){
    GetSigTable(analSet$volcano$sig.mat, "volcano plot");
}

GetVolcanoSigMat<-function(){
    return(CleanNumber(analSet$volcano$sig.mat));
}

GetVolcanoSigRowNames<-function(){
   rownames(analSet$volcano$sig.mat);
}

GetVolcanoSigColNames<-function(){
   colnames(analSet$volcano$sig.mat);
}

ContainInfiniteVolcano<-function(){
   if(sum(!is.finite(analSet$volcano$sig.mat))>0){
        return("true");
   }
   return("false");
}

#################################################################
################ One-way ANOVA ##################################
#################################################################

# perform anova and only return p values and MSres (for Fisher's LSD)
aof <- function(x, cls = dataSet$cls) {
   aov(x ~ cls);
}

# perform Kruskal Wallis Test
kwtest <- function(x, cls = dataSet$cls) {
   kruskal.test(x ~ cls);
}

FisherLSD<-function(aov.obj, thresh){
    LSD.test(aov.obj,"cls", alpha=thresh)
}

# return only the signicant comparison names
parseTukey <- function(tukey, cut.off){
	inx <- tukey$cls[,"p adj"] <= cut.off;
	paste(rownames(tukey$cls)[inx], collapse="; ");
}

# return only the signicant comparison names
parseFisher <- function(fisher, cut.off){
	inx <- fisher[,"pvalue"] <= cut.off;
	paste(rownames(fisher)[inx], collapse="; ");
}

ANOVA.Anal<-function(nonpar=F, thresh=0.05, post.hoc="fisher"){

    if(nonpar){
        aov.nm <- "Kruskal Wallis Test";
        anova.res<-apply(as.matrix(dataSet$norm), 2, kwtest);

        #extract all p values
        p.value<-unlist(lapply(anova.res, function(x) {x$p.value}));
        names(p.value)<-colnames(dataSet$norm);
        fdr.p <- p.adjust(p.value, "fdr");

        inx.imp <- p.value <= thresh;
        if(sum(inx.imp) == 0){ # no sig features!
            cutpt <- round(0.2*length(p.value));
            cutpt <- ifelse(cutpt>50, 50, cutpt);
            inx <- which(rank(p.value) == cutpt);
            thresh <- p.value[inx]; 
            inx.imp <- p.value <= thresh;
        }
        sig.p <- p.value[inx.imp];
        fdr.p <- fdr.p[inx.imp];

        sig.mat <- data.frame(signif(sig.p,5), signif(-log10(sig.p),5), signif(fdr.p,5), 'NA');
        rownames(sig.mat) <- names(sig.p);
        colnames(sig.mat) <- c("p.value", "-log10(p)", "FDR", "Post-Hoc");

        # order the result simultaneously
        ord.inx <- order(sig.p, decreasing = FALSE);
        sig.mat <- sig.mat[ord.inx,];

        fileName <- "anova_posthoc.csv";
        my.mat <- sig.mat[,1:3];
        colnames(my.mat) <- c("pval_KW", "-log10(p)", "FDR");
        write.csv(my.mat,file=fileName);
    }else{
        aov.nm <- "One-way ANOVA";
        aov.res<-apply(as.matrix(dataSet$norm), 2, aof);
        anova.res<-lapply(aov.res, anova);

        #extract all p values
        p.value<-unlist(lapply(anova.res, function(x) { x["Pr(>F)"][1,]}));
        names(p.value)<-colnames(dataSet$norm);

        fdr.p <- p.adjust(p.value, "fdr");

        # do post-hoc only for signficant entries
        inx.imp <- p.value <= thresh;
        if(sum(inx.imp) == 0){ # no sig features with default thresh
            # readjust threshold to top 20% or top 50
            cutpt <- round(0.2*length(p.value));
            cutpt <- ifelse(cutpt>50, 50, cutpt);
            inx <- which(rank(p.value) == cutpt);
            thresh <- p.value[inx]; 
            inx.imp <- p.value <= thresh;
        }

        aov.imp <- aov.res[inx.imp];
        sig.p <- p.value[inx.imp];
        fdr.p <- fdr.p[inx.imp];

        cmp.res <- NULL;
        post.nm <- NULL;
        if(post.hoc=="tukey"){
            tukey.res<-lapply(aov.imp, TukeyHSD, conf.level=1-thresh);
            cmp.res <- unlist(lapply(tukey.res, parseTukey, cut.off=thresh));
            post.nm = "Tukey's HSD";
        }else{
            fisher.res<-lapply(aov.imp, FisherLSD, thresh);
            cmp.res <- unlist(lapply(fisher.res, parseFisher, cut.off=thresh));
            post.nm = "Fisher's LSD";
        }

        # create the result dataframe,
        # note, the last column is string, not double

        sig.mat <- data.frame(signif(sig.p,5), signif(-log10(sig.p),5), signif(fdr.p,5), cmp.res);
        rownames(sig.mat) <- names(sig.p);
        colnames(sig.mat) <- c("p.value", "-log10(p)", "FDR", post.nm);

        # order the result simultaneously
        ord.inx <- order(sig.p, decreasing = FALSE);
        sig.mat <- sig.mat[ord.inx,];

        fileName <- "anova_posthoc.csv";
        write.csv(sig.mat,file=fileName);
     }
     aov<-list (
        aov.nm = aov.nm,
        raw.thresh = thresh,
        thresh = -log10(thresh), # only used for plot threshold line
        p.value = p.value,
        p.log = -log10(p.value),
        inx.imp = inx.imp,
        post.hoc = post.hoc,
        sig.mat = sig.mat
    );
    analSet$aov<<-aov;
    return(1);
}

PlotANOVA<-function(imgName, format="png", dpi=72, width=NA){
    lod <- analSet$aov$p.log;
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 9;
    }else if(width == 0){
        w <- 7;
        imgSet$anova<<-imgName;
    }else{
        w <- width;
    }
    h <- w*6/9;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    plot(lod, ylab="-log10(p)", xlab = GetVariableLabel(), main=analSet$aov$aov.nm, type="n");
    red.inx<- which(analSet$aov$inx.imp);
    blue.inx <- which(!analSet$aov$inx.imp);
    points(red.inx, lod[red.inx], bg="red", cex=1.2, pch=21);
    points(blue.inx, lod[blue.inx], bg="green", pch=21);
    abline (h=analSet$aov$thresh, lty=3);
    dev.off();
}

GetAovSigMat<-function(){
    return(CleanNumber(as.matrix(analSet$aov$sig.mat[, 1:3])));
}

GetAovSigRowNames<-function(){
    rownames(analSet$aov$sig.mat);
}

GetAovSigColNames<-function(){
    colnames(analSet$aov$sig.mat[, 1:3]);
}

GetAovPostHocSig<-function(){
    analSet$aov$sig.mat[,4];
}

GetSigTable.Anova<-function(){
    GetSigTable(analSet$aov$sig.mat, "One-way ANOVA and post-hoc analysis");
}

GetAnovaUpMat<-function(){
    lod <- analSet$aov$p.log;
    red.inx<- which(analSet$aov$inx.imp);
    as.matrix(cbind(red.inx, lod[red.inx]));
}

GetAnovaDnMat<-function(){
    lod <- analSet$aov$p.log;
    blue.inx <- which(!analSet$aov$inx.imp);
    as.matrix(cbind(blue.inx, lod[blue.inx]));
}
GetAnovaLnMat<-function(){
    lod <- analSet$aov$p.log;
    as.matrix(rbind(c(0, analSet$aov$thresh), c(length(lod)+1,analSet$aov$thresh)));
}

GetAnovaCmpds<-function(){
    names(analSet$aov$p.log);
}

GetMaxAnovaInx <- function(){
    which.max(analSet$aov$p.log);
}

PlotCmpdView<-function(cmpdNm, format="png", dpi=72, width=NA){
   imgName <- gsub("\\/", "_",  cmpdNm);
   imgName <- paste(imgName, "_dpi", dpi, ".", format, sep="");
   Cairo(file = imgName, dpi=dpi, width=240, height=240, type=format, bg="transparent");
   par(mar=c(4,3,1,2), oma=c(0,0,1,0));
   boxplot(dataSet$norm[, cmpdNm]~dataSet$cls,las=2, col= unique(GetColorSchema()));
   title(main=cmpdNm, out=T);
   dev.off();
   return(imgName);
}

# change to use dataSet$proc instead of dataSet$orig in
# case of too many NAs
PlotCmpd<-function(cmpdNm, format="png", dpi=72, width=NA){
   imgName <- gsub("\\/", "_",  cmpdNm);
   imgName <- paste(imgName, "_dpi", dpi, ".", format, sep="");

   if(is.na(width)){
       w <- 9;
   }else{
       w <- width;
   }

   if(substring(dataSet$format,4,5)!="ts"){

        Cairo(file = imgName, unit="in", dpi=dpi, width=w, height= w*5/9, type=format, bg="white");
        par(mar=c(4,4,2,2), mfrow = c(1,2), oma=c(0,0,2,0));

        mns <- by(as.numeric(dataSet$proc[, cmpdNm]), dataSet$proc.cls, mean, na.rm=T);
        sds <- by(as.numeric(dataSet$proc[, cmpdNm]), dataSet$proc.cls, sd, na.rm=T);

        ups <- mns + sds;
        dns <- mns - sds;

        # all concentration need start from 0
        y <- c(0, dns, mns, ups);

        rg <- range(y) + 0.05 * diff(range(y)) * c(-1, 1)
        pt <- pretty(y)

        axp=c(min(pt), max(pt[pt <= max(rg)]),length(pt[pt <= max(rg)]) - 1);

        # ymk <- pretty(c(0,ymax));
        x <- barplot(mns, col= unique(GetColorSchema()), las=2, yaxp=axp, ylim=range(pt));
        arrows(x, dns, x, ups, code=3, angle=90, length=.1);
        axis(1, at=x, col="white", col.tick="black", labels=F);
        box();
        mtext("Original Conc.", line=1);

        boxplot(dataSet$norm[, cmpdNm]~dataSet$cls,las=2, col= unique(GetColorSchema()));
        mtext("Normalized Conc.", line=1);
        title(main=cmpdNm, out=T);
   }else{
        if(dataSet$design.type =="time"){ # time trend within phenotype
            out.fac <- dataSet$exp.fac;
            in.fac <- dataSet$time.fac;
            xlab="Time";
        }else{ # factor a split within factor b
            out.fac <- dataSet$facB;
            in.fac <- dataSet$facA;
            xlab=dataSet$facA.lbl;
        }

        # two images per row
        img.num <- length(levels(out.fac));
        row.num <- ceiling(img.num/2)

        if(row.num == 1){
            h <- w*5/9;
        }else{
            h <- w*0.5*row.num;
        }
        Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
        par(mar=c(3,4,4,2), mfrow=c(row.num, 2));
        # make sure all at the same range
        ylim.ext <-  GetExtendRange (dataSet$norm[, cmpdNm], 12);
        for(lv in levels(out.fac)){
            inx <- out.fac == lv;
            dat <- dataSet$norm[inx, cmpdNm];
            cls <- in.fac[inx];
            boxplot(dat ~ cls, col="#0000ff22", ylim=ylim.ext, outline=FALSE, boxwex=c(0.5, 0.5), xlab=xlab, ylab="Abundance", main=lv);
            stripchart(dat ~ cls, method = "jitter", ylim=ylim.ext, vertical=T, add = T, pch=19, cex=0.7, names = c("",""));
        }
   }
   dev.off();
   return(imgName);
}


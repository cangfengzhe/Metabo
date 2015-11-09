##################################################
## R script for MetaboAnalyst
## Description: quality check
##
## Author: Jeff Xia, jeff.xia@mcgill.ca
## McGill University, Canada
##
## License: GNU GPL (>= 2)
###################################################

# type 0, percent diff, 1 absolute diff
PlotComparision <- function(imgName, format="png", dpi=72, width=NA, thresh=0.1, type=0){

    ids <- rownames(dataSet$orig);
    nms <- colnames(dataSet$orig);
    x <- dataSet$orig[,1];
    y <- dataSet$orig[,2];

    rm.inx <- is.na(x) | is.na(y);
    x <- x[!rm.inx];
    y <- y[!rm.inx];
    ids <- ids[!rm.inx];
    nms <- nms[!rm.inx];

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- h <- 8;
    }else{
        w <- h <- width;
    }

    xlims <- ylims <- range(c(x, y), na.rm=T)

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    plot(x, y, xlab=nms[1], ylab=nms[2], xlim = xlims, ylim=ylims)
    abline(0,1, col="blue")
    grid();

    # label outliers
    if(type == 0){
        ratios <- x/y;
        good <- 1 - thresh;
        hit.inx <- which(ratios < good);
        if(sum(hit.inx) > 0){
            for(i in 1:length(hit.inx)){
                inx <- hit.inx[i];
                text(x[inx], y[inx], labels=ids[inx], pos=2, col="red", cex=0.8)
            }
        }
        hit.inx <- which(ratios > 1/good);
        if(sum(hit.inx) > 0){
            for(i in 1:length(hit.inx)){
                inx <- hit.inx[i];
                text(x[inx], y[inx], labels=ids[inx], pos=4, col="red", cex=0.8)
            }
        }
    }else{
        diffs <- x - y;
        hit.inx <- which(diffs < -thresh);
        if(sum(hit.inx) > 0){
            for(i in 1:length(hit.inx)){
                inx <- hit.inx[i];
                text(x[inx], y[inx], labels=ids[inx], pos=2, col="red", cex=0.8)
            }
        }
        hit.inx <- which(diffs > thresh);
        if(sum(hit.inx) > 0){
            for(i in 1:length(hit.inx)){
                inx <- hit.inx[i];
                text(x[inx], y[inx], labels=ids[inx], pos=4, col="red", cex=0.8)
            }
        }
    }
    dev.off();
}

TestTimeDrift <- function(splits=5){
    y <- dataSet$orig[,1];

    rm.inx <- is.na(y);
    y <- y[!rm.inx];

    # now divide y evenly into segments
    max <- ceiling(length(y)/splits);
    splits.list <- split(y, as.integer((seq_along(y) - 1)/max));

    seg.nms <- paste("Time", 1:splits);
    names(splits.list) <- seg.nms;

    cls.lbl <- NULL;
    for(i in 1:length(splits.list)){
        cls.lbl <- c(cls.lbl, rep(seg.nms[i], length(splits.list[[i]])));
    }
    cls.lbl <- factor(cls.lbl, levels=seg.nms, ordered=T);

    p.mat <- matrix(0,nrow=splits, ncol=splits);
    rownames(p.mat) <- colnames(p.mat) <- seg.nms;

    for(m in 1:(splits-1)){
        for(n in m:splits){
            p.mat[m,n] <- t.test(splits.list[[m]],splits.list[[n]], na.rm=T)$p.value
        }
    }

    analSet$time.drift$p.mat <<- signif(p.mat, 5);
    analSet$time.drift$splits <<- splits.list;
    analSet$time.drift$cls.lbl <<- cls.lbl;
    # do the lowess correction
    rm.vals <- lowess(y~1:length(y))$y;

    corr.dat <- dataSet$orig[!rm.inx,,drop=F];
    corr.dat[,1] <- y-rm.vals + mean(y);

    # format to the digits in the data
    digit.num <- max(apply(matrix(y), 1, getndp));
    corr.dat[,1] <- round(corr.dat[,1],digit.num)
    write.csv(corr.dat, file = "drift_corrected.csv");
}

# plot two panel up is scatter plot, bottom is box plot
# splits is to cut data into time windoes only for box plot
PlotDriftView <- function(imgName, format="png", dpi=72, width=NA){

    ids <- rownames(dataSet$orig);
    y <- dataSet$orig[,1];
    rm.inx <- is.na(y);
    y <- y[!rm.inx];
    ids <- ids[!rm.inx];

    x <- 1:length(y); # x is index

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w  <- 9;
    }else{
        w <- width;
    }
    h <- w;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mfrow = c(2,1));
    # scatter plot
    plot(x, y, ylab="Measure", xlab="Index", pch=21, col="gray")
    lines(lowess(y~x), lwd=2, col="#1c61b6");

    boxplot.with.outlier.label(y~analSet$time.drift$cls.lbl, ids, col="#0000ff22", boxwex=c(0.5, 0.5), las=2);
    dev.off();
}



PerformBiocheck<-function(imgName, format="png", dpi=72, width=NA){

    # first update the compound name to hmdb valid name
    nm.map <- GetFinalNameMap();

    valid.inx <- !(is.na(nm.map$hmdb)|duplicated(nm.map$hmdb));
    nm.map <- nm.map[valid.inx,];
    orig.nms <- nm.map$query;

    hmdb.inx <- match(dataSet$cmpd,orig.nms);
    match.inx <- !is.na(hmdb.inx);

    # note, must use "as.character" since string column from data frame will be converted to factors
    # when they used with numerics, they will be changed to numbers, not string
    ref.nm <- as.character(nm.map$hmdb[hmdb.inx[match.inx]]);

    # now get the hmdb conc refs
    match.inx <- match(tolower(ref.nm), tolower(cmpd.db$name));

    # get reference values from lib
    ref.lows <- ref.highs <- ref.means <- rep(NA,length(ref.nm));
    for(i in 1:length(ref.nm)){
            inx <- match.inx[i];
            if(!is.na(inx)){ # no match to HMDB ID
                hits <- Get.ConcRef(ref.nm[i]);
                if(!is.na(hits)){
                    concs<-as.numeric(unlist(strsplit(hits$conc, " - "), use.names = FALSE));

                    low.inx<-seq(1,length(concs)-2, 3);
                    mean.inx<-seq(2,length(concs)-1, 3);
                    high.inx<-seq(3,length(concs), 3);

                    ref.lows[i]<-min(concs[low.inx], na.rm=T);
                    ref.highs[i]<-max(concs[high.inx], na.rm=T);
                    ref.means[i]<-mean(concs[mean.inx], na.rm=T);
                }
            }
    }

    # remove cmpounds with no hits
    hit.inx <- !is.na(ref.means);
    ref.lows <- ref.lows[hit.inx];
    ref.highs <- ref.highs[hit.inx];
    ref.means <- ref.means[hit.inx];
    ref.nm <- ref.nm[hit.inx];

    ref.mat <- dataSet$orig[,match.inx & hit.inx];
    colnames(ref.mat) <- ref.nm;

    # first estimate sd from the user data
    sds <- apply(ref.mat, 2, sd);

    tstat.mat <- ref.mat;
    # now, compute the state using t-like stats
    tstat.lows <- sweep(ref.mat, 2,ref.lows)/sds;
    tstat.means <- sweep(ref.mat, 2,ref.means)/sds;
    tstat.highs <- sweep(ref.mat, 2,ref.highs)/sds;

    tstat.mat[tstat.lows < 0] <- tstat.lows[tstat.lows < 0];
    tstat.mat[tstat.highs > 0] <- tstat.highs[tstat.highs > 0];
    tstat.mat[!(tstat.lows < 0 | tstat.highs > 0)] <- 0;

    rownames(tstat.mat) <- rownames(ref.mat);
    colnames(tstat.mat) <- colnames(ref.mat);
    names(ref.means) <- names(sds) <- names(ref.lows) <- names(ref.means) <- names(ref.highs) <- ref.nm;

    dataSet$proc <<- ref.mat;
    analSet$qc$biocheck.mat <<- tstat.mat;
    analSet$qc$biocheck.sds <<- sds;
    analSet$qc$biocheck.ref.means <<- ref.means;
    analSet$qc$biocheck.ref.highs <<- ref.highs;
    analSet$qc$biocheck.ref.lows <<- ref.lows;

    # plot heatmap on the tstat
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w  <- 9;
    }else{
        w <- width;
    }
    h <- w*10/9;

    if(format=="png"){
        bg="transparent";
    }else{
        bg="white";
    }
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg=bg);

    # make it symmetrical
    hc.dat <- as.matrix(tstat.mat);
    colors <- colorRampPalette(c("green", "yellow", "red"), space="rgb")(10);
    breaks <- seq(-5,5,by=1);
    plot.tstatheatmap(hc.dat, Colv=F, Rowv=F, col = colors, breaks=breaks, scale="none",
            key=TRUE, trace="none", density.info=c("none"));
    dev.off();
}

GetBioCheckCmpdNames <- function(){
    colnames(dataSet$proc);
}

GetBioCheckSmplNames <- function(){
    rownames(dataSet$proc);
}

# For compounds, use original concentration vs. ref and sd
GetBioCheckCmpdStat <-  function(cmpdName, format="png", dpi=72, width=NA){

    cmpd.conc <- dataSet$proc[, cmpdName];
    cmpd.sd <- analSet$qc$biocheck.sds[cmpdName];
    ref.mean <- analSet$qc$biocheck.ref.means[cmpdName];
    ref.high <- analSet$qc$biocheck.ref.highs[cmpdName];
    ref.low <- analSet$qc$biocheck.ref.lows[cmpdName];
    ylims <- GetExtendRange(c(cmpd.conc, ref.mean, ref.high, ref.low));

    nName <- gsub("\\/", "_",  cmpdName);
    imgName = paste(nName, "_dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w  <- 8;
    }else{
        w <- width;
    }
    h <- w*5/8;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");

    plot(cmpd.conc, ylim=ylims, xlab=NA, ylab="Conc.", axes=F, main=cmpdName);
    axis(2);
    axis(1, 1:length(cmpd.conc), substr(rownames(dataSet$proc), 1, 8), las=2, cex=0.8);
    box();
    abline(h=ref.mean, lty=2);
    abline(h=ref.low, lty=2);
    abline(h=ref.high, lty=2);
    dev.off();

    stat.mat <- cbind(cmpd.conc, ref.low, ref.mean, ref.high);
    colnames(stat.mat) <- c("Measured", "Min", "Mean" ,"Max");
    rownames(stat.mat) <- rownames(dataSet$proc);

    fileName <- paste("Biocheck_", nName,".csv", sep="");
    write.csv(round(stat.mat,4), file=fileName);

}

# for sample, use tstat as concentrations can be of different scales
GetBioCheckSampleStat <-  function(smplName, format="png", dpi=72, width=NA){

    smpl.tstats <- unlist(analSet$qc$biocheck.mat[smplName,]);

    nName <- gsub("\\/", "_",  smplName);
    imgName = paste(nName, "_dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w  <- 8;
    }else{
        w <- width;
    }
    h <- w*5/8;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");

    plot(smpl.tstats, ylim=GetExtendRange(c(0, smpl.tstats)), ylab='Deviation', xlab=NA, axes=F,main=smplName);
    axis(2);
    axis(1, 1:length(smpl.tstats), substr(colnames(analSet$qc$biocheck.mat), 1, 8), las=2, cex=0.8);
    box();
    abline(h=0, lty=2);
    dev.off();

    smpl.tstats <- data.frame(smpl.tstats);
    rownames(smpl.tstats) <- colnames(analSet$qc$biocheck.mat);
    colnames(smpl.tstats) <- c("Deviation");

    fileName <- paste("Biocheck_", nName,".csv", sep="");
    write.csv(round(smpl.tstats,4), file=fileName);

}
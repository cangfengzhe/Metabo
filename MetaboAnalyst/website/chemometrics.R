##################################################
## R script for MetaboAnalyst
## Description: perform PCA and PLS-DA
##
## Author: Jeff Xia, jeff.xia@mcgill.ca
## McGill University, Canada
##
## License: GNU GPL (>= 2)
###################################################

############################
########### PCA #############
#############################

# perform PCA analysis
PCA.Anal<-function(){
    pca<-prcomp(dataSet$norm, center=F, scale=F);

    # obtain variance explained
    sum.pca<-summary(pca);
    imp.pca<-sum.pca$importance;
    std.pca<-imp.pca[1,]; # standard devietation
    var.pca<-imp.pca[2,]; # variance explained by each PC
    cum.pca<-imp.pca[3,]; # cummulated variance explained

    # store the item to the pca object
    analSet$pca<<-append(pca, list(std=std.pca, variance=var.pca, cum.var=cum.pca));

    write.csv(signif(analSet$pca$x,5), file="pca_score.csv");
    write.csv(signif(analSet$pca$rotation,5), file="pca_loadings.csv");
}

# format: png, tiff, pdf, ps, svg
PlotPCAPairSummary<-function(imgName, format="png", dpi=72, width=NA, pc.num){
    pclabels <- paste("PC", 1:pc.num, "\n", round(100*analSet$pca$variance[1:pc.num],1), "%");
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 10;
    }else if(width == 0){
        w <- 8;
        imgSet$pca.pair <<- imgName;
    }else{
        w <- width;
    }
    h <- w;
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    if(dataSet$cls.type == "disc"){
        pairs(analSet$pca$x[,1:pc.num], col=GetColorSchema(), pch=as.numeric(dataSet$cls)+1, labels=pclabels);
    }else{
        pairs(analSet$pca$x[,1:pc.num], labels=pclabels);
    }
    dev.off();
}

# scree plot
PlotPCAScree<-function(imgName, format="png", dpi=72, width=NA, scree.num){
    stds <-analSet$pca$std[1:scree.num];
	pcvars<-analSet$pca$variance[1:scree.num];
	cumvars<-analSet$pca$cum.var[1:scree.num];

    ylims <- range(c(pcvars,cumvars));
    extd<-(ylims[2]-ylims[1])/10
    miny<- ifelse(ylims[1]-extd>0, ylims[1]-extd, 0);
    maxy<- ifelse(ylims[2]+extd>1, 1.0, ylims[2]+extd);

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 10;
    }else if(width == 0){
        w <- 8;
        imgSet$pca.scree<<-imgName;
    }else{
        w <- width;
    }
    h <- w*2/3;
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mar=c(5,5,6,3));
    plot(pcvars, type='l', col='blue', main='Scree plot', xlab='PC index', ylab='Variance explained', ylim=c(miny, maxy), axes=F)
    text(pcvars, labels =paste(100*round(pcvars,3),'%'), adj=c(-0.3, -0.5), srt=45, xpd=T)
    points(pcvars, col='red');

    lines(cumvars, type='l', col='green')
    text(cumvars, labels =paste(100*round(cumvars,3),'%'), adj=c(-0.3, -0.5), srt=45, xpd=T)
    points(cumvars, col='red');

    abline(v=1:scree.num, lty=3);
    axis(2);
    axis(1, 1:length(pcvars), 1:length(pcvars));
    dev.off();
}

# 2D score plot
PlotPCA2DScore <- function(imgName, format="png", dpi=72, width=NA, pcx, pcy, reg = 0.95, show=1, grey.scale = 0){

    xlabel = paste("PC",pcx, "(", round(100*analSet$pca$variance[pcx],1), "%)");
    ylabel = paste("PC",pcy, "(", round(100*analSet$pca$variance[pcy],1), "%)");
    pc1 = analSet$pca$x[, pcx];
    pc2 = analSet$pca$x[, pcy];
    text.lbls<-substr(names(pc1),1,14) # some names may be too long

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 9;
    }else if(width == 0){
        imgSet$pca.score2d<<-imgName;
        w <- 7.2;
    }else{
        w <- width;
    }
    h <- w;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    suppressMessages(require('ellipse'));
    op<-par(mar=c(5,5,3,3));

    if(dataSet$cls.type == "disc"){
        # obtain ellipse points to the scatter plot for each category
        lvs <- levels(dataSet$cls);
        pts.array <- array(0, dim=c(100,2,length(lvs)));
        for(i in 1:length(lvs)){
            inx <-dataSet$cls == lvs[i];
            groupVar<-var(cbind(pc1[inx],pc2[inx]), na.rm=T);
            groupMean<-cbind(mean(pc1[inx], na.rm=T),mean(pc2[inx], na.rm=T));
            pts.array[,,i] <- ellipse(groupVar, centre = groupMean, level = reg, npoints=100);
        }

        xrg <- range (pc1, pts.array[,1,]);
        yrg <- range (pc2, pts.array[,2,]);
        x.ext<-(xrg[2]-xrg[1])/12;
        y.ext<-(yrg[2]-yrg[1])/12;
        xlims<-c(xrg[1]-x.ext, xrg[2]+x.ext);
        ylims<-c(yrg[1]-y.ext, yrg[2]+y.ext);

        cols <- GetColorSchema(grey.scale==1);
        uniq.cols <- unique(cols);

        plot(pc1, pc2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot",
             color=cols, pch=as.numeric(dataSet$cls)+1); ## added
        grid(col = "lightgray", lty = "dotted", lwd = 1);

        # make sure name and number of the same order DO NOT USE levels, which may be different
        legend.nm <- unique(as.character(dataSet$cls));
        ## uniq.cols <- unique(cols);

        ## BHAN: when same color is choosen; it makes an error
        if ( length(uniq.cols) > 1 ) {
            names(uniq.cols) <- legend.nm;
        }

        # draw ellipse
        for(i in 1:length(lvs)){
            if (length(uniq.cols) > 1) {
                polygon(pts.array[,,i], col=adjustcolor(uniq.cols[lvs[i]], alpha=0.25), border=NA);
            } else {
                polygon(pts.array[,,i], col=adjustcolor(uniq.cols, alpha=0.25), border=NA);
            }
            if(grey.scale) {
                lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
            }
        }

        pchs <- GetShapeSchema(show, grey.scale);
        if(grey.scale) {
            cols <- rep("black", length(cols));
        }
        if(show == 1){
            text(pc1, pc2, label=text.lbls, pos=4, xpd=T, cex=0.75);
            points(pc1, pc2, pch=pchs, col=cols);
        }else{
            if(length(uniq.cols) == 1){
                points(pc1, pc2, pch=pchs, col=cols, cex=1.0);
            }else{
                if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>0))){
                    points(pc1, pc2, pch=pchs, col=cols, cex=1.8);
                }else{
                    points(pc1, pc2, pch=21, bg=cols, cex=2);
                }
            }
        }
        uniq.pchs <- unique(pchs);
        if(grey.scale) {
            uniq.cols <- "black";
        }
        legend("topright", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);
    }else{
        plot(pc1, pc2, xlab=xlabel, ylab=ylabel, type='n', main="Scores Plot");
        points(pc1, pc2, pch=15, col="magenta");
        text(pc1, pc2, label=text.lbls, pos=4, col ="blue", xpd=T, cex=0.8);
    }
    par(op);
    dev.off();
}


# 3D score plot
PlotPCA3DScore<- function(imgName, format="json", inx1, inx2, inx3){

    pca <- analSet$pca;
    pca3d <- list();
    pca3d$score$axis <- paste("PC", c(inx1, inx2, inx3), " (", 100*round(analSet$pca$variance[c(inx1, inx2, inx3)], 3), "%)", sep="");
    coords <- data.frame(t(signif(pca$x[,c(inx1, inx2, inx3)], 5)));
    colnames(coords) <- NULL; 
    pca3d$score$xyz <- coords;
    pca3d$score$name <- rownames(dataSet$norm);
    cls <- as.character(dataSet$cls);
    if(all.numeric(cls)){
        cls <- paste("Group", cls);
    }
    pca3d$score$facA <- cls;

    # now set color for each group
    cols <- unique(GetColorSchema());
    rgbcols <- col2rgb(cols);
    cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")})
    pca3d$score$colors <- cols;
    imgName = paste(imgName, ".", format, sep="");
    require(RJSONIO);
    json.obj <- toJSON(pca3d, .na='null');
    sink(imgName);
    cat(json.obj);
    sink();
}

PlotPCA3DScore_orig <- function(imgName, format="png", dpi=72, width=NA, inx1, inx2, inx3, angl){

    xlabel = paste("PC",inx1, "(", round(100*analSet$pca$variance[inx1],1), "%)");
    ylabel = paste("PC",inx2, "(", round(100*analSet$pca$variance[inx2],1), "%)");
    zlabel = paste("PC",inx3, "(", round(100*analSet$pca$variance[inx3],1), "%)");

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 9;
    }else if(width == 0){
        w <- 7.2;
        imgSet$pca.score3d<<-imgName;
    }else{
        w <- width;
    }
    h <- w;
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");

    ## BHAN: changed pch=17 --> as.numeric(dataSet$cls)+1 to matched same symbols
    pchs <- as.numeric(dataSet$cls)+1;
    uniq.pchs <- unique(pchs);

    if(dataSet$cls.type == "disc"){
        cols <- GetColorSchema();
        legend.nm <- unique(as.character(dataSet$cls));
        uniq.cols <- unique(cols);
        Plot3D(analSet$pca$x[, inx1], analSet$pca$x[, inx2], analSet$pca$x[, inx3], xlab= xlabel, ylab=ylabel,
               zlab=zlabel, angle =angl, color=cols, pch=pchs, box=F);
        legend("topleft", legend =legend.nm, pch=uniq.pchs, col=uniq.cols);
    }else{
        Plot3D(analSet$pca$x[, inx1], analSet$pca$x[, inx2], analSet$pca$x[, inx3], xlab= xlabel, ylab=ylabel,
               zlab=zlabel, angle =angl, pch=pchs, box=F);
    }
    dev.off();
}

GetPCALoadAxesSpec<-function(){
    pca.axis.lims;
}

GetPCALoadCmpds<- function(){
    names(analSet$pca$load.x.uniq);
}

GetPCALoadCmpdInxs<-function(){
    analSet$pca$load.x.uniq;
}

GetPCALoadMat <- function(){
    as.matrix(cbind(analSet$pca$load.x.uniq, analSet$pca$imp.loads[,2]));
}

# plot PCA loadings and also set up the matrix for display
PlotPCALoading<-function(imgName, format="png", dpi=72, width=NA, inx1, inx2, plotType, lbl.feat=1){

    loadings<-signif(as.matrix(cbind(analSet$pca$rotation[,inx1],analSet$pca$rotation[,inx2])),5);
    ldName1<-paste("Loadings", inx1);
    ldName2<-paste("Loadings", inx2);
    colnames(loadings)<-c(ldName1, ldName2);
    load.x.uniq <- jitter(loadings[,1]);
    names(load.x.uniq) <- rownames(loadings);
    analSet$pca$load.x.uniq <<- load.x.uniq;
    analSet$pca$imp.loads<<-loadings; # set up the loading matrix

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 9;
    }else if(width == 0){
        w <- 7.2;
        imgSet$pca.loading<<-imgName;
    }else{
        w <- width;
    }
    h <- w;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    if(plotType=="scatter"){
        par(mar=c(6,5,2,6));
        plot(loadings[,1],loadings[,2], las=2, xlab=ldName1, ylab=ldName2);

        pca.axis.lims <<- par("usr"); # x1, x2, y1 ,y2

        grid(col = "lightgray", lty = "dotted", lwd = 1);
        points(loadings[,1],loadings[,2], pch=19, col="magenta");
        if(lbl.feat > 0){
            text(loadings[,1],loadings[,2], labels=substr(rownames(loadings), 1, 12), pos=4, col="blue", xpd=T);
        }
    }else{ # barplot
        layout(matrix(c(1,1,2,2,2), nrow=5, byrow=T), respect = FALSE)
        cmpd.nms <- substr(rownames(loadings), 1, 14);
        hlims <- c(min(loadings[,1], loadings[,2]), max(loadings[,1], loadings[,2]));

        par(mar=c(1,4,4,1));
        barplot(loadings[,1], names.arg=NA, las=2, ylim=hlims, main =ldName1);

        par(mar=c(10,4,3,1));
        barplot(loadings[,2], names.arg=cmpd.nms, las=2, cex.names=1.0, ylim=hlims, main =ldName2);
    }
    dev.off();
}

# Biplot, set xpd = T to plot outside margin
PlotPCABiplot<-function(imgName, format="png", dpi=72, width=NA, inx1, inx2){
	choices = c(inx1, inx2);
	scores<-analSet$pca$x;
    lam <- analSet$pca$sdev[choices]
    n <- NROW(scores)
    lam <- lam * sqrt(n);
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 9;
    }else if(width == 0){
        w <- 7.2;
        imgSet$pca.biplot<<-imgName;
    }else{
        w <- width;
    }
    h <- w;
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    biplot(t(t(scores[, choices]) / lam), t(t(analSet$pca$rotation[, choices]) * lam), xpd =T, cex=0.9);
    dev.off();
}

# for plotting, max top 9
GetMaxPCAComp<-function(){
    return (min(9, dim(dataSet$norm)[1]-1, dim(dataSet$norm)[2]));
}

###############################
########### PLS-DA #############
################################

# pls analysis using oscorespls so that VIP can be calculated
# note: the VIP is calculated only after PLSDA-CV is performed
# to determine the best # of comp. used for VIP
PLSR.Anal<-function(){
    suppressMessages(require('pls'));
    cls<-as.numeric(dataSet$cls)-1;
    datmat<-as.matrix(dataSet$norm);
    analSet$plsr<<-plsr(cls~datmat,method='oscorespls');
    write.csv(signif(analSet$plsr$scores,5), row.names=rownames(dataSet$norm), file="plsda_score.csv");
    write.csv(signif(analSet$plsr$loadings,5), file="plsda_loadings.csv");
}

# plot pairwise summary
PlotPLSPairSummary<-function(imgName, format="png", dpi=72, width=NA, pc.num){
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 9;
    }else if(width == 0){
        w <- 7.2;
        imgSet$pls.pair <<- imgName;
    }else{
        w <- width;
    }
    h <- w;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
	pclabels <- paste("Component", 1:pc.num, "\n", round(100*analSet$plsr$Xvar[1:pc.num]/analSet$plsr$Xtotvar,1), "%");
	# pairs(analSet$plsr$scores[,1:pc.num], col=as.numeric(dataSet$cls)+1, pch=as.numeric(dataSet$cls)+1, labels=pclabels)
    pairs(analSet$plsr$scores[,1:pc.num], col=GetColorSchema(), pch=as.numeric(dataSet$cls)+1, labels=pclabels)
    dev.off();
}

# score plot
PlotPLS2DScore<-function(imgName, format="png", dpi=72, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0){
    suppressMessages(require('ellipse'));

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 9;
    }else if(width == 0){
        w <- 7.2;
        imgSet$pls.score2d<<-imgName;
    }else{
        w <- width;
    }
    h <- w;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
	par(mar=c(5,5,3,3));
    lv1 <- analSet$plsr$scores[,inx1];
    lv2 <- analSet$plsr$scores[,inx2];
	xlabel <- paste("Component", inx1, "(", round(100*analSet$plsr$Xvar[inx1]/analSet$plsr$Xtotvar,1), "%)");
	ylabel <- paste("Component", inx2, "(", round(100*analSet$plsr$Xvar[inx2]/analSet$plsr$Xtotvar,1), "%)");

    text.lbls<-substr(rownames(dataSet$norm),1,12) # some names may be too long

    # obtain ellipse points to the scatter plot for each category
    lvs <- levels(dataSet$cls);
    pts.array <- array(0, dim=c(100,2,length(lvs)));
    for(i in 1:length(lvs)){
            inx <-dataSet$cls == lvs[i];
            groupVar<-var(cbind(lv1[inx],lv2[inx]), na.rm=T);
            groupMean<-cbind(mean(lv1[inx], na.rm=T),mean(lv2[inx], na.rm=T));
            pts.array[,,i] <- ellipse(groupVar, centre = groupMean, level = reg, npoints=100);
     }

     xrg <- range (lv1, pts.array[,1,]);
     yrg <- range (lv2, pts.array[,2,]);
     x.ext<-(xrg[2]-xrg[1])/12;
     y.ext<-(yrg[2]-yrg[1])/12;
     xlims<-c(xrg[1]-x.ext, xrg[2]+x.ext);
     ylims<-c(yrg[1]-y.ext, yrg[2]+y.ext);

     ## cols = as.numeric(dataSet$cls)+1;
     cols <- GetColorSchema(grey.scale==1);
     uniq.cols <- unique(cols);

     plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot");
     grid(col = "lightgray", lty = "dotted", lwd = 1);

     # make sure name and number of the same order DO NOT USE levels, which may be different
     legend.nm <- unique(as.character(dataSet$cls));
     ## uniq.cols <- unique(cols);

     ## BHAN: when same color is choosen for black/white; it makes an error
     # names(uniq.cols) <- legend.nm;
     if ( length(uniq.cols) > 1 ) {
         names(uniq.cols) <- legend.nm;
     }
     # draw ellipse
     for(i in 1:length(lvs)){
        if ( length(uniq.cols) > 1) {
            polygon(pts.array[,,i], col=adjustcolor(uniq.cols[lvs[i]], alpha=0.25), border=NA);
        } else {
            polygon(pts.array[,,i], col=adjustcolor(uniq.cols, alpha=0.25), border=NA);
        }
        if(grey.scale) {
            lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
        }
     }

     pchs <- GetShapeSchema(show, grey.scale);
     if(grey.scale) {
        cols <- rep("black", length(cols));
     }
     if(show==1){ # display sample name set on
        text(lv1, lv2, label=text.lbls, pos=4, xpd=T, cex=0.75);
        points(lv1, lv2, pch=pchs, col=cols);
     }else{
        if (length(uniq.cols) == 1) {
            points(lv1, lv2, pch=pchs, col=cols, cex=1.0);
        } else {
            if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>0))){
                points(lv1, lv2, pch=pchs, col=cols, cex=1.8);
            }else{
                points(lv1, lv2, pch=21, bg=cols, cex=2);
            }
        }
     }

     uniq.pchs <- unique(pchs);
     if(grey.scale) {
        uniq.cols <- "black";
     }
     legend("topright", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);

     dev.off();
}

# 3D score plot
PlotPLS3DScore<-function(imgName, format="json", inx1, inx2, inx3){
    pls3d <- list();
    pls3d$score$axis <- paste("Component", c(inx1, inx2, inx3), " (", round(100*analSet$plsr$Xvar[c(inx1, inx2, inx3)]/analSet$plsr$Xtotvar, 1), "%)", sep="");
    coords <- data.frame(t(signif(analSet$plsr$score[,c(inx1, inx2, inx3)], 5)));
    colnames(coords) <- NULL; 
    pls3d$score$xyz <- coords;
    pls3d$score$name <- rownames(dataSet$norm);
    cls <- as.character(dataSet$cls);
    if(all.numeric(cls)){
        cls <- paste("Group", cls);
    }
    pls3d$score$facA <- cls;

    # now set color for each group
    cols <- unique(GetColorSchema());
    rgbcols <- col2rgb(cols);
    cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")})
    pls3d$score$colors <- cols;

    imgName = paste(imgName, ".", format, sep="");
    require(RJSONIO);
    json.obj <- toJSON(pls3d, .na='null');
    sink(imgName);
    cat(json.obj);
    sink();
}

PlotPLS3DScore_orig<-function(imgName, format="png", dpi=72, width=NA, inx1, inx2, inx3, angl){

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 9;
    }else if(width == 0){
        w <- 7.2;
        imgSet$pls.score3d<<-imgName;
    }else{
        w <- width;
    }
    h <- w;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
	par(mar=c(5,5,3,3));

	xlabel <- paste("Component", inx1, "(", round(100*analSet$plsr$Xvar[inx1]/analSet$plsr$Xtotvar,1), "%)");
	ylabel <- paste("Component", inx2, "(", round(100*analSet$plsr$Xvar[inx2]/analSet$plsr$Xtotvar,1), "%)");
	zlabel <- paste("Component", inx3, "(", round(100*analSet$plsr$Xvar[inx3]/analSet$plsr$Xtotvar,1), "%)");

    cols <- GetColorSchema();
    legend.nm <- unique(as.character(dataSet$cls));
    uniq.cols <- unique(cols);
    pchs <- as.numeric(dataSet$cls)+1;
    uniq.pchs <- unique(pchs);
	Plot3D(analSet$plsr$score[,inx1], analSet$plsr$score[,inx2], analSet$plsr$score[,inx3], xlab= xlabel, ylab=ylabel,
		zlab=zlabel, angle =angl, color=cols, pch=pchs, box=F);
        legend("topleft", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);
    dev.off();
}

GetPLSLoadAxesSpec<-function(){
    pls.axis.lims;
}

GetPLSLoadCmpds<- function(){
    names(analSet$plsr$load.x.uniq);
}

GetPLSLoadCmpdInxs<-function(){
    analSet$plsr$load.x.uniq;
}

GetPLSLoadMat <- function(){
    as.matrix(cbind(analSet$plsr$load.x.uniq, analSet$plsr$imp.loads[,2]));
}

# plot loading plot, also set the loading matrix for display
PlotPLSLoading<-function(imgName, format="png", dpi=72, width=NA, inx1, inx2, plotType, lbl.feat=1){

    # named vector
    load1<-analSet$plsr$loadings[,inx1];
    load2<-analSet$plsr$loadings[,inx2];
    loadings = signif(as.matrix(cbind(load1, load2)),5);

    ldName1<-paste("Loadings", inx1);
    ldName2<-paste("Loadings", inx2)
    colnames(loadings)<-c(ldName1, ldName2);
    load.x.uniq <- jitter(loadings[,1]);
    names(load.x.uniq) <- rownames(loadings);
    analSet$plsr$load.x.uniq <<- load.x.uniq;
    analSet$plsr$imp.loads<<-loadings; # set up loading matrix

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 9;
    }else if(width == 0){
        w <- 7.2;
        imgSet$pls.loading<<-imgName;
    }else{
        w <- width;
    }
    h <- w;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    if(plotType == "scatter"){
        par(mar=c(6,4,4,5));
        plot(loadings[,1],loadings[,2], las=2, xlab=ldName1, ylab=ldName2);

        pls.axis.lims <<- par("usr"); # x1, x2, y1 ,y2

        grid(col = "lightgray", lty = "dotted", lwd = 1);
        points(loadings[,1],loadings[,2], pch=19, col="magenta");
        if(lbl.feat > 0){
            text(loadings[,1],loadings[,2], labels=substr(rownames(loadings), 1, 12), pos=4, col="blue", xpd=T);
        }
    }else{ # barplot
        cmpd.nms <- substr(rownames(loadings), 1, 14);
        hlims <- c(min(loadings[,1], loadings[,2]), max(loadings[,1], loadings[,2]));
        layout(matrix(c(1,1,2,2,2), nrow=5, byrow=T))
        par(mar=c(1,4,4,1));
        barplot(loadings[,1], names.arg=NA, las=2, ylim=hlims, main = ldName1);

        par(mar=c(10,4,3,1));
        barplot(loadings[,2], names.arg=cmpd.nms, cex.names=1.0, las=2, ylim=hlims, main = ldName2);
    }
    dev.off();
}

# classification and feature selection
PLSDA.CV<-function(methodName="T", compNum=GetDefaultPLSCVComp(), choice="Q2"){

    # get classification accuracy using caret
    suppressMessages(require('caret'));

    cls<-as.numeric(dataSet$cls)-1;
    datmat<-as.matrix(dataSet$norm);

    plsda.cls <- train(dataSet$norm, dataSet$cls, "pls", trControl=trainControl(method=ifelse(methodName == 'L', "LOOCV", 'CV')), tuneLength=compNum);

    # use the classifical regression to get R2 and Q2 measure
    plsda.reg <- plsr(cls~datmat,method ='oscorespls', ncomp=compNum, validation= ifelse(methodName == 'L', "LOO", 'CV'));
    fit.info <- pls::R2(plsda.reg, estimate = "all")$val[,1,];

    # combine accuracy, R2 and Q2
    accu <- plsda.cls$results[,2]
    all.info <- rbind(accu, fit.info[,-1]);
    rownames(all.info) <- c("Accuracy", "R2", "Q2");

    # default use best number determined by Q2
    if(choice == 'Q2'){
        best.num <- which(all.info[3,] == max(all.info[3,]));
    }else if(choice == "R2"){
        best.num <- which(all.info[2,] == max(all.info[2,]));
    }else{
        best.num <- which(all.info[1,] == max(all.info[1,]));
    }

    # get coef. table, this can be error when class is very unbalanced  
    coef.mat <- try(varImp(plsda.cls, scale=T)$importance);
    if(class(coef.mat) == "try-error") {
        coef.mat <- NULL;
    }else{
        if(dataSet$cls.num > 2){ # add an average coef for multiple class
            coef.mean<-apply(coef.mat, 1, mean);
            coef.mat <- cbind(coef.mean = coef.mean, coef.mat);
        }
        # rearange in decreasing order, keep as matrix, prevent dimesion dropping if only 1 col
        inx.ord<- order(coef.mat[,1], decreasing=T);
        coef.mat <- data.matrix(coef.mat[inx.ord, ,drop=FALSE]);
        write.csv(signif(coef.mat,5), file="plsda_coef.csv"); # added 27 Jan 2014
    }
    # calculate VIP http://mevik.net/work/software/VIP.R
    pls<-analSet$plsr;
    b <- c(pls$Yloadings)[1:compNum];
    T <- pls$scores[,1:compNum, drop = FALSE]
    SS <- b^2 * colSums(T^2)
    W <- pls$loading.weights[,1:compNum, drop = FALSE]
    Wnorm2 <- colSums(W^2);
    SSW <- sweep(W^2, 2, SS / Wnorm2, "*")
    vips <- sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS));
    if(compNum > 1){
        vip.mat <- as.matrix(t(vips));
    }else{
        vip.mat <- as.matrix(vips);
    }
    colnames(vip.mat) <- paste("Comp.", 1:ncol(vip.mat));
    write.csv(signif(vip.mat,5),file="plsda_vip.csv");

    analSet$plsda<<-list(best.num=best.num, choice=choice, coef.mat=coef.mat, vip.mat=vip.mat, fit.info=all.info);
    return(1);
}


# perform permutation, using training classification accuracy as
# indicator, for two or multi-groups
PLSDA.Permut<-function(num=100, type="accu"){

    orig.cls<-cls<-as.numeric(dataSet$cls);
    datmat<-as.matrix(dataSet$norm);
    best.num<-analSet$plsda$best.num;

    # dummy is not used, for the purpose to maintain lapply API
    Get.pls.bw <- function(dummy){
         cls <- cls[order(runif(length(cls)))];
         pls <- plsda(datmat, as.factor(cls), ncomp=best.num);
         pred <- predict(pls, datmat);
         Get.bwss(pred, cls);
    }

    Get.pls.accu <- function(dummy){
         cls <- cls[order(runif(length(cls)))];
         pls <- plsda(datmat, as.factor(cls), ncomp=best.num);
         pred <- predict(pls, datmat);
         sum(pred == cls)/length(cls);
    }

    # first calculate the bw values with original labels
    pls <- plsda(datmat, as.factor(orig.cls), ncomp=best.num);
    pred.orig <- predict(pls, datmat);
    if(type=="accu"){
        perm.type = "prediction accuracy";
        res.orig <- sum(pred.orig == orig.cls)/length(orig.cls);
        res.perm <- Perform.permutation(num, Get.pls.accu);
    }else{
        perm.type = "separation distance";
        res.orig <- Get.bwss(pred.orig, orig.cls);
        res.perm <- Perform.permutation(num, Get.pls.bw);
     }

    perm.vec <- c(res.orig, unlist(res.perm, use.names=FALSE));
    # check for infinite since with group variance could be zero for perfect classification
    inf.found = TRUE;
	if(sum(is.finite(perm.vec))==length(perm.vec)){
    		inf.found = FALSE;
	}else {
	 	if(sum(is.finite(perm.vec))==0){ # all are infinite, give a random number 10
			perm.vec<-rep(10, length(perm.vec));
		}else{ # if not all inf, replace with the 10 fold of non-inf values
    		perm.vec[!is.finite(perm.vec)]<-10*max(perm.vec[is.finite(perm.vec)]);
		}
	}

    # calculate the significant p value as the proportion of sampled permutations better than or equal to original one
    # note, the precision is determined by the permutation number i.e. for 100 time, no better than original
    # p value is < 0.01, we can not say it is zero
    better.hits <- sum(perm.vec[-1]>=perm.vec[1]);
    if(better.hits == 0) {
        p <- paste("p < ", 1/num, " (", better.hits, "/", num, ")", sep="");
    }else{
        p <- better.hits/num;
        p <- paste("p = ", signif(p, digits=5), " (", better.hits, "/", num, ")", sep="");
    }

    analSet$plsda$permut.p<<-p;
    analSet$plsda$permut.inf<<-F;
    analSet$plsda$permut.type<<- perm.type;
    analSet$plsda$permut<<-perm.vec;
    return(p);
}

# BHan: added bgcolor parameter for B/W color
PlotPLS.Imp<-function(imgName, format="png", dpi=72, width=NA, type, feat.nm, feat.num, color.BW=FALSE){
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 8;
    }else if(width == 0){
        w <- 7;
        imgSet$pls.imp<<-imgName;
    }else{
        w <- width;
    }
    h <- w;
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    if(type=="vip"){
        analSet$plsda$imp.type<<-"vip";
        vips<-analSet$plsda$vip.mat[,feat.nm];
        PlotImpVar(vips, "VIP scores", feat.num, color.BW);
    }else{
        analSet$plsda$imp.type<<-"coef";
        data<-analSet$plsda$coef.mat[,feat.nm];
        PlotImpVar(data, "Coefficients", feat.num, color.BW);
    }
    dev.off();
}

# BHan: added bgcolor parameter for B/W color
PlotImpVar <- function(imp.vec, xlbl, feat.num=15, color.BW=FALSE){
    cls.len <- length(levels(dataSet$cls));
    if(cls.len == 2){
        rt.mrg <- 5;
    }else if(cls.len == 3){
        rt.mrg <- 6;
    }else if(cls.len == 4){
        rt.mrg <- 7;
    }else if(cls.len == 5){
        rt.mrg <- 8;
    }else if(cls.len == 6){
        rt.mrg <- 9;
    }else{
        rt.mrg <- 11;
    }
    op <- par(mar=c(5,7,3,rt.mrg)); # set right side margin with the number of class

    if(feat.num <= 0){
        feat.num = 15;
    }

    if(feat.num > length(imp.vec)){
        feat.num <- length(imp.vec);
    }

    # first get the top subset
    imp.vec <- rev(sort(imp.vec))[1:feat.num];

    # reverser the order for display
    imp.vec <- sort(imp.vec);
    
    # as data should already be normalized, use mean/median should be the same
    # mns is a list contains means of all vars at each level
    # conver the list into a matrix with each row contains var averages across different lvls
    mns <- by(dataSet$norm[, names(imp.vec)], dataSet$cls, 
                    function(x){ # inner function note, by send a subset of dataframe
                        apply(x, 2, mean, trim=0.1)
                    });
    mns <- t(matrix(unlist(mns), ncol=feat.num, byrow=TRUE));

    # vip.nms <-substr(names(imp.vec), 1, 12);
    vip.nms <-substr(names(imp.vec), 1, 14);
    names(imp.vec) <- NULL;

    # modified for B/W color
    dotcolor <- ifelse(color.BW, "darkgrey", "blue");
    dotchart(imp.vec, bg=dotcolor, xlab= xlbl, cex=1.3);
    
    mtext(side=2, at=1:feat.num, vip.nms, las=2, line=1)

    axis.lims <- par("usr"); # x1, x2, y1 ,y2

    # get character width
    shift <- 2*par("cxy")[1];
    lgd.x <- axis.lims[2] + shift;

    x <- rep(lgd.x, feat.num);
    y <- 1:feat.num;
    par(xpd=T);
    suppressMessages(require(RColorBrewer));

    nc <- ncol(mns);

    # modified for B/W color
    colorpalette <- ifelse(color.BW, "Greys", "RdYlGn");
    col <- colorRampPalette(brewer.pal(10, colorpalette))(nc); # set colors for each class
    if(color.BW) col <- rev(col);

    # calculate background
    bg <- matrix("", nrow(mns), nc);
    for (m in 1:nrow(mns)){
        bg[m,] <- (col[nc:1])[rank(mns[m,])];
    }

    cls.lbl <- levels(dataSet$cls);

    for (n in 1:ncol(mns)){
        points(x,y, bty="n", pch=22, bg=bg[,n], cex=3);
        # now add label
        text(x[1], axis.lims[4], cls.lbl[n], srt=45, adj=c(0.2,0.5));
        # shift x, note, this is good for current size
        x <- x + shift/1.25;
    }

    # now add color key, padding with more intermediate colors for contiuous band
    col <- colorRampPalette(brewer.pal(25, colorpalette))(50)
    if(color.BW) col <- rev(col);

    nc <- length(col);
    x <- rep(x[1] + shift, nc);

    shifty <- (axis.lims[4]-axis.lims[3])/3;
    starty <- axis.lims[3] + shifty;
    endy <- axis.lims[3] + 2*shifty;
    y <- seq(from = starty, to = endy, length = nc);

    points(x,y, bty="n", pch=15, col=rev(col), cex=2);

    text(x[1], endy+shifty/8, "High");
    text(x[1], starty-shifty/8, "Low");

    par(op);
}

# Plot plsda classification performance using different components
PlotPLS.Classification<-function(imgName, format="png", dpi=72, width=NA){
    res<-analSet$plsda$fit.info;
    colnames(res) <- 1:ncol(res);
    best.num <- analSet$plsda$best.num;
    choice <- analSet$plsda$choice;
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 7;
    }else if(width == 0){
        w <- 7;
        imgSet$pls.class<<-imgName;
    }else{
        w <- width; 
    }
    h <- w*5/7;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mar=c(5,5,2,7)); # put legend on the right outside
    barplot(res, beside = TRUE, col = c("lightblue", "mistyrose","lightcyan"), ylim= c(0,1.05), xlab="Number of components", ylab="Performance");

    if(choice == "Q2"){
        text((best.num-1)*3 + best.num + 2.5, res[3,best.num]+ 0.02, labels = "*", cex=2.5, col="red");
    }else if(choice == "R2"){
        text((best.num-1)*3 + best.num + 1.5, res[2,best.num]+ 0.02, labels = "*", cex=2.5, col="red");
    }else{
        text((best.num-1)*3 + best.num + 0.5, res[1,best.num]+ 0.02, labels = "*", cex=2.5, col="red");
    }

    # calculate the maximum y position, each bar is 1, place one space between the group
    xpos <- ncol(res)*3 + ncol(res) + 1;
    legend(xpos, 1.0, rownames(res), fill = c("lightblue", "mistyrose","lightcyan"), xpd=T);
    dev.off();
}


# Plot plsda classification performance using different components
PlotPLS.Permutation<-function(imgName, format="png", dpi=72, width=NA){
    bw.vec<-analSet$plsda$permut;
    len<-length(bw.vec);

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 8;
    }else if(width == 0){
        w <- 7;
        imgSet$pls.permut<<-imgName;
    }else{
        w <- width; 
    }
    h <- w*6/8;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mar=c(5,5,2,4));
    hst <- hist(bw.vec, breaks = "FD", freq=T,
            ylab="Frequency", xlab= 'Permutation test statistics', col="lightblue", main="");

    # add the indicator using original label
    h <- max(hst$counts)
    arrows(bw.vec[1], h/5, bw.vec[1], 0, col="red", lwd=2);
    text(bw.vec[1], h/3.5, paste('Observed \n statistic \n', analSet$plsda$permut.p), xpd=T);
    dev.off();
}

# get which number of components give best performance
GetPLSBestTune<-function(){
    if(is.null(analSet$plsda$best.num)){
        return (0);
    }
    analSet$plsda$best.num;
}

# obtain VIP score
GetPLSSigMat<-function(type){
    if(type == "vip"){
        return (CleanNumber(signif(as.matrix(analSet$plsda$vip.mat),5)));
    }else if(type == "coef"){
        return (CleanNumber(signif(as.matrix(analSet$plsda$coef.mat),5)));
    }else{
        return (CleanNumber(signif(as.matrix(analSet$plsr$imp.loads),5)));
    }
}

GetPLSSigRowNames<-function(type){
    if(type == "vip"){
        return (rownames(analSet$plsda$vip.mat));
    }else if(type == "coef"){
        return (rownames(analSet$plsda$coef.mat));
    }else{
        return (rownames(analSet$plsr$imp.loads))
    }
}

GetPLSSigColNames<-function(type){
    if(type == "vip"){
        return (colnames(analSet$plsda$vip.mat));
    }else if(type == "coef"){
        return (colnames(analSet$plsda$coef.mat));
    }else{
        return (colnames(analSet$plsr$imp.loads));
    }
}

GetPLS_CVRowNames <- function(){
    rownames(analSet$plsda$fit.info);
}

GetPLS_CVColNames <- function(){
    colnames(analSet$plsda$fit.info);
}

GetPLS_CVMat<-function(){
    return(signif(analSet$plsda$fit.info, 5));
}

GetMaxPLSPairComp<-function(){
    return (min(dim(dataSet$norm)[1]-1, dim(dataSet$norm)[2]));
}

GetMaxPLSCVComp<-function(){
    return (min(dim(dataSet$norm)[1]-2, dim(dataSet$norm)[2]));
}

GetDefaultPLSPairComp<-function(){
    return (min(5, dim(dataSet$norm)[1]-1, dim(dataSet$norm)[2]));
}

GetDefaultPLSCVComp<-function(){
    return (min(5, dim(dataSet$norm)[1]-2, dim(dataSet$norm)[2], dataSet$min.grp.size));
}

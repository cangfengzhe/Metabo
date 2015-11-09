##################################################
## R script for MetaboAnalyst
## Description: create interactive PCA visualiazation
## based on LiveGraphics3D
##
## Author: Jeff Xia, jeff.xia@mcgill.ca
## Date: Jan 16, 2015
## McGill University, Canada
###################################################

# perform PCA analysis, prepare file for interactive liveGraphics3D
iPCA.Anal<-function(fileNm){
    pca<-prcomp(dataSet$norm, center=T, scale=F);
    imp.pca<-summary(pca)$importance;

    pca3d <- list();
    pca3d$score$axis <- paste("PC", 1:3, " (", 100*round(imp.pca[2,][1:3], 3), "%)", sep="");
    coords <- data.frame(t(signif(pca$x[,1:3], 5)));
    colnames(coords) <- NULL; 
    pca3d$score$xyz <- coords;
    pca3d$score$name <- rownames(dataSet$norm);
    facA <- as.character(dataSet$facA);
    if(all.numeric(facA)){
        facA <- paste("Group", facA);
    }
    pca3d$score$facA <- facA;
    facB <- as.character(dataSet$facB);
    if(all.numeric(facB)){
        facB <- paste("Group", facB);
    }
    pca3d$score$facB <- facB;

    pca3d$loadings$axis <- paste("Loadings", 1:3);
    coords <- data.frame(t(signif(pca$rotation[,1:3], 5)));
    colnames(coords) <- NULL; 
    pca3d$loadings$xyz <- coords;
    pca3d$loadings$name <- colnames(dataSet$norm);

    # now set color for each group
    cols <- unique(GetColorSchema());
    rgbcols <- col2rgb(cols);
    cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")});
    pca3d$score$colors <- cols;

    require(RJSONIO);
    json.obj <- toJSON(pca3d, .na='null');
    sink(fileNm);
    cat(json.obj);
    sink();
}

# this function should only be called from local RConnection
Plot.AllSamples<-function(){

    print("plotting sample images ... ");

    # now calculate the feature summary for each sample, this is for plotSample
    # calculate SD
    sds <- apply(data, 2, sd);
    # now calculate the p value for each data points (across col)
    p.mat <- matrix(0, nrow = nrow(data), ncol=ncol(data));
    for(i in 1:ncol(data)){
        # since mean zero, calculate the p
         p.mat[,i] <- dnorm(data[, i], 0, sds[i]);
    }

    # get the rank of the p values across row
    rank.mat <- t(apply(p.mat, 1, rank, ties.method="random"));

    smplNms <- rownames(data);
    # now plot summary all samples
    # we can afford this b/c sample number is usually not large
    smplImgs <- paste(smplNms, ".png", sep="");

    # use abbreviated name
    colnames(data) <- substr(colnames(data), 1, 10);
    inx <- 1:ncol(data);
    title.txt <- "Comparison of all variables\n";

    if(ncol(data) > 20){
        inx <- 1:20;
        title.txt <- "Top 20 different variables\n";
    }

    for (i in 1:nrow(data)){
        # reorder data var order based on the rank for this sample
        ndat <- data[,order(rank.mat[i, ])];
        ndat <- ndat[, inx];
        Cairo(file = smplImgs[i], width=280, height=480, bg="transparent", type="png");

        # default boxplot is bottom to top (horizontal =T), we need to
        # reverse variable order => from top to bottom
        ndat <- ndat[, ncol(ndat):1];
        op<-par(mar=c(2,6,4,2));
        bp <- boxplot(ndat, names=colnames(ndat), col="bisque", horizontal=T, xaxt="n", las=2);
        title(paste(title.txt, "in sample: ", smplNms[i], sep=""));
        points(ndat[i, ], seq(bp$n), col = "red", pch = 18, cex=1.5);
        axis(1, las=1);
        dev.off();
    }
}

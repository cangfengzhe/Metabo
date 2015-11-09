sapply(dir(), function(x){
  if (substr(x, nchar(x), nchar(x)) == 'R'){
    print(x)
    try({
      source(x)
    })
    
  }
})
InitDataObjects("conc", "stat", FALSE)
Read.TextData("Neg_export_II.csv", "colu", "disc");
SanityCheckData();
ReplaceMin();
IsSmallSmplSize();
FilterVariable("iqr")
Normalization("ProbNorm", "LogNorm", "AutoNorm", "1", "T", ratio=FALSE, ratioNum=20)
PLSR.Anal()
PlotPLS2DScore("pls_score2d_0_", "png", 72, width=NA, 1,2,0.95,1,0)
PlotPLSLoading("pls_loading_0_", "png", 72, width=NA, 1, 2,"scatter", 1);
GetMinGroupSize();
PLSDA.CV("L",4, "Q2")
PlotPLS.Imp("pls_imp_0_", "png", 72, width=NA, "vip", "Comp. 1", 15,FALSE)
GetMinGroupSize();
GetMinGroupSize();
PLSDA.CV("L",3, "Q2")
PlotPCAPairSummary("pca_pair_0_", "png", 72, width=NA, 5)
PlotPCA2DScore("pca_score2d_0_", "png", 72, width=NA, 1,2,0.95,1,0)

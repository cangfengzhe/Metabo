#see updated code base and some examples in the function "test" 
# https://github.com/dgrapov/devium/blob/master/R/Devium%20PLS%20%20and%20OPLS.r

#Orthogonal Signal Correction for PLS models (OPLS)
#adapted from an example in the book <a href="http://www.springer.com/life+sciences/systems+biology+an+bioinfomatics/book/978-3-642-17840-5">"Chemometrics with R by Ron Wehrens"</a>

#this code requires the following packages:
need.packages<-c("pls", # to generate PLS models
                 "ggplot2" ) # to plot results


#I will use sample data set (glycans in humans) from a google spreadsheet which can be found here
#https://docs.google.com/spreadsheet/ccc?key=0Ap1AEMfo-fh9dHdxRkQtX08xQWdRNVB4VG5HZU9LLXc&usp=sharing
#as an example

#two main functions will be used, which are defined below
#1) OSC.correction 	--> to calculate OSC corrected models
#2) plot.OSC.results 	--> to plot OSC corrected results


#function to carry out orthogonal signal correction-PLS (~OPLS)
OSC.correction<-function(pls.y,pls.data,comp=5,OSC.comp=4,validation = "LOO",...){ # later open to  all plsr options, ...
  
  require(pls)
  
  #initialize
  OSC.results<-list()
  OSC.results$data[[1]]<-pls.data
  OSC.results$y[[1]]<-pls.y # also add a place to store plsr options for record keeping
  #  add a place to store plsr options for record keeping
  
  #need to iteratively fit models for each OSC
  for(i in 1:(OSC.comp+1)){ 
    print(i)
    data<-OSC.results$data[[i]]
    tmp.model<-plsr(OSC.results$y[[1]]~., data = data, ncomp = comp, validation = validation)#,...
    ww<-tmp.model$loading.weights[,1]
    pp<-tmp.model$loadings[,1]
    w.ortho<- pp - crossprod(ww,pp)/crossprod(ww)*ww
    t.ortho<- as.matrix(pls.data) %*% w.ortho
    p.ortho<- crossprod(as.matrix(data),t.ortho)/ c(crossprod(t.ortho))
    Xcorr<- data - tcrossprod(t.ortho,p.ortho)
    
    #store results
    OSC.results$RMSEP[[i]]<-matrix(RMSEP(tmp.model)$val,ncol=2,byrow=TRUE)
    OSC.results$scores[[i]]<-tmp.model$scores
    OSC.results$loadings[[i]]<-tmp.model$loadings
    OSC.results$loading.weights[[i]]<-tmp.model$loading.weights
    OSC.results$total.LVs[[i]]<-comp
    OSC.results$OSC.LVs[[i]]<-i-1 # account for first model not having any OSC LVs
    #initialize data for next round
    OSC.results$data[[i+1]]<-as.data.frame(Xcorr)
  }
  
  return(OSC.results)	
}

#function to plot OSC.results
plot.OSC.results<-function(obj,plot="RMSEP",groups=NULL){
  require(ggplot2)
  #plot = one of: c("RMSEP","scores","loadings","delta.weights")
  #groups is a factor to show group visuyalization in scores plot
  switch(plot,
         RMSEP 			=  .local<-function(obj){
           #bind info and RMSEP
           comps<-obj$total.LVs
           ocomps<-obj$OSC.LVs
           plot.obj<-obj$RMSEP
           bound<-do.call("rbind",lapply(1:length(comps),function(i)
           {
             out<-as.data.frame(cbind(plot.obj[[i]][,1],c(0:comps[i]),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
             colnames(out)<-c("RMSEP","component","model")
             out
           }))
           bound[,1:2]<-as.numeric(as.matrix(bound[,1:2]))	
           
           #custom theme
           .theme<- theme(
             axis.line = element_line(colour = 'gray', size = .75), 
             panel.background = element_blank(),  
             plot.background = element_blank()
           )
           #plot				 
           p<-ggplot(data=bound, aes(x=component, y=RMSEP, group=model,color=model)) + geom_line(size=1,alpha=.5) + geom_point(size=2)+.theme
           print(p)
         },
         scores 			=	.local<-function(obj){
           comps<-obj$total.LVs
           ocomps<-obj$OSC.LVs
           plot.obj<-obj$scores
           bound<-do.call("rbind",lapply(1:length(comps),function(i)
           {
             out<-as.data.frame(cbind(plot.obj[[i]][,1:2],unlist(groups),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
             colnames(out)<-c("Comp1","Comp2","groups","model")
             out
           }))
           bound[,1:2]<-as.numeric(as.matrix(bound[,1:2]))	
           
           #calculate convex hull for polygons for each group
           data.obj <- split(bound, bound$model)
           tmp.obj <- lapply(1:length(data.obj), function(i){
             obj<-data.obj[[i]]
             s2<-split(obj,obj[,3])
             do.call(rbind,lapply(1:length(s2),function(j){
               tmp<-s2[[j]]
               tmp[chull(tmp[,1:2]),] 
             }))
           })
           chull.boundaries <- do.call("rbind", tmp.obj)
           
           #custom theme
           .theme<- theme(
             axis.line = element_line(colour = 'gray', size = .75), 
             panel.background = element_blank(), 
             panel.border = element_rect(colour="gray",fill=NA),
             plot.background = element_blank()
           )
           
           #make plot
           p<-ggplot(data=bound, aes(x=Comp1, y=Comp2, group=groups,color=groups)) + #geom_density2d(aes(group=groups))+
             geom_hline(aes(yintercept=0),color="gray60",linetype="dashed")+geom_vline(aes(xintercept=0),color=I("gray60"),linetype=2)+facet_grid(. ~ model)
           p<-p+geom_polygon(data=chull.boundaries,aes(x=Comp1,y=Comp2,fill=groups),alpha=.5) +geom_point(size=2)+.theme
           print(p)
         },
         loadings 		= 	.local<-function(obj){ # will only plot first component for each model
           comps<-obj$total.LVs
           ocomps<-obj$OSC.LVs
           plot.obj<-obj$loadings
           bound<-do.call("rbind",lapply(1:length(comps),function(i)
           {
             out<-as.data.frame(cbind(plot.obj[[i]][,1:2],rownames(plot.obj[[i]]),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
             colnames(out)<-c("Comp1","Comp2","variable","model")
             out
           }))
           bound[,1:2]<-as.numeric(as.matrix(bound[,1:2]))	
           
           #custom theme
           .theme<- theme(
             axis.line = element_line(colour = 'gray', size = .75), 
             panel.background = element_blank(), 
             legend.position = "none",
             plot.background = element_blank()
           )
           
           #make plot
           p<-ggplot(data=bound, aes(x=variable,y=Comp1, fill=variable)) + geom_bar(stat = "identity") + coord_flip() + #geom_density2d(aes(group=groups))+
             facet_grid(. ~ model) +.theme
           print(p)
         },
         delta.weights 	= 	.local<-function(obj){ # will only plot first component for each model
           comps<-obj$total.LVs
           ocomps<-obj$OSC.LVs
           plot.obj<-obj$loading.weights
           bound<-do.call("rbind",lapply(2:(length(ocomps)),function(i)
           {
             out<-as.data.frame(cbind(plot.obj[[1]][,1]-plot.obj[[i]][,1],names(plot.obj[[i]][,1]),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
             colnames(out)<-c("delta_weight","variable","model")
             out
           }))
           bound[,1]<-as.numeric(as.matrix(bound[,1]))	
           
           #theme
           .theme<- theme(
             axis.line = element_line(colour = 'gray', size = .75), 
             panel.background = element_blank(), 
             legend.position = "none",
             plot.background = element_blank()
           )
           #make plot
           p<-ggplot(data=bound, aes(x=variable,y=delta_weight, fill=variable)) + geom_bar(stat = "identity") + coord_flip() + #geom_density2d(aes(group=groups))+
             facet_grid(. ~ model) +.theme
           print(p)
         }
  )				
  .local(obj)
}


#now import data and Y using which ever way you preffer
# I use devium (https://github.com/dgrapov/devium)
# now format and scale data for modeling 

imported.data<-pls.data# copied from clipboard 
pls.data<-data.frame(matrix(as.numeric(as.matrix(imported.data)),nrow(imported.data),ncol(imported.data))) # make sure its numeric
dimnames(pls.data)<-dimnames(imported.data) 

#Y (object to predict)
Y <- pls.y
pls.y<-as.numeric(as.character(unlist(Y))) # Disease_Status

#auto scaling of data 
scaled.data<-as.data.frame(scale(pls.data,scale=apply(pls.data,2,sd),center=colMeans(pls.data)))

#compare a model with 0 or 1 OSC components and 10 total components 
# use SIMPLS algorithm for calculating components and leave-one-out internal cross-validation for parameter estimation
mods<-OSC.correction(pls.y,pls.data,comp=10,OSC.comp=1,validation = "LOO",methods="oscorespls")

#visualize predictive error for trainning data (RMSEP)
plot.OSC.results(mods,plot="RMSEP",groups=groups)

# based on this we can see that 2 copmponents minimize the error for both 0 or 1 OSC models
# recalculate OSC model using 1 OSC and 2 total components ( 1 non - OSC)
mods<-OSC.correction(pls.y,pls.data,comp=2,OSC.comp=1,validation = "LOO",methods="oscorespls")

#visualize scores comparing 0 and 1 OSC models
#I set groups = pls.y to display polygons around each class
plot.OSC.results(mods,plot="delta.weights",groups=pls.y)

#now compare model variable loadings between 0 and 1 OSC models
plot.OSC.results(mods,plot="loadings")

#finally identify changes (delta) in variable model weights (beta weights) between 0 and 1 OSC models
plot.OSC.results(mods,plot="delta.weights")



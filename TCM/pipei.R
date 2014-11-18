#大于10000的200多个mol与all mesh pipei
#Signs and Symptoms


errorRes<-matrix(NA,10000,2)
warnRes<-matrix(NA,10000,2)
result<-matrix(NA,nrow(molPmidMore)*nrow(meshPmidCount),4)
n<-0;
m<-0;
kk<-0

for (ii in 52: nrow(molPmidMore)){
  for (jj in 1: nrow(meshPmidCount)){
    molName<-molPmidMore[ii,1];
    meshName<-meshPmidCount[jj,1];
    print(paste('jj',jj))
    
    tryCatch({
      
      prestr=paste(c('("',molName,'" [MeSH Terms] OR "',molName,'" [Title/Abstract]) AND ','("',meshName,'" [MeSH Terms] OR "',meshName,'"[Title/Abstract])'),collapse='')
      pmid<-downloadPmid(prestr);
      if(is.matrix(pmid)){
        kk<-kk+1;
        pmid0<-paste(pmid,collapse = ',');
        result[kk,1]<-molName;
        result[kk,2]<-meshName;
        result[kk,4]<-pmid0;
        result[kk,3]<-nrow(pmid)
       
        print(paste('kk',kk))
      }
    }  
    ,error=function(e){
      n<<-n+1;
      errorRes[n,1]<<-ii;
      errorRes[n,2]<<-jj;
      print(e);
    },warning=function(w){
      m<<-m+1;
      warnRes[m,1]<<-ii;
      warnRes[m,2]<<-jj;
      print(w)
    })
    
  }
  print(paste('ii',ii))
}

#大于10000的mesh与所有的分子匹配
length(meshPmidCount[meshPmidCount[,2]>10000,1])
meshCount<-as.data.frame(meshPmidCount)
meshCount[,2]<-as.numeric(meshCount[,2])
meshCount[,1]<-as.character(meshCount[,1])
meshMore<-meshCount[meshCount[,2]>=10000,]
meshMore[,1]<-as.character(meshMore[,1])
molCount<-result_1


errorRes<-matrix(NA,10000,2)
warnRes<-matrix(NA,10000,2)
result<-matrix(NA,nrow(molCount),2)
n<-0;
m<-0;
kk<-0

for (ii in 169: nrow(molCount)){
  
    molName<-molCount[ii,1];
    meshName<-'Signs and Symptoms'
    
    
    tryCatch({
      
      prestr=paste(c('("',molName,'" [MeSH Terms] OR "',molName,'" [Title/Abstract]) AND ','("',meshName,'" [MeSH Terms] OR "',meshName,'"[Title/Abstract])'),collapse='')
      pmid<-downloadPmid(prestr);
      if(is.matrix(pmid)){
        #kk<-kk+1;
        #pmid0<-paste(pmid,collapse = ',');
        result[ii,1]<-molName;
        #result[kk,2]<-meshName;
        #result[kk,4]<-pmid0;
        result[ii,2]<-nrow(pmid)      
        
      }
      if(is.matrix(pmid)==F){
       
        result[ii,1]<-molName;
        #result[kk,2]<-meshName;
        #result[kk,4]<-pmid0;
        result[ii,2]<-NA;      
      }
    }  
    ,error=function(e){
      n<<-n+1;
      errorRes[n,1]<<-ii;
      errorRes[n,2]<<-jj;
      print(e);
    },warning=function(w){
      m<<-m+1;
      warnRes[m,1]<<-ii;
      warnRes[m,2]<<-jj;
      print(w)
    })
    
  
  print(paste('ii',ii))
}




meshMore<-meshMore[2:nrow(meshMore),]

#分子 先与Signs and Symptoms匹配，有文献 的再进行下一步的匹配
errorRes<-matrix(NA,10000,2)
warnRes<-matrix(NA,10000,2)
result<-matrix(NA,nrow(molMore)*nrow(meshMore),4)
n<-0;
m<-0;
kk<-0

for (ii in 1: nrow(molMore)){
  for (jj in 1: nrow(meshMore)){
    molName<-molMore[ii,1];
    meshName<-meshMore[jj,1];
    print(paste('jj',jj))
    
    tryCatch({
      
      prestr=paste(c('("',molName,'" [MeSH Terms] OR "',molName,'" [Title/Abstract]) AND ','("',meshName,'" [MeSH Terms] OR "',meshName,'"[Title/Abstract])'),collapse='')
      pmid<-downloadPmid(prestr);
      if(is.matrix(pmid)){
        kk<-kk+1;
        pmid0<-paste(pmid,collapse = ',');
        result[kk,1]<-molName;
        result[kk,2]<-meshName;
        result[kk,4]<-pmid0;
        result[kk,3]<-nrow(pmid)
        
        print(paste('kk',kk))
      }
    }  
    ,error=function(e){
      n<<-n+1;
      errorRes[n,1]<<-ii;
      errorRes[n,2]<<-jj;
      print(e);
    },warning=function(w){
      m<<-m+1;
      warnRes[m,1]<<-ii;
      warnRes[m,2]<<-jj;
      print(w)
    })
    
  }
  print(paste('ii',ii))
}


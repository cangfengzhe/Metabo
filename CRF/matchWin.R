#匹配基因和蛋白质
pmid<-''
gene<-'';
protein<-'';
result<-data.frame(pmid,gene,protein)
result0<-result;
rowNum<-nrow(geneDataTmp)
rowNum<-2200
ii<-1
while(ii <= rowNum){
  
  str<-geneDataTmp[ii,1]
  if(substr(str,1,3)=='ZZZ'){
    print(ii)
    pmid<-substr(str,4,nchar(str));  #nchar 计算字符个数
    for( jj in c((ii+1):rowNum)){
      # protein
      if(substr(geneDataTmp[jj,3],1,5)=='B-pro'){
        protein<-geneDataTmp[jj,1];
        for( kk in (jj+1):rowNum){
          if(substr(geneDataTmp[kk,3],1,5)=='I-pro'){
          protein<-paste(protein,geneDataTmp[jj,1])
          }
          else {
            
            break;
          }
        }      
      } 
      
      #gene 
      if(substr(geneDataTmp[jj,3],1,5)=='B-DNA'){
        gene<-geneDataTmp[jj,1];
        for(kk in (jj+1):rowNum){
          if(substr(geneDataTmp[kk,3],1,5)=='I-DNA'){
            gene<-paste(gene,geneDataTmp[jj,1])
          }
          else {
            
            break;
          }
        }
        
      }
      if(substr(geneDataTmp[jj,1],1,3)=='ZZZ'){
        result<-data.frame(pmid,gene,protein)        
        result0<-rbind(result0,result)
        protein<-'';
        gene<-''
        pmid0<-''
        protein0<-'';
        gene0<-'';
        
      }
    }
    
  
  
  }  
  ii<-ii+1;
  
}


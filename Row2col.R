tar<-read.csv('D:\\deskTop\\trashBin\\ZWj\\fenge.csv',stringsAsFactors =F)

n<-0
tar2<-matrix(NA,nrow(tar)*3,2)


for(ii in 1: nrow(tar)){
 
 for(jj  in 2:7){
   
   if (tar[ii,jj]!=''){
     n<-n+1;
     tar2[n,1]=tar[ii,1]
     tar2[n,2]=tar[ii,jj]
   }
  
   
 }
  
}

write.csv(tar2,'D:\\deskTop\\trashBin\\ZWj\\tar2.csv')

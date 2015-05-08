wshape_feature <- function(data, normalize = T, bind = T){

  data <- check_feature(data)
  if(normalize){
   out <-  gsub('[A-Z]+','X',data[,1]) %>%
      gsub('[a-z]+','x', .) %>%
       gsub('\\d+', '0', .)%>%
      gsub('[^a-zA-Z0-9]+', '_', .)
    }else{

      out <-   gsub('[A-Z]','X',data[,1]) %>%
          gsub('[a-z]','x', .) %>%
          gsub('\\d', '0', .)%>%
          gsub('[^a-zA-Z0-9]', '_', .)
    }

  if(bind){
    out <- cbind(data, out)
  }else{
      out <- cbind(data[,1], out)
      }

 out

}




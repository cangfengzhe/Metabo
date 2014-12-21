library(RCurl)
# 设置头文件与提交的表单
header <- c("Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
            "Accept-Encoding"="gzip, deflate",
            "Accept-Language"="zh-CN,zh;q=0.8,en-US;q=0.6,en;q=0.4",
            "Cache-Control"="max-age=0",
            "Connection"="keep-alive",
            "Content-Length"="52",
            "Content-Type"="application/x-www-form-urlencoded",
            "Cookie"="LPTSRVID=1276604206",
            "Host"="192.168.101.253",
            "Origin"="http://192.168.101.253",
            "Referer"="http://192.168.101.253/portal/logon.htm",
            "User-Agent"="Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36",
            "PtUser"="2012015281",
            "PtPwd"="660808",
            "PtButton"="%B5%C7%C2%BC")

# 设置调试信息
d = debugGatherer()
#handle信息，handle会随时更新，RCurl 设置了很多回调函数
handle = getCurlHandle()
temp<- postForm("http://192.168.101.253/portal/logon.cgi",.params=header,
                .opts=list(debugfunction=d$update,verbose = TRUE),curl=handle,style="post")

d$value()[2] %>% cat()  #显示 response header

x
post <-scan("clipboard",what="character",quiet=TRUE,sep="\n")
post

%aa% <- function(x) View(x)




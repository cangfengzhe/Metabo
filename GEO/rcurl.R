library(RCurl)
# 设置头文件与提交的表单
header <- c(Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8", 
    `Accept-Encoding` = "gzip, deflate", `Accept-Language` = "zh-CN,zh;q=0.8,en-US;q=0.6,en;q=0.4", 
    `Cache-Control` = "max-age=0", Connection = "keep-alive", `Content-Length` = "182", 
    `Content-Type` = "application/x-www-form-urlencoded", Cookie = "wordpress_test_cookie=WP+Cookie+check; wfvt_1908536264=5495156d36f3c; _gat=1; _ga=GA1.2.11408004.1419038338", 
    Host = "cos.name", Origin = "http://cos.name", Referer = "http://cos.name/cn/?loggedout=true", 
    `User-Agent` = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36", 
    log = "lipidong", pwd = "lipidong1990", rememberme = "forever", 
    `user-submit` = "", `user-cookie` = "1", redirect_to = "http://cos.name/cn/", 
    `_wpnonce` = "28a75be428", `_wp_http_referer` = "/cn/?loggedout=true")

# 设置调试信息
d = debugGatherer()
# handle信息，handle会随时更新，RCurl 设置了很多回调函数
handle = getCurlHandle()
temp <- postForm("http://cos.name/cn/wp-login.php", .params = header, 
    .opts = list(cookiefile = "", debugfunction = d$update, verbose = TRUE), 
    curl = handle, style = "post")

d$value()[2] %>% cat()  #显示 response header


# 可以使用handle，以登录模式打开所有该网站页面
test <- getURL("http://cos.name/cn/", curl = handle, debugfunction = d$update, 
    verbose = TRUE)
d$value()[2] %>% cat()

writeLines(test, "D:/deskTop/aa.html")


# 采用cookie进行登录

d2 = debugGatherer()
cHandle2 <- getCurlHandle(httpheader = myHttpheader, followlocation = 1, 
    debugfunction = d2$update, verbose = TRUE, cookiefile = "yourcookiefile.txt")


 

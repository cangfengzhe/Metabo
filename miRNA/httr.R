library(httr)
url <- 'http://www.renren.com/303669775'
cook <- set_cookies(anonymid='i7v7iaa5n8m5gj', '_r01_'=1, '__utma'='151146938.11989299.1428905562.1428905562.1428905562.1', '__utmz'='151146938.1428905562.1.1.utmcsr=so.com|utmccn=(organic)|utmcmd=organic|utmctr=postscript%20%E7%BC%96%E7%A8%8B', depovince='SXI', jebecookies='d0498d5e-cf7f-4acd-a0e7-0aae9da75a0a|||||', JSESSIONID='abcmk68JMRgXkdlx73f0u', ick_login='f7ea42ca-5f38-46c7-934e-ff4950973b9c', '_de'='833291F94C2DF9DA99AF2B66E7B8960F8ED172744450A224', p='ba788b8e8c4ec9e9073d6a5739f1b9525',first_login_flag=1, ln_uact='hope-dream@163.com', ln_hurl='http://hdn.xnimg.cn/photos/hdn121/20110210/1740/h_main_KAP4_4a3000001b092f75.jpg', t='3355128749d78db81e79deb067304a0f5', societyguester='3355128749d78db81e79deb067304a0f5', id=303669775,xnsid='fa1fffee', ver=7.0, loginfrom='null', jebe_key='808f8a9e-7e72-4a65-bfe3-9c3e6f7bb892%7C05d1a5aa8ea70ce90b1eee0eb9e74d20%7C1430322686457%7C1%7C1430322685811', wp_fold=0)
aa <- GET(url, cook)
aa %>% class


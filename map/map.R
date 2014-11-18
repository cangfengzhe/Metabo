attach(mtcars)
View(mtcars)
plot(wt,mpg)
abline(lm(mpg~wt)) #加直线
aa<-lm(mpg~wt)
summary(aa)

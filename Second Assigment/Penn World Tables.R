#Ze Zheng
#Sept 17,2020
#Penn World Tables 

install.packages("tidyverse")
install.packages("tibble")
install.packages("pwt9")
library(tidyverse)
library(tibble)
library(pwt9)
data("pwt9.1")
Tog<-subset(pwt9.1,country=="Togo")
Tog<-tibble(Tog)
# Creating the y, n, and dyy variables in the Ghana tibble
#Creating the Plot Series for y
Tog<-mutate(Tog, y=rgdpe/pop)
with(Tog,plot(year,y,type="l",main="Standard of Living (y) vs Year"))
#Creating the Plot Series for n
Tog<-mutate(Tog, n=(pop-lag(pop))/lag(pop))
with(Tog,plot(year,n,type="l",main="Populaton Growth Rate (n) vs Year"))
# Time series plot for dyy
Tog<-mutate(Tog, dyy=(y-lag(y))/lag(y))
with(Tog, plot(year, dyy, type="l",main="Change in the Standard of Living(dyy) vs Year"))
# Making the backward looking 5 year moving average of dyy
# Adding the moving average to the previous plot
Tog<-mutate(Tog, dyy5yma=(dyy+lag(dyy, n=1)+lag(dyy, n=2)+lag(dyy, n=3)+lag(dyy, n=4))/5)
with(Tog, plot(year, dyy5yma, type="l",main="5 year moving average for dyy(dyy5yma) vs Year"))
with(Tog, lines(year, dyy5yma, col="red"))

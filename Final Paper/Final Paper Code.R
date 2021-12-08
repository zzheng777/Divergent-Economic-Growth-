#Ze Zheng
#Final Paper
#Divergent Economic Growth 


rm(list=ls())
library(pwt9)
data("pwt9.1")
library(tidyverse)
library(stargazer)
library(tidyverse)
library(ggplot2)
TO<-subset(pwt9.1, isocode=="TGO") #Subseting Togo information from Pwt9 to TO list

TO <-mutate(TO,y=rgdpe/pop)
TO<-mutate(TO, n=(pop-lag(pop))/lag(pop))
TO<-mutate(TO, g=(rtfpna- lag(rtfpna)) / lag(rtfpna))


rgdpeg<-ggplot(TO, aes(x=year, y=rgdpe))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Real GDP", title = "Togo's GDP")
rgdpeg  

y<-ggplot(TO, aes(x=year, y=y))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "GDP Per Capita", title = "Togo's Per Capita GDP")
y

rkna<-ggplot(TO, aes(x=year, y=rkna))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Capital Stock", title = "Togo's Capital Stock")
rkna

csh_i<-ggplot(TO, aes(x=year, y=csh_i))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Savings Rate", title = "Togo's Savings Rate")
csh_i

rtfpna<-ggplot(TO, aes(x=year, y=rtfpna))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Technology", title = "Togo's Technology Change")
rtfpna

n<-ggplot(TO, aes(x=year, y=n))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Growth Rate of Population", title = "Togo's Growth Rate of Population Growth")
n

g<-ggplot(TO, aes(x=year, y=g))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Growth Rate of Technology", title = "Togo's Growth Rate of Technology")
g

hc<-ggplot(TO, aes(x=year, y=hc))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Human Capital", title = "Togo's Human Capital")
hc

pop<-ggplot(TO, aes(x=year, y=pop))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Population", title = "Togo's Popuation")
pop

#Scatter Plot vs Y
ggplot(TO, aes(x=csh_i, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Savings Rate", y= "Standard of Living", title = "Togo: Comparing Standard of Living with Savings Rate")

ggplot(TO, aes(x=hc, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Human Capital", y= "Standard of Living", title = "Togo: Comparing Standard of Living with Human Capital")

ggplot(TO, aes(x=rtfpna, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Technology", y= "Standard of Living", title = "Togo: Comparing Standard of Living with Technology")

ggplot(TO, aes(x=n, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Population Growth Rate", y= "Standard of Living", title = "Togo: Comparing Standard of Living with Population Growth Rate")

ggplot(TO, aes(x=g, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Growth Rate of Technology", y= "Standard of Living", title = "Togo: Comparing Standard of Living with Growth Rate of Tech")

# Romer Scatterplot 

ggplot(TO, aes(x=n, y=g))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Population Growth Rate", y= "Growth Rate of Technology", title = "Togo: Comparing Population Growth Rate with Growth Rate of Tech")

# Relation between Human Capital and n 
ggplot(TO, aes(x=n, y=hc))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Population Growth Rate", y= "Human Capital", title = "Togo: Comparing n with hc")


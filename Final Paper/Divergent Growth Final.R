#Andy Zheng
#Final Paper 
#Divergent Economic Growth 

rm(list=ls())
setwd("~/Desktop/DEG/Divergent Growth/Final Paper")
library(pwt9)
data("pwt9.1")
library(tidyverse)
library(stargazer)

library(ggplot2)

#Solow Growth Model Graphs

zafzaf.csv<-(read.csv("zafzaf.csv"))
zaf<-zafzaf.csv %>% filter(isocode=="ZAF")

# Time Series Plot

rgdpeg<-ggplot(zaf, aes(x=year, y=rgdpe))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Real GDP", title = "South Africa GDP")
rgdpeg  

y<-ggplot(zaf, aes(x=year, y=y))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "GDP Per Capita", title = "South Africa Per Capita GDP")
y

rkna<-ggplot(zaf, aes(x=year, y=rkna))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Capital Stock", title = "South Africa Capital Stock")
rkna

csh_i<-ggplot(zaf, aes(x=year, y=csh_i))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Savings Rate", title = "South Africa Savings Rate")
csh_i

rtfpna<-ggplot(zaf, aes(x=year, y=rtfpna))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Technology", title = "South Africa Technology Change")
rtfpna

n<-ggplot(zaf, aes(x=year, y=n))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Growth Rate of Population", title = "South Africa Growth Rate of Population Growth")
n

g<-ggplot(zaf, aes(x=year, y=g))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Growth Rate of Technology", title = "South Africa Growth Rate of Technology")
g

hc<-ggplot(zaf, aes(x=year, y=hc))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Human Capital", title = "South Africa Human Capital")
hc

pop<-ggplot(zaf, aes(x=year, y=pop))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Population", title = "South Africa Popuation")
pop

# Scatterplot vs Y 

ggplot(zaf, aes(x=rkna, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Capital Stock", y= "Standaed of Living", title = "South Africa: Comparing Standard of Living with Capital Stock")

ggplot(zaf, aes(x=csh_i, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Savings Rate", y= "Standard of Living", title = "South Africa: Comparing Standard of Living with Savings Rate")

ggplot(zaf, aes(x=rtfpna, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Technology", y= "Standard of Living", title = "South Africa: Comparing Standard of Living with Technology")

ggplot(zaf, aes(x=hc, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Human Capital", y= "Standard of Living", title = "South Africa: Comparing Standard of Living with Human Capital")

ggplot(zaf, aes(x=n, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Population Growth Rate", y= "Standard of Living", title = "South Africa: Comparing Standard of Living with Population Growth Rate")

ggplot(zaf, aes(x=g, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Growth Rate of Technology", y= "Standard of Living", title = "South Africa: Comparing Standard of Living with Growth Rate of Tech")

# Romer Scatterplot 

ggplot(zaf, aes(x=n, y=g))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Population Growth Rate", y= "Growth Rate of Technology", title = "South Africa: Comparing Population Growth Rate with Growth Rate of Tech")

# Relation between Human Capital and n 
ggplot(zaf, aes(x=n, y=hc))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Population Growth Rate", y= "Human Capital", title = "South Africa: Comparing n with hc")



# Incorporating Institutions

owdpwt.csv<-(read.csv("owdpwt.csv"))

Trade<-ggplot(owdpwt.csv, aes(x=year, y=Trade))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Trade", title = "South Africa Trade")
Trade

FDI<-ggplot(owdpwt.csv, aes(x=year, y=FDI))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "FDI", title = "South Africa Foreign Direct Investment")
FDI

iiagpwt.csv<-(read.csv("iiagpwt.csv"))

ppr<-ggplot(iiagpwt.csv, aes(x=year, y=ppr))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Private Property Rights", title = "South Africa Private Property Rights")
ppr

acm<-ggplot(iiagpwt.csv, aes(x=year, y=acm))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Anti-Corruption Mechanism", title = "South Africa Anti-Corruption Mechanism")
acm

ps<-ggplot(iiagpwt.csv, aes(x=year, y=ps))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Personal Safety", title = "South Africa Personal Safety")
ps

wre<-ggplot(iiagpwt.csv, aes(x=year, y=wre))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Womens Political Representation", title = "South Africa Womens Political Representation")
wre


edu<-ggplot(iiagpwt.csv, aes(x=year, y=edu))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Education", title = "South Africa - Education")
edu

#Institution vs Y 


ggplot(owdpwt.csv, aes(x=y, y=Trade))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Standard of Living", y= "Trade", title = "South Africa: Comparing y with Trade")

ggplot(owdpwt.csv, aes(x=y, y=FDI))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Standard of Living", y= "FDIe", title = "South Africa: Comparing y with FDI")

ggplot(iiagpwt.csv, aes(x=y, y=ppr))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Standard of Living", y= "Private Property Rights", title = "South Africa: Comparing y with Private Property Rights")

ggplot(iiagpwt.csv, aes(x=y, y=acm))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Standard of Living", y= "Anti-Corruption Mechanism", title = "South Africa: Comparing y with Anti-Corruption Mechanism")

ggplot(iiagpwt.csv, aes(x=y, y=ps))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Standard of Living", y= "Personal Safety", title = "South Africa: Comparing y with Personal Safety")

ggplot(iiagpwt.csv, aes(x=y, y=wre))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Standard of Living", y= "Womens Political Representation", title = "South Africa: Comparing y with Womens Political Representation")

ggplot(iiagpwt.csv, aes(x=y, y=edu))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Standard of Living", y= "Education", title = "South Africa: Comparing y with Education")


library(corrplot)
iiagcor<-subset(iiagpwt.csv, select = c(y, Trade, FDI, ppr, acm, ps, edu, inf))
cor1<-cor(iiagcor)
corrplot(cor1)

pwtcor<-subset(owdpwt.csv, selec=c(y,n,g,csh_i, rtfpna, hc, rkna, Trade, FDI))
cor2<-cor(pwtcor)
corrplot(cor2)

zafcor<-subset(zaf, select=c(y,n,g,csh_i,rtfpna,hc,rkna))
cor3<-cor(zafcor)
corrplot(cor3)

og1.csv<-(read.csv("og1.csv"))
og1cor<-subset(og1.csv, select=c(og, srl, phr, seo, hd, y))
cor4<-cor(og1cor)
corrplot(cor4)

og<-ggplot(og1.csv, aes(x=year, y=og))+geom_line(color="hotpink3")+geom_point(color="hotpink3")+labs(x = "Year", y= "Overall Governance", title = "South Africa Overall Governance")
og
ggplot(og1.csv, aes(x=og, y=y))+geom_point(color="hotpink3")+geom_smooth(method = "lm", se = FALSE)+labs(x = "Overall Governance", y= "Standard of Living", title = "South Africa: Comparing y with Overall Governance")

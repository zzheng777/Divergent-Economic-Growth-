#How working affects College Students GPA.
#Ze Zheng

install.packages(ggplot2)
library(ggplot2)

EconStudy2.data<-read_csv(file="EconStudy2.csv")



p1<- ggplot()+
  geom_point(aes(y=Worked, x= GPA), data = EconStudy2.data)
  

p1+ labs (title = "Hours Worked Per Week Compared to Current GPA",
          x = "Current GPA", y= "Hours Worked in a week")



  
  
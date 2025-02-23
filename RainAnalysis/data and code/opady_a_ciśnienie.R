f<-file.choose() #BasicRain
options(warn=-1)
library(dplyr)
library(data.table)
library(microbenchmark)
library(ggplot2) 
OpenMeteo<-read.csv(f)
OpenMeteo$time<-substring(OpenMeteo$time,1,10)


OpenMeteoToYears<-OpenMeteo
OpenMeteoToYears$time<-substring(OpenMeteoToYears$time,1,4)
AveragePressonYear<-OpenMeteoToYears %>%
  group_by(time) %>%
  summarise(AveragePressureOnYear=mean(surface_pressure..hPa.,na.rm=TRUE))
AveragePressonYear

AveragePressonDay<-OpenMeteo %>%
  group_by(time) %>%
  summarise(AveragePressure=(mean(surface_pressure..hPa.,na.rm=TRUE)))
Raining<-OpenMeteo %>%
  group_by(time) %>%
  summarise(SumofRaining=sum(rain..mm.,na.rm=TRUE))
Result<-Raining %>%
  inner_join(AveragePressonDay,by=c("time"="time")) %>%
  filter(SumofRaining>0.3)
ResultWithoutRaining<-Raining %>%
  inner_join(AveragePressonDay,by=c("time"="time"))



#Zmieniam na doubla (ma byc liczba)
PressureYearDouble=AveragePressonYear[1,2,drop=TRUE]
PressureYearDouble


#Liczenie proporcji(Sum deszczu)
LessThenAverage<-Result%>%
  filter(AveragePressure<=PressureYearDouble)
SumLess=sum(LessThenAverage$SumofRaining)
SumLess

MoreThenAverage<-Result%>%
  filter(AveragePressure>PressureYearDouble)
SumMore=sum(MoreThenAverage$SumofRaining)
SumMore

BelowString<-paste("Rainfall total below average =",toString(SumLess),"[mm]")
AboveString<-paste("Rainfall total above average =",toString(SumMore),"[mm]")


#Wykres z flitracji 0.3
ggplot(Result,aes(x=AveragePressure,y=SumofRaining)) +
  geom_bar(stat="identity")+
  geom_col(width=0.5)+
  xlab("Pressure [hPa]")+
  ylab("Rainfall total [mm]")+
  geom_vline(xintercept=PressureYearDouble,colour="red")+
  geom_rect(aes(xmin=1015,xmax=1030,ymin=19,ymax=23),fill="white")+
  annotate("text",x=1020,y=22,label="Year Avg. Pressure", size=2)+
  geom_segment(x=1025,y=22,xend=1028,yend=22,colour="red")+
  annotate("text",x=1023,y=21,label=BelowString, size=2)+
  annotate("text",x=1023,y=20,label=AboveString, size=2)
  
f2<-file.choose()#Wilgotnosc
options(warn=-1)
library(dplyr)
library(data.table)
library(microbenchmark)
library(ggplot2) 
OpenMeteo<-read.csv(f2)
OpenMeteo$time<-substring(OpenMeteo$time,1,10)

#Srednia wilogtnosc rok
OpenMeteoHum<-OpenMeteo%>%
  mutate(time=substring(OpenMeteo$time,1,4))%>%
  group_by(time)%>%
  summarise(AverageHumidityYear=mean(relative_humidity_2m....,na.rm=TRUE))
AverageHumidityYearDouble=OpenMeteoHum[1,2,drop=TRUE]


AverageHumidityTable<-OpenMeteo %>%
  group_by(time)%>%
  summarise(AverageHumidity=mean(relative_humidity_2m....,na.rm=TRUE))

SumOfRainTable<-OpenMeteo %>%
  group_by(time)%>%
  summarise(RainSum=sum(rain..mm.,na.rm=TRUE))


TableResult<-SumOfRainTable %>%
  inner_join(AverageHumidityTable,by=c("time"="time")) %>%
  filter(RainSum>0.3)


#Liczenie proporcji(Sum deszczu)
LessThenAverageHum<-TableResult%>%
  filter(AverageHumidity<=AverageHumidityYearDouble)
SumLessHum=sum(LessThenAverageHum$RainSum)


MoreThenAverageHum<-TableResult%>%
  filter(AverageHumidity>AverageHumidityYearDouble)
SumMoreHum=sum(MoreThenAverageHum$RainSum)


BelowStringHum<-paste("Rainfall total below average =",toString(SumLessHum),"[mm]")
AboveStringHum<-paste("Rainfall total above average =",toString(SumMoreHum),"[mm]")


ggplot(TableResult,aes(x=AverageHumidity,y=RainSum)) +
  geom_bar(stat="identity")+
  geom_col(width=0.5)+
  xlim(50,100)+
  xlab("Average Humidity [%]")+
  ylab("Rainfall total [mm]")+
  geom_vline(xintercept=AverageHumidityYearDouble,colour="red")+
  geom_rect(aes(xmin=50,xmax=65,ymin=41,ymax=49),fill="white")+
  annotate("text",x=54,y=48,label="Year Avg. Humidity ", size=2)+
  geom_segment(x=58,y=48,xend=60,yend=48,colour="red")+
  annotate("text",x=57.5,y=46,label=BelowStringHum, size=2)+
  annotate("text",x=57.5,y=44,label=AboveStringHum, size=2)
 




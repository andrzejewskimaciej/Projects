library(dplyr)
library(data.table)
library(ggplot2) 

f4<-file.choose() #BrazilSugarCaneOfficial
SugarTable<-read.csv(f4)


f3<-file.choose()
RainTableAsia<-read.csv(f3) #IndiaRain




#India
#Pogoda dobra rzecz suma
RainTableAsiaSum<-RainTableAsia%>%
  mutate(time=substring(time,1,4))%>%
  group_by(time)%>%
  summarise(SumRainFall=sum(rain..mm.,na.rm=TRUE))

SugarTableIndia<-SugarTable %>%
  filter(Entity=="India") %>%
mutate(Year=as.character(Year))

IndiaResultSum<-SugarTableIndia %>%
  select(Year,Sugar.cane) %>%
  inner_join(RainTableAsiaSum,by=c("Year"="time"))%>%
  mutate(Sugar.cane=Sugar.cane/100000)


ggplot(IndiaResultSum,aes(x=Year,group=1))+
  geom_line(aes(y=SumRainFall),color="red")+
  geom_line(aes(y=Sugar.cane),color="blue")+
  ylab("")+
  xlab("Years(1961-2022)")+
  scale_x_discrete(labels=NULL)+
  geom_rect(aes(xmin=2,xmax=17,ymin=3000,ymax=3300),fill="white")+
  annotate("text",x=9,y=3200,label="Year Production SugarCane [10^5 tonnes] ", size=2)+
  annotate("text",x=8,y=3100,label="Year Rainfall Sum [mm]", size=2)+
  geom_segment(x=15,y=3200,xend=16,yend=3200,colour="blue")+
  geom_segment(x=12,y=3100,xend=13,yend=3100,colour="red")




#China
f6<-file.choose() #ChinaRain

RainTableChina<-RainTableAsia%>%
  mutate(time=substring(time,1,4))%>%
  group_by(time)%>%
  summarise(SumRainFall=sum(rain..mm.,na.rm=TRUE))


SugarTableChina<-SugarTable %>%
  filter(Entity=="China")%>%
  mutate(Year=as.character(Year))


ChinaResultSum<-SugarTableChina %>%
  select(Year,Sugar.cane) %>%
  inner_join(RainTableChina,by=c("Year"="time"))%>%
  mutate(Sugar.cane=Sugar.cane/100000)



ggplot(ChinaResultSum,aes(x=Year,group=1))+
  geom_line(aes(y=SumRainFall),color="blue")+
  geom_line(aes(y=Sugar.cane),color="red")+
  ylab("")+
  xlab("Years(1961-2022)")+
  scale_x_discrete(labels=NULL)+
  geom_rect(aes(xmin=2,xmax=17,ymin=1100,ymax=1250),fill="white")+
  annotate("text",x=9,y=1200,label="Year Production SugarCane [10^5 tonnes] ", size=2)+
  annotate("text",x=8,y=1150,label="Year Rainfall Sum [mm]", size=2)+
  geom_segment(x=15,y=1200,xend=16,yend=1200,colour="red")+
  geom_segment(x=12,y=1150,xend=13,yend=1150,colour="blue")
  
  



#Brazylia
f5<-file.choose() #BrazilRain
RainBrazilTable<-read.csv(f5)


RainBrazilTable<-RainBrazilTable%>%
  mutate(time=substring(time,1,4))%>%
  group_by(time)%>%
  summarise(SumRainFall=sum(rain..mm.,na.rm=TRUE))


SugarTableBrazil<-SugarTable %>%
  filter(Entity=="Brazil")%>%
  mutate(Year=as.character(Year))

BrazilResultSum<-SugarTableBrazil %>%
  select(Year,Sugar.cane) %>%
  inner_join(RainBrazilTable,by=c("Year"="time"))%>%
  mutate(Sugar.cane=Sugar.cane/1000000)


ggplot(BrazilResultSum,aes(x=Year,group=1))+
  geom_line(aes(y=SumRainFall),color="red")+
  geom_line(aes(y=Sugar.cane),color="blue")+
  ylab("")+
  xlab("Years(1961-2022)")+
  scale_x_discrete(labels=NULL)+
  geom_rect(aes(xmin=2,xmax=17,ymin=1100,ymax=1250),fill="white")+
  annotate("text",x=9,y=1200,label="Year Production SugarCane [10^6 tonnes] ", size=2)+
  annotate("text",x=8,y=1150,label="Year Rainfall Sum [mm]", size=2)+
  geom_segment(x=15,y=1200,xend=16,yend=1200,colour="blue")+
  geom_segment(x=12,y=1150,xend=13,yend=1150,colour="red")



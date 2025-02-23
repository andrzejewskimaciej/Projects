library(dplyr)
library(data.table)
library(ggplot2) 
f6<-file.choose() #electricity-production
f7<-file.choose() #PolandRain
PowerTable<-read.csv(f6)
PolandRainTable<-read.csv(f7)


PolandPowerTable<-PowerTable %>%
  filter(Entity=="Poland") %>%
  select(Entity,Year,Electricity.from.hydro...TWh)%>%
  mutate(Electricity.from.hydro...TWh=Electricity.from.hydro...TWh*500)

PolandRainTableResult<-PolandRainTable%>%
  mutate(time=as.integer(substring(time,1,4)))%>%
  group_by(time)%>%
  summarise(SumofRain=sum(rain..mm.,na.rm=TRUE))


Result<-PolandRainTableResult%>%
  inner_join(PolandPowerTable,by=c("time"="Year"))
Result


ggplot(Result,aes(x=time,group=1))+
  geom_line(aes(y=SumofRain),color="red")+
  geom_line(aes(y=Electricity.from.hydro...TWh),color="blue")+
  xlab("Years")+
  ylab("")+
  geom_rect(aes(xmin=1965,xmax=1977,ymin=1250,ymax=1350),fill="white")+
  annotate("text",x=1972,y=1325,label="Year HydroPower production [0.5pKh]] ", size=2)+
  annotate("text",x=1971,y=1275,label="Year Rainfall Sum [mm]", size=2)+
  geom_segment(x=1966,y=1325,xend=1967,yend=1325,colour="blue")+
  geom_segment(x=1966,y=1275,xend=1967,yend=1275,colour="red")


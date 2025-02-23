library(dplyr)
library(data.table)
library(ggplot2)


f8<-file.choose() #CracowRain
CracowTable<-read.csv(f8)
f9<-file.choose()
PmTable<-read.csv(f9)#Cracow2023

options(warn=-1)
#Liczenie sredniej
AveragePm<-PmTable %>%
  #group_by(Data)%>%
  mutate(AveragePM=mean(PM2.5,na.rm=TRUE)) %>%
  select(Data,AveragePM)
AveragePmDouble=AveragePm[1,2,drop=TRUE]

PmTable=PmTable[,c(1,2)]
PmDaily<-PmTable%>%
  mutate(Data=as.character(substring(Data,1,10)))%>%
  group_by(Data)%>%
  summarise(AverageDailyPm=mean(PM2.5,na.rm=TRUE))

CracowTableResult<-CracowTable%>%
  mutate(time=as.character(substring(time,1,10)))%>%
  group_by(time)%>%
  summarise(SumRain=sum(rain..mm.,na.rm=TRUE))%>%
  select(time,SumRain)

BoolRainCracow<-CracowTableResult%>%
  mutate(DidItRain = case_when(
    SumRain>0~ TRUE,
    SumRain==0 ~ FALSE))%>%
  group_by(DidItRain)%>%
  summarise(RainCounter=n())%>%
  mutate(PrcRain=RainCounter/sum(RainCounter,na.rm=TRUE))
BoolRainCracow


falseString=paste(as.character(round(BoolRainCracow[1,3,drop=TRUE],digits=3)),"%")
trueString=paste(as.character(1-round(BoolRainCracow[1,3,drop=TRUE],digits=3)),"%")


final<-CracowTableResult%>%
  inner_join(PmDaily,by=c("time"="Data"))
 

a<-cor.test(final$SumRain,final$AverageDailyPm,method=c("spearman"))
Spearman<-as.character(round(a$estimate,digits=2))
Spearman<-paste("Współczynnik Spearmana z PM2.5:",Spearman)

pie(BoolRainCracow$RainCounter,labels = c(FALSE,TRUE),main="Did it rain?")
text(0.2,0.5,falseString)
text(-0.2,-0.2,trueString)
text(0.1,1.1,Spearman)




library(dplyr)
library(ggplot2)
library(openmeteo)
pure_data<-weather_history(c(52.2298,21.0118),"1990-01-01","2009-12-31",
                            c("temperature_2m","apparent_temperature","precipitation"))
data<-na.omit(pure_data)
colnames(data)<-c("time","temperature_2m","apparent_temperature",
                  "precipitation") 
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
data<-transform(data, temperature_2m=as.double(temperature_2m),
                apparent_temperature=as.double(apparent_temperature),
                precipitation=as.double(precipitation))

data<-transform(data,month=strftime(data$time,"%B"))
data$month<-factor(data$month,levels=unique(data$month))
data<-transform(data,daytime=ifelse(as.integer(strftime(data$time,"%H"))>8 & 
                      as.integer(strftime(data$time,"%H"))<19,
                      yes="Day",no="Night"))
data$daytime<-factor(data$daytime,levels=c("Day","Night"))
#deszcz definiujemy powyżej 0.5mm
data<-transform(data,are_precipitation=
                  factor(ifelse(precipitation>0.5,"Yes","No"),
                         levels=c("Yes","No")))

x<-data %>% summarize("Mean temperature"=mean(temperature_2m,na.rm = TRUE),
                      "Mean apparent temperature"=mean(apparent_temperature,
                                                       na.rm = TRUE),
                      .by=c(month,daytime,are_precipitation))

z<-x %>% tidyr::pivot_longer(cols=c("Mean temperature","Mean apparent temperature"),
                             names_to="type",
                      values_to="temperature_value")
z<-z %>% transform(factor2=paste(type,are_precipitation,sep="_"))

ggplot(z,aes(x=month))+
  geom_col(aes(y=temperature_value,fill=are_precipitation),
           position=position_dodge(width=0.6))+
  labs(x="Month",y="Temperature [°C]",
       title="Temperature differences due to precipitation ")+
  facet_grid(z$daytime ~ z$type, axes="all_x")+
  scale_fill_manual(values=c("#F5C710","#2297E6"))+
  guides(fill=guide_legend(title="Are precipitation?"))

ggsave("Mean temp and mean app temp.png",width=17,height=10)

ggplot(x,aes(x=month))+
  geom_col(aes(y=`Mean temperature`,fill=are_precipitation),
           position=position_dodge(width=0.6))+
  labs(x="Month",y="Temperature [°C]",
       title="Mean temperature differences due to precipitation ")+
  scale_fill_manual(values=c("#2297E6","#F5C710"))+
  facet_grid(x$daytime, axes="all_x")+
  guides(fill=guide_legend(title="Are precipitation?"))  

ggsave("Mean temp.png",width=10,height=8)

ggplot(x,aes(x=month))+
  geom_col(aes(y=`Mean apparent temperature`,fill=are_precipitation),
           position=position_dodge(width=0.6))+
  labs(x="Month",y="Temperature [°C]",
       title="Mean apparent temperature differences due to precipitation ")+
  scale_fill_manual(values=c("#2297E6","#DF536B"))+
  facet_grid(x$daytime, axes="all_x")+
  guides(fill=guide_legend(title="Are precipitation?"))  

ggsave("App temp.png",width=10,height=8)
                     
  








setwd("../desktop/learning/reproducibleresearch/RepData_PeerAssessment1")
actdata<-read.csv("activity.csv",header=TRUE,colClasses=c("numeric","Date","numeric"))
library(lubridate)
library(dplyr)
maturedata<-actdata%>%mutate(absinterval=floor(interval/100)*60+interval%%100)
minute(maturedata$date)<-maturedata$absinterval
maturedata<-select(maturedata,steps,date,interval)
names(maturedata)<-c("steps","time","interval")
maturedata<-mutate(maturedata,date=as.Date(time),month=month(time),monthday=day(time),weekday=wday(time))

#mean total number of steps taken per day
TotalByDay<-maturedata%>%select(steps,date)%>%group_by(date)%>%summarise(totalsteps=sum(steps))%>%filter(!is.na(totalsteps))
hist(TotalByDay$totalsteps,col="green",breaks=15,main="histogram of total steps by day",xlab="Total Steps Per Day")
mean(TotalByDay$totalsteps)
median(TotalByDay$totalsteps)

#average daily activity pattern
AvgDailyAct<-maturedata%>%mutate(timeofday=format(time,format="%H:%M:%S"))%>%select(timeofday,steps)%>%group_by(timeofday)%>%summarise(AvgSteps=mean(steps,na.rm=TRUE))
library(ggplot2)
library(scales)
AvgDailyAct<-mutate(AvgDailyAct,time=as.POSIXct(timeofday,format="%H:%M:%S"))
ggplot(data=AvgDailyAct,aes(time,AvgSteps))+geom_line()+scale_x_datetime(labels=date_format("%H:%M"))
AvgDailyAct$timeofday[which.max(AvgDailyAct$AvgSteps)]

#calculate missing value
sum(is.na(maturedata$steps))
#fill data with average value through other days
maturedata<-maturedata%>%mutate(timeofday=format(time,format="%H:%M:%S"))
filled<-merge(maturedata,AvgDailyAct[,1:2],all.x=TRUE)[,2:9]
filled<-filled%>%mutate(steps=ifelse(is.na(steps),round(AvgSteps),steps))
filled<-filled[order(filled$time),]
#Process filled data as first assignment
TotalByDay_filled<-filled%>%select(steps,date)%>%group_by(date)%>%summarise(totalsteps=sum(steps))
par(mfrow=c(2,1),mar=c(2,1,2,1),oma=c(0,1,0,1))
hist(TotalByDay$totalsteps,col="green",breaks=15,main="histogram of total steps by day",xlab="")
hist(TotalByDay_filled$totalsteps,col="green",breaks=15,main="histogram of total steps by day (filled)",xlab="")
mean(TotalByDay_filled$totalsteps)
median(TotalByDay_filled$totalsteps)

#calculate daily pattern separately on weekdays and weekends, we use filled data
#Note: for ease of coding, instead of using factor, I used group_by in dplyr
weekdata<-filled%>%mutate(timeofday=format(time,format="%H:%M:%S"),weekends=ifelse(weekday==1|weekday==7,"weekends","weekdays"))%>%select(steps,timeofday,weekends)%>%group_by(timeofday,weekends)%>%summarise(AvgSteps=mean(steps))
weekdata<-mutate(weekdata,time=as.POSIXct(timeofday,format="%H:%M:%S"))
ggplot(data=weekdata,aes(time,AvgSteps))+geom_line()+scale_x_datetime(labels=date_format("%H:%M"))+facet_grid(weekends~.)

#libraries
library(dplyr)
library(boot)
library(ggplot2)
library(lubridate)

## Turtle monitoring analysis
tdata <- read.csv("TurtleData.csv",header=T,sep=",")
tdata$AgeAtCapture <- as.numeric(as.character(tdata$AgeAtCapture))

#kill the negative effort
tdata <- tdata[-which(tdata$effort<0),]

#time filtering
tdata$ymd <- ymd(paste(tdata$year,tdata$month,tdata$day,sep="-"))
tz(tdata$ymd) <- "" #no timezone

#filter the data
length(which(tdata$effort<0)) #how many we are getting rid of
tdata <- filter(tdata,effort>0) # filter negative efforts


unique.turtles <- as.data.frame(tdata%>%group_by(year)%>%summarise(ut=length(unique(turtleid[!is.na(turtleid)])))%>%ungroup())
unique.turtles 
year.turtles <- as.vector(table(tdata$year))


activity.year <- as.data.frame(tdata%>%group_by(year,activity)%>%summarise(sum(!is.na(turtleid)))%>%ungroup())
filter(activity.year,activity=="Radio Tracking")
filter(activity.year,activity=="Visual Survey")


plot(unique.turtles$ut~year.turtles)


#Unique turtle IDS   
# we want a effort ID which corresponds to a unique turtle monitoring event
# here we consider the fact that only one section will be observed on a given day
# therefore the combination of the effortid (tag for the day of sampling) and section
# will be unique with the addition of start time ... 

tdata$event <- paste(as.character(tdata$effortid),tdata$section,tdata$timestart,sep="_")

simulationdata <- as.data.frame(tdata%>%group_by(event)%>%summarise(effort=unique(effort),numturtles=sum(!is.na(turtleid)))%>%ungroup())

simulationdata$bin <- as.character(cut(simulationdata$effort,c(seq(0,180,20),max(simulationdata$effort)),right=T)) #10 mintues
simulationdata$samp <- apply(simulationdata,1,FUN = function(x){as.numeric(gsub("]","",unlist(strsplit(x[4],","))[2]))})
simulationdata[which(simulationdata$samp==max(simulationdata$samp)),"samp"] <- "180+" #change to 3+ hour label
simulationdata$samp <- factor(simulationdata$samp,levels=c(seq(20,180,20),"180+")) #set the order

nsamp <- c(5,10,15,20,25,30)

rdata <- NULL
for(i in 1:1000){
  for (j in nsamp){

resamp <- as.data.frame(simulationdata %>% group_by(samp) %>% sample_n(.,j,replace=TRUE) 
                        %>% summarise(numturtles=mean(numturtles,na.rm=T)))[,2]

    temp <- data.frame(n=j,resamp=resamp)
    rdata <- rbind(rdata,temp)
  }
}

simdata=data.frame(effort=rep(c(seq(20,180,20),"180+"),1000*length(nsamp)),nsamp=rdata$n,meanturtles=rdata$resamp)

ave.numturtles <- as.data.frame(simdata%>%group_by(effort,nsamp)%>%
                                  summarise(mean=mean(meanturtles),sd=sd(meanturtles))%>%
                                  ungroup())
#set plot levels
simdata$effort=factor(simdata$effort,levels=c(seq(20,180,20),"180+"))
ave.numturtles$effort=factor(ave.numturtles$effort,levels=c(seq(20,180,20),"180+"))


p1=ggplot(simdata,aes(x=effort,y=meanturtles))+geom_boxplot()+theme_bw()+
  labs(x="Sample time",y="# of unique turtles")+facet_wrap(~nsamp,nrow=3);p1

p2=ggplot(ave.numturtles,aes(x=effort,y=mean,group=1))+geom_point()+geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd))+
  geom_line()+theme_bw()+labs(x="Sample time",y="Mean # of unique turtles")+facet_wrap(~nsamp,nrow=3);p2


### Effort to grab new turles ----
tdata$AgeAtCapture=as.character(tdata$AgeAtCapture)

#we want 19+ yr old turtles so we can use all with some degree of reason given time sampled 1996-2015
turtle.data=tdata[-which(tdata$AgeAtCapture==""),]
#percent we are tossing out
length(which(turtle.data$AgeAtCapture %in% as.character(c(1:18))))/nrow(turtle.data)*100

turtle.data <- tdata[-which(tdata$AgeAtCapture %in% as.character(c(1:18))),] #19 + turtles ** assume that non-aged fit this rule

number.unique <- length(unique(turtle.data$turtleid));number.unique

#unique sample event
turtle.data$event <- paste(as.character(turtle.data$effortid),turtle.data$section,turtle.data$timestart,sep="_")

#time ordering
turtle.data <- turtle.data[order(turtle.data$ymd),]

#total effort in minutes
turtle.data$teffort <- turtle.data$effort*turtle.data$observers


years <- unique(turtle.data$year)


df=NULL # total dataframe shell
for(i in years){
  temp <- filter(turtle.data,year==i)
  events <- unique(temp$event)  
  
  unique.turtles <- NA # starting a vector for the new turtles in a given year (i)
  effort.log <- NULL # growing log of effort for each sequential sample event within a year (i)

  
  for(j in events){
      temp2 <- subset(temp,event==j)
      unique.turtles <- c(unique.turtles,temp2$turtleid) #vector of turtles
      unique.turtles <- unique.turtles[!duplicated(unique.turtles)] # get rid of duplicated incase of recapture
      effort.log <- sum(c(effort.log,sum(temp2$teffort))) # total cumulative effort
      new.turtles <- temp2$turtleid[is.element(temp2$turtleid,unique.turtles)] # the new turtles added
      
      out <- data.frame(year=i,event=j,ymd=temp2$ymd[1],effort=sum(temp2$teffort),cum_effort=effort.log,
                        total_turtles=sum(!is.na(unique.turtles)),
                        new_turtles=sum(!is.na(new.turtles)))
      df=rbind(df,out)
      
    }
}

,colour=factor(year)scale_x_log10()+annotation_logticks(sides="b")+

#all years
ggplot(df,aes(x=cum_effort,y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
  stat_smooth()+geom_hline(aes(yintercept=number.unique),lty=2)
#no 2015
ggplot(filter(df,year!=2015),aes(x=cum_effort,y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
  stat_smooth()

ggplot(df,aes(x=cum_effort,y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
  stat_smooth()+facet_wrap(~year,nrow=10)

ggplot(filter(df,year!=2015),aes(x=cum_effort,y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
  stat_smooth()+facet_wrap(~year,nrow=10)


#composite of all years

events <- unique(turtle.data$event)  

unique.turtles <- NA # starting a vector for the new turtles in a given year (i)
effort.log <- NULL # growing log of effort for each sequential sample event within a year (i)
df2=NULL

for(j in events){
  temp2 <- subset(turtle.data,event==j)
  unique.turtles <- c(unique.turtles,temp2$turtleid) #vector of turtles
  unique.turtles <- unique.turtles[!duplicated(unique.turtles)] # get rid of duplicated incase of recapture
  effort.log <- sum(c(effort.log,sum(temp2$teffort))) # total cumulative effort
  new.turtles <- temp2$turtleid[is.element(temp2$turtleid,unique.turtles)] # the new turtles added
  
  out <- data.frame(year=i,event=j,ymd=temp2$ymd[1],effort=sum(temp2$teffort),cum_effort=effort.log,
                    total_turtles=sum(!is.na(unique.turtles)),
                    new_turtles=sum(!is.na(new.turtles)))
  df2=rbind(df2,out)
  
}

ggplot(df2,aes(x=cum_effort,y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
  stat_smooth()+geom_hline(aes(yintercept=number.unique),lty=2)

ggplot(df2,aes(x=cum_effort,y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
  stat_smooth()+geom_hline(aes(yintercept=number.unique),lty=2)+scale_y_log10()+scale_x_log10()+annotation_logticks(sides="bl")

ggplot(df2,aes(x=log10(cum_effort),y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
  stat_smooth()

ggplot(df2,aes(x=cum_effort,y=log10(total_turtles)))+geom_point()+theme_bw()+theme(legend.position="none")+
  stat_smooth()
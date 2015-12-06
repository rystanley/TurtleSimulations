### Analaysis of Keji Blandings turtle monitoring data. 


#load required libraries -------
      library(dplyr)
      library(boot)
      library(ggplot2)
      library(lubridate)
      library(segmented)

#custom functions
      mround <- function(x,base){ 
        base*ceiling(x/base) 
      } 

## load data, change data formats and clean bugs
      tdata <- read.csv("TurtleData.csv",header=T,sep=",")
      
      tdata$AgeAtCapture <- as.numeric(as.character(tdata$AgeAtCapture)) # convert from factor to character
      
      tdata <- tdata[-which(tdata$effort<0),] #remove and sampling efforts recorded as negative. 

      tdata$ymd <- ymd(paste(tdata$year,tdata$month,tdata$day,sep="-"))#convert date-time to POSIX for filtering
      tz(tdata$ymd) <- "" # remove timezone associated with the POSIX data


#Data exploration ------------
      unique.turtles <- as.data.frame(tdata%>%group_by(year)%>%summarise(ut=length(unique(turtleid[!is.na(turtleid)])))%>%ungroup())
      unique.turtles$sampling <- as.vector(table(tdata$year)) # number of unique turtles each year
      
      p1 <- ggplot(unique.turtles,aes(x=sampling,y=ut))+geom_point()+
        theme_bw()+stat_smooth(method="lm")+
        labs(x="Number of observational events",y="Number of unique turtles");p1
      
      ggsave("UniqueTurtles~ObservationalEvents.png",p1)
      
      #number of a times a sampling method was used in each year 
      activity.year <- as.data.frame(tdata%>%group_by(year,activity)%>%summarise(sum(!is.na(turtleid)))%>%ungroup())
      filter(activity.year,activity=="Radio Tracking")
      filter(activity.year,activity=="Visual Survey")


## Relationship between sampling time and the number of turtles observed. For this analysis we resampled the data to derive
#  estimates of the number of turtles what would be expected to be observed based on the effort in minutes   
      
        
        tdata$event <- paste(as.character(tdata$effortid),tdata$section,tdata$timestart,sep="_") # tag for a sampling event
        
        # summarize the data for the number of turtles observed in a given sampling event. Note that no observations
        # are considered NA
        
        simulationdata <- as.data.frame(tdata%>%group_by(event)%>%summarise(effort=unique(effort),numturtles=sum(!is.na(turtleid)))%>%ungroup())
        
        simulationdata$bin <- as.character(cut(simulationdata$effort,c(seq(0,180,20),max(simulationdata$effort)),right=T)) #10 mintues
        simulationdata$samp <- apply(simulationdata,1,FUN = function(x){as.numeric(gsub("]","",unlist(strsplit(x[4],","))[2]))})
        simulationdata[which(simulationdata$samp==max(simulationdata$samp)),"samp"] <- "180+" #change to 3+ hour label
        simulationdata$samp <- factor(simulationdata$samp,levels=c(seq(20,180,20),"180+")) #set the order
        
        #number of observations to bootstrap from the sampling effort groupings
        nsamp <- c(5,10,15,20,25,30)
        
        rdata <- NULL # takes ~ 2.5 mintues
        for(i in 1:1000){ # number of randomizations
          for (j in nsamp){ # sampling levels
        
          resamp <- as.data.frame(simulationdata %>% group_by(samp) %>% sample_n(.,j,replace=TRUE) 
                                %>% summarise(numturtles=mean(numturtles,na.rm=T)))[,2]
        
            temp <- data.frame(n=j,resamp=resamp)
            rdata <- rbind(rdata,temp)
          }
        }
        
        # create a dataframe with the resampling estimates (mean number of turtles among each of the resampling levels)
        simdata <- data.frame(effort=rep(c(seq(20,180,20),"180+"),1000*length(nsamp)),nsamp=rdata$n,meanturtles=rdata$resamp)
        
        # Derive summary statistics from the resampling events 
        ave.numturtles <- as.data.frame(simdata%>%group_by(effort,nsamp)%>%
                                          summarise(mean=mean(meanturtles),sd=sd(meanturtles))%>%
                                          ungroup())
        #set factor levels for plotting
        simdata$effort <- factor(simdata$effort,levels=c(seq(20,180,20),"180+"))
        ave.numturtles$effort <- factor(ave.numturtles$effort,levels=c(seq(20,180,20),"180+"))
        
        ## Facet plot of all the resampling levels
        p2 <- ggplot(simdata,aes(x=effort,y=meanturtles))+geom_boxplot()+theme_bw()+
          labs(x=expression(paste("Sampling effort (minutes . # ",observers^-1,")",sep="")),y="# of turtles")+facet_wrap(~nsamp,nrow=3);p2
        
        p3 <- ggplot(ave.numturtles,aes(x=effort,y=mean,group=1))+geom_point()+geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd))+
          geom_line()+theme_bw()+labs(x=expression(paste("Sampling effort (minutes . # ",observers^-1,")",sep="")),y="Mean # of turtles ± sd")+facet_wrap(~nsamp,nrow=3);p3
        
        ## we can see that regardless of the resampling level the resutls are similar in that
        ## sampling approaching an effort (number of minutes of sampling / number of observers) peaks ~ 140. Only
        ## the error changes. 
        
        p4 <- ggplot(filter(simdata,nsamp==20),aes(x=effort,y=meanturtles))+geom_boxplot()+theme_bw()+
          labs(x=expression(paste("Sampling effort (minutes . # ",observers^-1,")",sep="")),y="Number of turtles");p4
        
        p5 <- ggplot(filter(ave.numturtles,nsamp==20),aes(x=effort,y=mean,group=1))+geom_point()+
          geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.2)+geom_line()+theme_bw()+
          labs(x=expression(paste("Sampling effort (minutes . # ",observers^-1,")",sep="")),
               y="Mean # of turtles ± sd");p5
        
        ## save plots
        ggsave("NumberTurtles~SamplingEffort.png",p4,width=7,height=7)
        ggsave("MeanNumberTurtles~SamplingEffort.png",p5,width=7,height=7)
        
        
        ## Now the sampling effort is a difficult metric to interpret. Instead we will re-run the simulation with sampling mintues which is the
        ## composite of the effort * # of observers. 
        
        simulationdata2 <- as.data.frame(tdata%>%group_by(event)%>%summarise(effort=unique(effort),
                                                                             observers=unique(observers),
                                                                             numturtles=sum(!is.na(turtleid)))%>%ungroup())
        
        simulationdata2$teffort <- simulationdata2$effort * simulationdata2$observers * simulationdata2$observers #minutes sampled
        
        simulationdata2$bin <- as.character(cut(simulationdata2$teffort,c(seq(0,600,90),max(simulationdata2$teffort)),right=T)) #10 mintues
        simulationdata2$samp <- apply(simulationdata2,1,FUN = function(x){as.numeric(gsub("]","",unlist(strsplit(x[6],","))[2]))})
        simulationdata2[which(simulationdata2$samp==max(simulationdata2$samp)),"samp"] <- "600+" #change to 3+ hour label
        simulationdata2$samp <- factor(simulationdata2$samp,levels=c(seq(90,600,90),"600+")) #set the order
        
        #number of observations to bootstrap from the sampling effort groupings
        nsamp <- c(5,10,15)
        
        rdata <- NULL # takes ~ 2.5 mintues
        for(i in 1:1000){ # number of randomizations
          for (j in nsamp){ # sampling levels
            
            resamp <- as.data.frame(simulationdata2 %>% group_by(samp) %>% sample_n(.,j,replace=TRUE) 
                                    %>% summarise(numturtles=mean(numturtles,na.rm=T)))[,2]
            
            temp <- data.frame(n=j,resamp=resamp)
            rdata <- rbind(rdata,temp)
          }
        }
        
        # create a dataframe with the resampling estimates (mean number of turtles among each of the resampling levels)
        simdata2 <- data.frame(effort=rep(c(seq(90,600,90),"600+"),1000*length(nsamp)),nsamp=rdata$n,meanturtles=rdata$resamp)
        
        # Derive summary statistics from the resampling events 
        ave.numturtles2 <- as.data.frame(simdata2%>%group_by(effort,nsamp)%>%
                                           summarise(mean=mean(meanturtles),sd=sd(meanturtles))%>%
                                           ungroup())
        #set factor levels for plotting
        simdata2$effort <- factor(simdata2$effort,levels=c(seq(90,600,90),"600+"))
        ave.numturtles2$effort <- factor(ave.numturtles2$effort,levels=c(seq(90,600,90),"600+"))
        
        ## Facet plot of all the resampling levels
        p6 <- ggplot(simdata2,aes(x=effort,y=meanturtles))+geom_boxplot()+theme_bw()+
          labs(x="Sampling effort (total people minutes)",y="Number of turtles")+
          facet_wrap(~nsamp,nrow=3);p6
        
        p7 <- ggplot(ave.numturtles2,aes(x=effort,y=mean,group=1))+geom_point()+geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd))+
          geom_line()+theme_bw()+labs(x="Sampling effort (total people minutes)",y="Mean # of turtles ± sd")+
          facet_wrap(~nsamp,nrow=3);p7
        
        ## we can see that regardless of the resampling level the resutls are similar in that
        ## sampling approaching an effort (number people minutes sampling) peaks ~ 450 minutes . Only
        ## the error changes. 
        
        p8 <- ggplot(filter(simdata2,nsamp==10),aes(x=effort,y=meanturtles))+geom_boxplot()+theme_bw()+
          labs(x="Sampling effort (total people minutes)",y="Number of turtles");p4
        
        p9 <- ggplot(filter(ave.numturtles2,nsamp==10),aes(x=effort,y=mean,group=1))+
          geom_point()+geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.2)+
          geom_line()+theme_bw()+labs(x="Sampling effort (total people minutes)",y="Mean # of turtles ± sd");p9
       
        ## save plots
        ggsave("NumberTurtles~Effort.png",p8,width=7,height=7)
        ggsave("MeanNumberTurtles~Effort.png",p9,width=7,height=7)

### Effort to grab new turles --------

        #we want 19+ yr old turtles so we can use all with some degree of reason given time sampled 1996-2015
        temp <- tdata[-which(tdata$AgeAtCapture==""),]
        #percent we are tossing out
        print(paste0("~",round(length(which(temp$AgeAtCapture %in% as.character(c(1:18))))/nrow(temp)*100),
                    "% data not used (< 19 yr old turtles)"))
        
        #subset the data for turtles younger than 19 years old. Here we assume that turtles which have no age identified
        #are older than the age of cutoff (19yrs). 
        turtle.data <- tdata[-which(tdata$AgeAtCapture %in% as.character(c(1:18))),] 
        number.unique <- length(unique(turtle.data$turtleid))-1# number of unique turtles obseved over the monitoring data period
        
        print(paste0(number.unique," turtles (19+ yrs old) observed between ",min(tdata$year)," & ",max(tdata$year)))
        
        #unique sample event
        turtle.data$event <- paste(as.character(turtle.data$effortid),turtle.data$section,turtle.data$timestart,sep="_")
        
        #order data sequntially through time
        turtle.data <- turtle.data[order(turtle.data$ymd),]
        
        #total effort in people minutes
        turtle.data$teffort <- turtle.data$effort*turtle.data$observers*turtle.data$observers
        
        # index of the sampling years with data available
        years <- unique(turtle.data$year)
        
        # calculate the return on sampling investment. How many more turtles are observed as a function of time 
        # spent monitoring (total ppl mintues) ** for each year
        
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
              df <- rbind(df,out)
              
            }
        }
        
        
        # Number of new turtles as a function of effort 
        p10a <- ggplot(df,aes(x=cum_effort/60,y=total_turtles))+geom_point()+
               theme_bw()+theme(legend.position="none")+
               stat_smooth()+geom_hline(aes(yintercept=number.unique),lty=2)+
               labs(x="Cumulative sampling effort (people hours)",y="Number of unique turtle observations")+
               scale_y_continuous(limits=c(0,mround(number.unique,5)),
                            breaks=c(seq(0,round(number.unique/10)*10,5),number.unique,mround(number.unique,5)),
                            labels=c(seq(0,round(number.unique/10)*10,5),number.unique,mround(number.unique,5)));p10a
        
        #2015 was an anomlous year with the most effort sustained over the longest period and this scews the results
        p10b <- ggplot(filter(df,year!=2015),aes(x=cum_effort/60,y=total_turtles))+geom_point()+
               theme_bw()+theme(legend.position="none")+
               stat_smooth()+geom_hline(aes(yintercept=number.unique),lty=2)+
               labs(x="Cumulative sampling effort (people hours)",y="Number of unique turtle observations")+
               scale_y_continuous(limits=c(0,mround(number.unique,5)),
                             breaks=c(seq(0,round(number.unique/10)*10,5),number.unique,mround(number.unique,5)),
                             labels=c(seq(0,round(number.unique/10)*10,5),number.unique,mround(number.unique,5)));p10b
        
        #wrapped for each year
        p11a <-  ggplot(df,aes(x=cum_effort/60,y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
          stat_smooth()+facet_wrap(~year,nrow=10)+
          labs(x="Cumulative sampling effort (people hours)",y="Number of unique turtle observations");p11a
        
        p11b <- ggplot(filter(df,year!=2015),aes(x=cum_effort,y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
          stat_smooth()+facet_wrap(~year,nrow=10)+
          labs(x="Cumulative sampling effort (people hours)",y="Number of unique turtle observations");p11b
        
        ## save plots
        ggsave("UniqueTurtles~Effort_byYear_composite.png",p10a,width=7,height=7)
        ggsave("UniqueTurtles~Effort_byYear_composite_no2015.png",p10b,width=7,height=7)
        ggsave("UniqueTurtles~Effort_byYear_facet.png",p11a,width=7,height=7)
        ggsave("UniqueTurtles~Effort_byYear_facet_no2015.png",p11b,width=7,height=7)


#Look at the number of unique turtles observed with cumulative effort over all years

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

df2$cum_effort <- df2$cum_effort/60 # convert to hours 

#fit the plot of effort to observe new turtles among all years
#fit a segmented regression model to show the relationship and inflection point
fit=lm(total_turtles~cum_effort,df2)
segmented.mod=segmented(fit,seg.Z=~cum_effort,psi=2000)
inflectionpoint <- as.numeric(segmented.mod$psi.history[5])

#data from segmented model for plotting
dat2 <- data.frame(cum_effort=df2$cum_effort,total_turtles=broken.line(segmented.mod)$fit)


p12 <- ggplot(df2,aes(x=cum_effort,y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
  geom_hline(aes(yintercept=number.unique),lty=2)+
  geom_line(data=dat2)+
  geom_hline(aes(yintercept=number.unique),lty=2)+
  geom_vline(aes(xintercept=inflectionpoint),lty=2)+
  labs(y="Number of turtles",x="Sampling effort (people hours)")+
  scale_y_continuous(limits=c(0,mround(number.unique,5)),
                     breaks=c(seq(0,round(number.unique/10)*10,5),number.unique,mround(number.unique,5)),
                     labels=c(seq(0,round(number.unique/10)*10,5),number.unique,mround(number.unique,5)));p12


dat2 <- data.frame(cum_effort=df2$cum_effort,total_turtles=broken.line(segmented.mod)$fit)

ggplot(df2,aes(x=cum_effort/60,y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
  geom_hline(aes(yintercept=number.unique),lty=2)+geom_line(data=dat2)+
  geom_vline(aes(xintercept=inflectionpoint/60),lty=2)+labs(x="Hours of effort",y="Cumulative effort")

ggplot(df2,aes(x=cum_effort,y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
  stat_smooth()+geom_hline(aes(yintercept=number.unique),lty=2)+scale_y_log10()+scale_x_log10()+annotation_logticks(sides="bl")

ggplot(df2,aes(x=log10(cum_effort),y=total_turtles))+geom_point()+theme_bw()+theme(legend.position="none")+
  stat_smooth()

ggplot(df2,aes(x=cum_effort,y=log10(total_turtles)))+geom_point()+theme_bw()+theme(legend.position="none")+
  stat_smooth()
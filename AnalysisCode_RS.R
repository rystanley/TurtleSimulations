#libraries
library(dplyr)

## Turtle monitoring analysis
tdata <- read.csv("TurtleData.csv",header=T,sep=",")

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
table(simulationdata$bin)



###
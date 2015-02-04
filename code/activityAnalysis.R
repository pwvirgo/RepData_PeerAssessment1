# project 1 of Reproducible Research February 2015

setwd("~/a/highEd/dataScience_coursera/reproduce/prjct1")


# get the data from the course web site 
# dUrl="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
unzip("activity.zip", exdir="data")

stepdata=read.csv("data/activity.csv")

str(stepdata)
summary(stepdata)
cmplt<-stepdata[!is.na(stepdata$steps),]

#str(cmplt)
# find the total number of steps per day ignoring missing values
steps=aggregate(cmplt$steps, by=list(cmplt$date), FUN=sum)
s
step.mean=mean(steps$x)
step.median=median(steps$x)

library(ggplot2)
plt=ggplot(steps, aes(x=x)) + 
   geom_histogram(,color="black", fill="white", binwidth=2000) +
   labs(title="Number of Steps per Day", y="Number of days", 
        x="Number of Steps")
plt + 
   geom_vline(aes(xintercept=step.mean), color="red", 
         linetype="dashed", size=1)
   geom_vline(aes(xintercept=step.median), color="blue", 
         linetype="dashed", size=1)

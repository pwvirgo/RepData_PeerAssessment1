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

step.mean=mean(steps$x)
step.median=median(steps$x)
format(step.mean, digits=12, nsmall=1, big.mark=",")
sprintf("Total steps per day - mean: %8.1f  median %8.1f", step.mean, step.median)

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

#---------------------------------------------------------------------
# What is the Average daily activity pattern?
#
# 1) Make a time series plot (i.e. type = "l") of the 5-minute interval
#    (x-axis) and the average number of steps taken, averaged across all
#    days (y-axis)
#
# 2) Which 5-minute interval, on average across all the days in the
#    dataset, contains the maximum number of steps?
#---------------------------------------------------------------------
head(cmplt)

avg5=aggregate(cmplt$steps, by=list(cmplt$interval), FUN=mean)
head(avg5)

most4=avg5[avg5$x==max(avg5$x),]
tmp<-sprintf("%s\nMaximum steps (%4.1f) occured interval %5i", 
      "Average steps per day for each 5 minute interval",
      most4$x, most4$Group.1)

ggplot(avg5, aes(x=Group.1, y=x)) + geom_line() + 
   labs(title=tmp, x="(5 minute) Interval", y="Number of Steps")

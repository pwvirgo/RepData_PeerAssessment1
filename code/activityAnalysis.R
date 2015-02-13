# project 1 of Reproducible Research February 2015

setwd("~/a/highEd/dataScience_coursera/reproduce/prjct1")


# get the data from the course web site 
# dUrl="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
unzip("activity.zip", exdir="data")

stepdata=read.csv("data/activity.csv")
summary(stepdata)
# Create a dataframe cmplt with no missing values
cmplt<-stepdata[!is.na(stepdata$steps),]
# -------------------------------------------------------------------
# Find the mean and median of the total number of steps per day
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Since this will be done 2 times - once ignoring missing vlaues and
# once with imputed missing values - the code to summarise the data
# will be put in a reuseable function.
# -------------------------------------------------------------------
stepsPerDay<-function(df) {
   steps=aggregate(df$steps, by=list(df$date), FUN=sum)
   
   stepped.mean=mean(steps$x)
   stepped.median=median(steps$x)
   tmp=sprintf("Steps per day.  Mean: %8.1f  Median %8.1f",
               stepped.mean, stepped.median)
   
   library(ggplot2)
   plt=ggplot(steps, aes(x=x)) + 
      geom_histogram(,color="black", fill="white", binwidth=2000) +
      labs(title=tmp, y="Number of days", 
           x="Number of Steps")
#    the following line fails
#    "Error in eval(expr, envir, enclos) : object 'stepped.mean' not found"
#    although it works fine in knitr!
#    + 
#       geom_vline(aes(xintercept=stepped.mean), color="red", 
#                  linetype="dashed", size=1)
   plt
}
# -------------------------------------------------------------------
# Create a histogram to report on the Steps per day when missing values
# are not included.  Also report the Mean and Median steps per day.
# -------------------------------------------------------------------
stepsPerDay(cmplt)


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



# -------------------------------------------------------------------
# Imputing missing values
# Make a dataframe like the original data but replace missing values
# for steps with imputed values.  The imputed values are the average
# number of steps for each time interval.
# -------------------------------------------------------------------
nmiss=sum(is.na(stepdata$steps))
# create df impute like stepdata plus column "x" mean # steps @ interval
impute<-merge(stepdata, avg5, by.y="Group.1", by.x="interval")
# replace each missing steps with average sterps for that interval
impute$steps[is.na(impute$steps)]<-impute$x[is.na(impute$steps)]
summary(impute)
stepsPerDay(impute)

impute$date<-as.Date(impute$date)
impute$wd<-weekdays(impute$date, abbreviate=T)
impute$wd<-ifelse(impute$wd=="Sun" | impute$wd=="Sat","Weekend","Weekday")
impute$wd<-as.factor(impute$wd)
avgstep=aggregate(impute$steps, by=list(impute$wd, impute$interval),FUN=mean)
colnames(avgstep)<-c('day_type', 'interval', 'steps')
tmp="Mean number of steps on Weekdays and Weekends"

ggplot(avgstep, aes(x=interval, y=steps, color=day_type)) + geom_line() + 
   labs(title=tmp, x="(5 minute) Interval", y="Number of Steps") + 
   facet_wrap(~ day_type, nrow=2) + 
   theme_bw() +
   theme(strip.background=element_rect(fill="#faefde"), 
         legend.position="none")

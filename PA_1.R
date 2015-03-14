# Data analysis for reproducible Research 
# default options
Sys.setlocale("LC_TIME", "English")
opts_chunk$set(echo=TRUE, results='asis')
# read the dataset
wd<-getwd()
fn<-paste(wd,"/activity.csv",sep = "")
dset <- read.csv(fn, header = TRUE,stringsAsFactors=FALSE)
# exam the structure
str(dset)
head(dset,n = 1000)
tail(dset,n = 1000)
# transforming the variables

# convert dates to proper type
dset$date<-as.Date(dset$date)
# format intervals, extract hours and minutes
ival<-formatC(dset$interval,digits = 0,width = 4,flag = "0000")
dset$hour<-substr(ival,start = 1,stop = 2)
dset$mins<-substr(ival,start = 3,stop = 4)

# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.

# 1. Make a histogram of the total number of steps taken each day
##### Compute the column and plot the histogram
stepspday <- aggregate(steps ~ date, data=dset, FUN="sum", na.exclude=TRUE)
hist(stepspday$steps,breaks = 12,
     xlab = "No of steps / day",
     ylab = "% fraction of days",
     main = "Total number of steps",
     col = "orange",border = "purple"
     )
# 2. Calculate and report the mean and median total number of steps taken per day
mean(stepspday$steps, na.rm=TRUE)
median(stepspday$steps, na.rm=TRUE)

# What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken,
# averaged across all days (y-axis)

dset$interval<-as.factor(dset$interval)
meanspinterval <- aggregate(steps ~ interval, data=dset, FUN="mean", na.exclude=TRUE)
library(lattice)
xyplot(steps ~ interval, data=meanspinterval,col = "blue",
       type="l", grid=TRUE, xlab="day time interval", 
       ylab="Average steps",main="Averaged Daily Activity",)
# 2. Which 5-minute interval, on average across all the days 
# in the dataset, contains the maximum number of steps?
maxed <- meanspinterval$interval[which.max(meanspinterval$steps)]
maxed<-formatC(as.integer(as.character(maxed)),digits=0,width = 4,flag = "0")
maxed<-paste(substr("Average daily activity is maximized at : ",maxed,1,2)," hours and ", substr(maxed,3,4)," minutes",sep="")


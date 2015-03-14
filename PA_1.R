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

# Imputing missing values
# Note that there are a number of days/intervals where there are missing
# values (coded as NA). The presence of missing days may introduce bias
# into some calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset 
###  (i.e. the total number of rows with NAs)
NAs <- sum(is.na(dset$steps))
NAs

# 2. Devise a strategy for filling in all of the missing values in 
#### the dataset. The strategy does not need to be sophisticated. 
#### For example, you could use the mean/median for that day, 
#### or the mean for that 5-minute interval, etc.

# I will use the step means per interval already stored... 
str(meanspinterval)
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# make copy
dsetfill<-dset
# index of the NAs
idx<-which(is.na(dset$steps))
# substitute NAs with mean interval values
dsetfill$steps[idx]<-meanspinterval$steps[dset$interval[idx]]

# 4. Make a histogram of the total number of steps taken each day
# and Calculate and report the mean and median total number of steps 
# taken per day. Do these values differ from the estimates from the 
# first part of the assignment? 
# What is the impact of imputing missing data on the estimates 
# of the total daily number of steps?
fstepspday <- aggregate(steps ~ date, data=dsetfill, FUN="sum", na.exclude=TRUE)
par(mfrow=c(1,2))
hist(stepspday$steps,breaks = 12,
     xlab = "No of steps / day",
     ylab = "% fraction of days",
     main = "Total number of steps",
     col = "orange",border = "purple"
)
hist(fstepspday$steps,breaks = 12,
     xlab = "No of steps / day",
     ylab = "% fraction of days",
     main = "Total number of steps NAs averaged per interval",
     col = "red",border = "black"
)
# mean and median with NA adjustment
mean(fstepspday$steps, na.rm=TRUE)
median(fstepspday$steps, na.rm=TRUE)
# original mean and median 
mean(stepspday$steps, na.rm=TRUE)
median(stepspday$steps, na.rm=TRUE)
# means remain identical from the NAs imputation
# medians differs slightly
# Shapes of distributions does not differentiate 
# the volume of steps increases as result of NAs imputation

# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with 
# two levels -- "weekday" and "weekend" indicating whether a given date 
# is a weekday or weekend day.
dsetfill$day <- "working"
dsetfill$day[weekdays(as.Date(dsetfill$date),abb=TRUE) %in% c("Sat","Sun")] <- "weekend"
table(dsetfill$day)

# 2. Make a panel plot containing a time series plot (i.e. type = "l")
# of the 5-minute interval (x-axis) and the average number of steps 
# taken, averaged across all weekday days or weekend days (y-axis). 
meanspday <- aggregate(steps ~ interval + day, data=dsetfill, FUN="mean")
xyplot(steps ~ interval | day, data=meanspday, type="l", grid=T, layout=c(1,2), 
       ylab="Average steps", xlab="time intervals (5 mins)", 
       main="Average activity Weekdays vs. Weekends")




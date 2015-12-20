
#Reproducible Research - Peer Assessment 1

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Data

The data for this assignment can be downloaded from the course web site:

**Dataset:** Activity monitoring data [52K]
The variables included in this dataset are:

**steps:** Number of steps taking in a 5-minute interval (missing values are coded as NA)

**Date:** The date on which the measurement was taken in YYYY-MM-DD format

**Interval:** Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

##Required Packages

This project requires both the *DPLYR* and *GGPLOT2* packages to be loaded in order to run.


```r
library
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

## Loading the data
Load the data into the environment and ensure it is the right format for processing (table object)


```r
raw<-read.csv(unz("activity.zip","activity.csv"))
activity <- tbl_df(raw)
```

##What is mean total number of steps taken per day?

**1. Calculate the total number of steps taken per day**


```r
ByDay <-summarise(group_by(activity,date),TotalSteps=sum(steps))
```

**2. Make a histogram of the total number of steps taken each day**


```r
hist(ByDay$TotalSteps, breaks = 25, main = "Histogram of Total # of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

**3. Calculate and report the mean and median of the total number of steps taken per day**

Mean Steps per Day

```r
mean((ByDay$TotalSteps), na.rm = TRUE)
```

```
## [1] 10766.19
```

Median Steps per Day

```r
median((ByDay$TotalSteps), na.rm = TRUE)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

**1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

First create a factor to group on based on the interval and summarise, ignoring null values

```r
activity$IntervalFctr<-as.factor(activity$interval)
ByInterval<-summarise(group_by(activity,IntervalFctr),MeanSteps=mean(steps, na.rm = TRUE))
```

Then plot the results in a line graph

```r
ByInterval$IntervalFctr<-as.numeric(as.character(ByInterval$IntervalFctr))
plot(ByInterval$IntervalFctr, ByInterval$MeanSteps, type = "l", main='Activity Per Day', ylab ='Mean # of Steps', xlab='5 Minute Intervals')
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
MaxSteps<-which.max(ByInterval$MeanSteps)
ByInterval[MaxSteps,]
```

```
## Source: local data frame [1 x 2]
## 
##   IntervalFctr MeanSteps
## 1          835  206.1698
```

##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

I decided to impute the number of steps for the observations with missing step data by using the mean of the  particular weekday it fell on.


```r
activity$weekday <- weekdays(as.Date(activity$date))
activity$weekday <- factor(activity$weekday)
ByWeekday <- summarise(group_by(activity,weekday),MeanSteps= mean(steps,na.rm = TRUE))
```

**3. Create a new dataset that is equal to the original dataset but with the missing data filled in.**


```r
ByImpute <- merge(activity, ByWeekday, by=c("weekday"))
ByImpute$steps<-ifelse(is.na(ByImpute$steps),ByImpute$MeanSteps,ByImpute$steps)
```

**4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**


```r
ByDay <-summarise(group_by(ByImpute,date),TotalSteps=sum(steps))
hist(ByDay$TotalSteps, breaks = 25, main = "Histogram of Total # of Steps per Day (Imputed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

Mean Steps per Day

```r
mean((ByDay$TotalSteps))
```

```
## [1] 10821.21
```

Median Steps per Day

```r
median((ByDay$TotalSteps))
```

```
## [1] 11015
```

## Are there differences in activity patterns between weekdays and weekends?

Use the dataset with the filled-in missing values for this part.

**1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**


```r
ByImpute$IsWeekday <- ifelse(ByImpute$weekday=='Saturday'|ByImpute$weekday=='Sunday','Weekend','Weekday')
ByWeekdayInterval <-summarise(group_by(ByImpute,IntervalFctr,IsWeekday),MeanSteps=mean(steps))
```

**2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.**


```r
ggplot(ByWeekdayInterval, aes(x=IntervalFctr, y=MeanSteps, group=1)) + geom_line() + scale_x_discrete(breaks=seq(0,2500,500)) + facet_wrap(~IsWeekday, nrow=2) + ylab("Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 

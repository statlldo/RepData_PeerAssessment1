---
title: "RepData_PeerAssessment"
author: "Me"
date: "06/12/2020"
output: 
  html_document: 
    keep_md: yes
---



## Reproducible data Peer ASsessment

First, load the data into R, and change the format of the date to date format.
Take a look at the first few lines.

Then, use the aggregate function to get total steps per day
Take a look at the first few lines of this too.


```r
myData<-read.csv("activity.csv")
head(myData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
myData$date<-as.Date(myData$date)

TotalByDay<- aggregate(myData$steps, list(Date = myData$date), FUN = sum, na.rm = TRUE)
head(TotalByDay)
```

```
##         Date     x
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

## Histogram to show the total number of steps per day



```r
hist(TotalByDay$x, xlab = "Steps in a day", ylab = "number of days" ,main = "Histogram of daily total steps")
```

![](PA1_template_files/figure-html/totalSteps-1.png)<!-- -->

## Mean of steps per day


```r
x<-format(mean(TotalByDay$x, na.rm = TRUE), scientific = FALSE)
```
The mean of steps per day is 9354.23

## Median of steps per day


```r
y<-format(median(TotalByDay$x, na.rm = TRUE), scientific = FALSE)
```
The median of steps per day is 10395

## Average number of steps by day 
(I wasn't clear what is meant by this request - average steps per interval for each day? This will just mirror total steps per day? But can't think what else is being asked for...)


```r
library(ggplot2)
TotalByDay$av<-TotalByDay$x/288
 ggplot(data=TotalByDay, aes(x=Date, y = av)) + geom_line() + labs(title = "Average steps per interval, by day", x = "Date", y = "Average steps per 5 minute interval")
```

![](PA1_template_files/figure-html/avPerDay-1.png)<!-- -->

##which interval has the maximum number of steps, across all days?

Plot total steps across time period, by interval. 


```r
TotalByInterval<- aggregate(myData$steps, list(Interval = myData$interval), FUN = sum, na.rm = TRUE)
TotalByInterval$av<-TotalByInterval$x
head(TotalByInterval)
```

```
##   Interval   x  av
## 1        0  91  91
## 2        5  18  18
## 3       10   7   7
## 4       15   8   8
## 5       20   4   4
## 6       25 111 111
```

```r
summary(TotalByInterval)
```

```
##     Interval            x                 av         
##  Min.   :   0.0   Min.   :    0.0   Min.   :    0.0  
##  1st Qu.: 588.8   1st Qu.:  131.8   1st Qu.:  131.8  
##  Median :1177.5   Median : 1808.0   Median : 1808.0  
##  Mean   :1177.5   Mean   : 1981.3   Mean   : 1981.3  
##  3rd Qu.:1766.2   3rd Qu.: 2800.2   3rd Qu.: 2800.2  
##  Max.   :2355.0   Max.   :10927.0   Max.   :10927.0
```

```r
 ggplot(data=TotalByInterval, aes(x=Interval, y = x)) + geom_line() + labs(title = "Total steps by interval across whole time period", x = "Interval", y = "Total steps per 5 minute interval")
```

![](PA1_template_files/figure-html/maxInterval-1.png)<!-- -->

```r
 maxMe<-which.max(TotalByInterval$x)
maxInt <- TotalByInterval[maxMe,]$Interval
print(maxInt)
```

```
## [1] 835
```
The interval with the most steps is 835        

## Impute for missing values
Code has been written to impute the missing values using the average (mean) for the rest of that Date
(A better approach might be to take the nearest neighbour). This code
1. Loads the dplyr package
2. Gets a logical vector to show where NAs are in the dataset
2. Calculates the average steps per interval for each day
3. Imputes (2) into any missing values for that day
4. If all values for a day are NA, '0' is imputed


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
myData %>% 
  group_by(interval) %>% 
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```

```
## # A tibble: 17,568 x 3
## # Groups:   interval [288]
##     steps date       interval
##     <dbl> <date>        <int>
##  1 1.72   2012-10-01        0
##  2 0.340  2012-10-01        5
##  3 0.132  2012-10-01       10
##  4 0.151  2012-10-01       15
##  5 0.0755 2012-10-01       20
##  6 2.09   2012-10-01       25
##  7 0.528  2012-10-01       30
##  8 0.868  2012-10-01       35
##  9 0      2012-10-01       40
## 10 1.47   2012-10-01       45
## # ... with 17,558 more rows
```

Now with this dataset, I recalculate the mean, median and show the histogram again. 



## Histogram to show the total number of steps per day after imputation (shows imputation has made a difference)



```r
myData$date<-as.Date(myData$date)

TotalByDay<- aggregate(myData$steps, list(Date = myData$date), FUN = sum, na.rm = TRUE)

hist(TotalByDay$x, xlab = "Steps in a day", ylab = "number of days" ,main = "Histogram of daily total steps")
```

![](PA1_template_files/figure-html/totalStepsImp-1.png)<!-- -->

## Mean of steps per day after imputation (different to before imputation)


```r
x<-format(mean(TotalByDay$x, na.rm = TRUE), scientific = FALSE)
```
The mean of steps per day after imputation is 9354.23

## Median of steps per day after imputation (different to before imputation)


```r
y<-format(median(TotalByDay$x, na.rm = TRUE), scientific = FALSE)
```
The median of steps per day after imputation is 10395

## Difference between weekdays and weekends

First, add a column to identify weekdays or weekends using the weekday function
Split the data into two datasets depending on weekend vs weekday
Then show two histograms, to show the difference in steps by interval


```r
myData$weekday<-weekdays(myData$date)
weekendData<-myData[myData$weekday=="Saturday" | myData$weekday =="Sunday",]
weekdayData<-myData[myData$weekday=="Monday" | myData$weekday =="Tuesday" | myData$weekday=="Wednesday" | myData$weekday =="Thursday" | myData$weekday=="Friday",]

head(weekdayData)
```

```
##   steps       date interval weekday
## 1    NA 2012-10-01        0  Monday
## 2    NA 2012-10-01        5  Monday
## 3    NA 2012-10-01       10  Monday
## 4    NA 2012-10-01       15  Monday
## 5    NA 2012-10-01       20  Monday
## 6    NA 2012-10-01       25  Monday
```

```r
TotalByIntervalWeekday<- aggregate(weekdayData$steps, list(Interval = weekdayData$interval), FUN = sum, na.rm = TRUE)
TotalByIntervalWeekday$av<-TotalByIntervalWeekday$x

TotalByIntervalWeekend<- aggregate(weekendData$steps, list(Interval = weekendData$interval), FUN = sum, na.rm = TRUE)
TotalByIntervalWeekend$av<-TotalByIntervalWeekend$x

head(TotalByIntervalWeekend)
```

```
##   Interval  x av
## 1        0  0  0
## 2        5  0  0
## 3       10  0  0
## 4       15  0  0
## 5       20  0  0
## 6       25 52 52
```

```r
#panels
par(mfrow=c(2,1), mar=c(4,4,2,1))

plot(x~Interval,data=TotalByIntervalWeekday,type="l",main="Steps on Weekdays", xlab="Interval", col="blue")
plot(x~Interval,data=TotalByIntervalWeekend,type="l",main="Steps on Weekends", xlab="Interval", col="red")
```

![](PA1_template_files/figure-html/weekday-1.png)<!-- -->

##The end

# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
head(activity)
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

## What is mean total number of steps taken per day?

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.2
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
head(table(activity$date))
```

```
## 
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##        288        288        288        288        288        288
```

```r
bydate <- group_by(activity, date)
step_per_day <- summarize(bydate, steps=sum(steps))
mean(step_per_day$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

## What is the average daily activity pattern?

```r
head(table(activity$interval))
```

```
## 
##  0  5 10 15 20 25 
## 61 61 61 61 61 61
```

```r
byinterval <- group_by(activity, interval)
step_by_interval <- summarize(byinterval, steps=mean(steps, na.rm=TRUE))
plot(step_by_interval)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Imputing missing values

```r
table(is.na(activity$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

```r
#step_by_interval$interval <- as.factor(step_by_interval$interval)
#activity$interval <- as.factor(activity$interval)
imactivity <- merge(activity, step_by_interval, by.x="interval", by.y="interval" )
imactivity$steps.x[is.na(imactivity$steps.x)] <- 
    imactivity$steps.y[is.na(imactivity$steps.x)]
bydate <- group_by(imactivity, date)
step_per_day <- summarize(bydate, steps=sum(steps.x))    
hist(step_per_day$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

## Are there differences in activity patterns between weekdays and weekends?

```r
activity$weekdays <- as.factor(weekdays(as.Date(activity$date)))
activity$weekdays <- factor(activity$weekdays, levels(activity$weekdays)[c(2,6,7,5,1,3,4)])
table(activity$weekdays)
```

```
## 
##    Monday   Tuesday Wednesday  Thursday    Friday  Saturday    Sunday 
##      2592      2592      2592      2592      2592      2304      2304
```

```r
byweekdays <- group_by(activity, weekdays, interval)
step_by_iw <- summarize(byweekdays, steps=mean(steps, na.rm=TRUE))
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.2
```

```r
qplot(interval, steps, data=step_by_iw, facets=weekdays~., geom=c("line"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

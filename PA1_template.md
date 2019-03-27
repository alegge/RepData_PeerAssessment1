---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
1. Load the data (i.e. read.csv())

```r
library(ggplot2)
library(lattice)
activity_df <- read.csv('activity.csv')
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
daily_steps <- aggregate(steps ~ date, activity_df, sum, na.rm = TRUE)
g <- ggplot(daily_steps, aes(date, steps)) + geom_bar(stat = "identity") 
g <- g+ theme(axis.text.x = element_text(angle = 90)) + labs(title = "Total number of steps per day")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

2. Calculate and report the mean and median total number of steps taken per day

```r
mean(daily_steps$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(daily_steps$steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
average_steps <- aggregate(steps ~ interval, activity_df, mean)
plot(average_steps, type = 'l', xlab = '5 Minute Intervals', ylab = 'Average number of steps', main = 'Time Series Plot')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
average_steps[average_steps$steps == max(average_steps$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity_df))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Replace null values with 5 minute interval mean. 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
library(zoo)
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
no_nulls <- activity_df
for (i in 1:nrow(no_nulls)) {
    if (is.na(no_nulls$steps[i])) {
        no_nulls$steps[i] <- average_steps[which(no_nulls$interval[i] == average_steps$interval), ]$steps
    }
}
head(no_nulls)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
daily_steps_na <- aggregate(steps ~ date, no_nulls, sum, na.rm = TRUE)
g <- ggplot(daily_steps_na, aes(date, steps)) + geom_bar(stat = "identity") 
g <- g + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Total number of steps per day with imputed values with no missing data")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(daily_steps_na$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(daily_steps_na$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```
After imputing the missing data, the mean is the same and the median increased by 1.19.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
no_nulls$dateType <- ifelse(as.POSIXlt(no_nulls$date)$wday %in% c(0,6), "Weekend", "Weekday")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
average_days <- aggregate(steps ~ dateType + interval, no_nulls, mean)
xyplot(average_days$steps ~ average_days$interval | average_days$dateType, layout = c(1,2), type = 'l', xlab = '5 minute intervals', ylab = 'Average number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


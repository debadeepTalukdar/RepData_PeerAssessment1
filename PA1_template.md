---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

``` r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

``` r
steps_per_day <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)
hist(steps_per_day$steps, main="Total Steps Per Day", xlab="Steps", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

``` r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

``` r
median(steps_per_day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

``` r
avg_interval <- aggregate(steps ~ interval, activity, mean, na.rm=TRUE)
plot(avg_interval$interval, avg_interval$steps, type="l",
     xlab="5-min Interval", ylab="Avg Steps", main="Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
avg_interval[which.max(avg_interval$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

``` r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

``` r
activity_filled <- activity
for(i in 1:nrow(activity_filled)) {
  if(is.na(activity_filled$steps[i])) {
    interval_val <- activity_filled$interval[i]
    activity_filled$steps[i] <- avg_interval$steps[avg_interval$interval == interval_val]
  }
}

steps_filled <- aggregate(steps ~ date, activity_filled, sum)
hist(steps_filled$steps, main="Total Steps Per Day (Imputed)", xlab="Steps", col="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
mean(steps_filled$steps)
```

```
## [1] 10766.19
```

``` r
median(steps_filled$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
activity_filled$day_type <- ifelse(weekdays(activity_filled$date) 
                                   %in% c("Saturday","Sunday"), 
                                   "weekend", "weekday")
activity_filled$day_type <- as.factor(activity_filled$day_type)

avg_by_daytype <- aggregate(steps ~ interval + day_type, activity_filled, mean)

library(lattice)
xyplot(steps ~ interval | day_type, data=avg_by_daytype, 
       type="l", layout=c(1,2),
       xlab="Interval", ylab="Average Steps",
       main="Weekday vs Weekend Activity")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Reproducible Research Assignment1 
========================================================
# by Waleed Hassan
## Loading and preprocessing the data

```r
setwd("~/GitHub/RepData_PeerAssessment1/activity")
data <- read.csv("activity.csv", header= TRUE)
```
## What is mean total number of steps taken per day?
### Make a histogram of the total number of steps taken each day

```r
daily <- tapply(data$steps, data$date, sum) # calc sum of steps for each day
hist(daily)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 
### Calculate and report the mean and median total number of steps taken per day

```r
mean<- mean(daily) 
median<- median(daily)
```
### The mean of total number of steps taken per day is NA, while the median is NA

## What is the average daily activity pattern?
### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
interval <- tapply(data$steps, data$interval, mean, na.rm=T)
plot(unique(data$interval), interval, type='l')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max<- interval[interval == max(interval)]
max
```

```
##   835 
## 206.2
```
## Imputing missing values
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
na <- is.na(data$steps)
length(na[na == TRUE])
```

```
## [1] 2304
```
### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
average_per_interval <- aggregate(steps~interval, data=data, FUN=mean)
```

```
## Error: object 'FUN' of mode 'function' was not found
```

```r
imputed.steps <- numeric()
for(i in 1:nrow(data)) {
  obs <- data[i,]
  if (is.na(obs$steps)) {
    steps <- subset(average_per_interval,interval==obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  imputed.steps <- c(imputed.steps,steps)
}
```

```
## Error: object 'average_per_interval' not found
```
### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
imputed.data <- data
imputed.data$steps <- imputed.steps
```

```
## Error: replacement has 0 rows, data has 17568
```
### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
imp.dailyy <- tapply(imputed.data$steps, imputed.data$date, sum) # calc sum of steps for each day
hist(imp.daily)
```

```
## Error: object 'imp.daily' not found
```

```r
mean(imp.daily) 
```

```
## Error: object 'imp.daily' not found
```

```r
median(imp.daily)
```

```
## Error: object 'imp.daily' not found
```
### there was a slight difference between both means and medians

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
imputed.data$date<- as.Date(imputed.data$date)
imputed.data$day<- weekdays(imputed.data$date)
imputed.data$graph <- imputed.data$day
imputed.data$graph<- replace(imputed.data$graph, imputed.data$graph =="Monday", "weekday")
imputed.data$graph<- replace(imputed.data$graph, imputed.data$graph =="Tuesday", "weekday")
imputed.data$graph<- replace(imputed.data$graph, imputed.data$graph =="Wednesday", "weekday")
imputed.data$graph<- replace(imputed.data$graph, imputed.data$graph =="Thursday", "weekday")
imputed.data$graph<- replace(imputed.data$graph, imputed.data$graph =="Friday", "weekday")
imputed.data$graph<- replace(imputed.data$graph, imputed.data$graph =="Saturday", "weekend")
imputed.data$graph<- replace(imputed.data$graph, imputed.data$graph =="Sunday", "weekend")
```
### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(plyr)
library(lattice)
average.steps <- ddply(imputed.data, .(interval, graph), summarize, steps = mean(steps))
xyplot(steps ~ interval | graph, data = average.steps, layout = c(1, 2), type = "l")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

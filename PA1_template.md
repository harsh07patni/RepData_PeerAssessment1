#Reproducible Research: Peer Assessment 1

##Loading and preprocessing the data

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
setwd("E:/R Training Files/05 Reproducable Research")
df <- read.csv("activity.csv")
df$date <- as.Date(df$date)
```

3. Process the data to remove the NA

```r
df.completecases <- na.omit(df)
```

##What is mean total number of steps taken per day?
 
1. Make a histogram of the total number of steps taken each day


```r
library(ggplot2)
steps.by.day <- aggregate(steps ~ date, data=df.completecases, FUN=sum)
ggplot(steps.by.day, aes(x=steps)) +
    geom_histogram(binwidth=2500, colour="black", fill="white")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
mean(steps.by.day$steps)
```

```
## [1] 10766.19
```



```r
median(steps.by.day$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps.by.interval <- aggregate(steps ~ interval, data=df.completecases,
                               FUN=mean)
ggplot(steps.by.interval, aes(x=interval, y=steps)) +
  geom_line(stat="identity")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.by.interval$interval[which.max(steps.by.interval$steps)]
```

```
## [1] 835
```

##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
length(which(is.na(df)))
```

```
## [1] 2304
```


```r
length(which(is.na(df$steps)))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
mean(na.omit(df$steps))
```

```
## [1] 37.3826
```


```r
median(na.omit(df$steps))
```

```
## [1] 0
```
### Using the 5-minute interval mean to impute values.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
df.new <- df
df.new$steps[is.na(df.new$steps)] <- mean(na.omit(df$steps))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps.by.day.new <- aggregate(steps ~ date, data=df.new, FUN=sum)
ggplot(steps.by.day.new, aes(x=steps)) +
    geom_histogram(binwidth=2500, colour="black", fill="white")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 


```r
mean(steps.by.day.new$steps)
```

```
## [1] 10766.19
```


```r
median(steps.by.day.new$steps)
```

```
## [1] 10766.19
```


##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
df.new$day[weekdays(as.Date(df.new$date)) %in% c("Saturday", "Sunday")] <- "weekend"
df.new$day[!weekdays(as.Date(df.new$date)) %in% c("Saturday", "Sunday")] <- "weekday"
df.new[, 4] <- as.factor(df.new[, 4])

steps.new <- aggregate(steps ~ interval + day, data=df.new, FUN=mean)
ggplot(steps.new, aes(x=interval, y=steps, group=1)) + geom_line() +
    facet_wrap(~ day, ncol=1)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 

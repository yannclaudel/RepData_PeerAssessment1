# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.3
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```
Load data from activity.zip  

```r
filename <- "activity"
if(!file.exists(paste0(filename,".csv"))) {
    unzip(paste0(filename,".zip"))
    unlink(paste0(filename,".zip"))
}

activity <- read.csv(paste0(filename,".csv"))
activity$date <- as.Date(activity$date)
act <- tbl_df(activity)
actNotNul <- filter(act, !is.na(steps))
```


## What is mean total number of steps taken per day?

Histogram of the total number of steps taken each day

```r
result <-
  actNotNul %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) 
ggplot(data=result, aes(x=date, y=steps)) + geom_bar(stat="identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 
  
Mean and median

```r
meanV <- as.character(round(mean(result$steps),digits=1))  
medianV  <- median(result$steps)
```
The mean of total number of steps taken per day is 10766.2  
The median of total number of steps taken per day is 10765





## What is the average daily activity pattern?

Perfom mean of steps by interval and plot

```r
result <-
  actNotNul %>%
  group_by(interval) %>%
  summarize(steps = mean(steps)) 
  qplot(interval, steps, data = result, geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
  maxV <- max(result$steps)
  intervalMax <- result[which.max(result$steps),]$interval
```
The 5-minute interval 835 contains the maximum number of steps, 206.1698113 steps

## Imputing missing values

```r
missingV <- sum(is.na(act$steps))
```
The total number of missing values in the dataset is 2304

Replacing the missig values with the mean of the corresponding 5-minute interval

```r
actNaEqualsMean <- act
  
  for (i in 1:nrow(actNaEqualsMean)) {
    if (is.na(actNaEqualsMean[i,]$steps)) {
      filterInt <- actNaEqualsMean[i,]$interval
      value <- filter(result,interval == filterInt)$steps
    
      if (!is.na(value))
        actNaEqualsMean[i,]$steps <- value
      else
        actNaEqualsMean[i,]$steps <- 0
      }
  }
```

Histogram of the total number of steps taken each day

```r
result <-
  actNaEqualsMean %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) 
ggplot(data=result, aes(x=date, y=steps)) + geom_bar(stat="identity")  
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 
  Mean and median

```r
meanV <- as.character(round(mean(result$steps),digits=1))  
medianV  <- median(result$steps)
```
The mean of total number of steps taken per day is 10766.2  
The median of total number of steps taken per day is 1.0766189\times 10^{4}



## Are there differences in activity patterns between weekdays and weekends?

Creation of a new factor variable in the dataset with two levels - "weekday" and "weekend" 

```r
actNaEqualsMean <- mutate(actNaEqualsMean, day = as.POSIXlt(date)$wday , we = ifelse(day %in% c(0,6),"weekend","weekday"))
```

Perform and plot the average number of steps taken, averaged across all weekday days or weekend days

```r
result <-
  actNaEqualsMean %>%
  group_by(interval,we) %>%
  summarize(steps = mean(steps)) 

qplot(interval, steps, data = result, geom = "line", facets = we~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 


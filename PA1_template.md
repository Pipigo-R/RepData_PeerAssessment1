Repdata\_project1\_wk2
================
Bing Hu
08Feb2021

# Reproducible Research Peer Assessment 1

## Loading and preprocessing the data

``` r
  ## Get dataset
  dnldzipfile <- "ActivyMonitoringData.zip"
  # Checking if archieve already exists.
  if (!file.exists(dnldzipfile)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileURL, dnldzipfile, method="curl")
  }  
  # Unzip downloaded file into ./ActivyMonitoringData
  if (!file.exists("ActivyMonitoringData")) { 
    unzip(dnldzipfile) 
  }
  activity <- read.csv("./activity.csv" , colClasses = c("numeric", "Date", "numeric"))
```

## What is mean total number of steps taken per day?

1.  Make a histogram of the total number of steps taken each day.

``` r
  activitytot <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)
  hist(activitytot$steps, 
       main="Total steps taken per day", 
       xlab="Steps", 
       ylab="Days", 
       ylim=c(0,35),
       col="blue")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

1.  Calculate and report the mean and median total number of steps taken per day.

-   The mean of the total number of steps taken per day is: 1.076618910^{4}.
-   The median of the total number of steps taken per day is: 1.076510^{4}.

## What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

``` r
  stepsInterval <- aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
  plot(steps~interval,data=stepsInterval, col='blue', lwd=2, type="l", 
       xlab="Interval", ylab="Average number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?. The interval 835 contains the maximum number of steps.

## Imputing missing values.

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs). The total number of missing values in the dataset is 2304.

2.  Devise a strategy for filling in all of the missing values in the dataset use average of the same interval to impute missing value of steps.

``` r
  meanInterval <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)
  mrgActivity <- merge(activity, meanInterval, by="interval")
  imputActivity <- transform(mrgActivity, steps.x = ifelse(is.na(steps.x),steps.y,steps.x))
```

1.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
  newActivity <- data.frame(steps = imputActivity$steps.x, date=imputActivity$date, 
                          interval=imputActivity$interval)
```

1.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

``` r
  newActivitytot <- aggregate(steps ~ date, newActivity, sum, na.rm = TRUE)
  
  hist(newActivitytot$steps, 
       main="Total steps taken per day", 
       xlab="Steps", 
       ylab="Days", 
       ylim=c(0,35),
       col="red")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
  meanSteps <- mean(newActivitytot$steps)
  medianSteps <- median(newActivitytot$steps)
```

-   The mean of the total number of steps taken per day is: 1.076618910^{4}.
-   The median of the total number of steps taken per day is: 1.076618910^{4}.

## Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. Create a new factor column with the Weekday or Weekend identifier:.

``` r
  newActivity$dayType <- weekdays(as.Date(newActivity$date))
  newActivity$dayType[newActivity$dayType %in% c('Saturday','Sunday')] <-"Weekend"
  newActivity$dayType[newActivity$dayType %in% 
                        c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')] <-"Weekday"
  newActivity$dayType <- as.factor(newActivity$dayType)
```

1.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
  Weekday <- subset(newActivity, dayType=="Weekday")
  Weekday_si <- aggregate(steps~interval, data=Weekday, mean)
  
  Weekend <- subset(newActivity, dayType=="Weekend")
  Weekend_si <- aggregate(steps~interval, data=Weekend, mean)
  
  par(mfrow=c(2,1), mar=c(4,4,2,1))
  
    plot(steps~interval,data=Weekday_si,type="l",
         main="Steps for Weekdays", xlab="Time Interval", col="red")
    plot(steps~interval,data=Weekend_si,type="l",
       main="Steps for Weekends", xlab="Time Interval", col="red")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)

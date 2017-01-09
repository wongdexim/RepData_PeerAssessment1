Loading and preprocessing the data
----------------------------------

### Data info

Data set activity.csv from
<https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>

The variables included in this dataset are:

-   steps: Number of steps taking in a 5-minute interval (missing values
    are coded as NA)
-   date: The date on which the measurement was taken in YYYY-MM-DD
    format
-   interval: Identifier for the 5-minute interval in which measurement
    was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

### 1. Code for reading in the dataset and/or processing the data

    #load necessary libraries for assignment 
    library("ggplot2")
    library("dplyr")
    #set directory and read file
    setwd("C://Users//WongD//Documents//R//R-3.3.1//r programming//reproducible research week 2")
    activity<-read.csv("activity.csv", header=TRUE)

What is mean total number of steps taken per day?
-------------------------------------------------

### 2. Histogram of the total number of steps taken each day

    #groups all intervals and steps by date
    daily<-group_by(activity, date)
    daily<-summarise_each(daily, funs(sum))
    # data check
    head(daily)

    ## # A tibble: 6 × 3
    ##         date steps interval
    ##       <fctr> <int>    <int>
    ## 1 2012-10-01    NA   339120
    ## 2 2012-10-02   126   339120
    ## 3 2012-10-03 11352   339120
    ## 4 2012-10-04 12116   339120
    ## 5 2012-10-05 13294   339120
    ## 6 2012-10-06 15420   339120

    # output 2
    hist(daily$steps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    ##alternate format using ggplot
    ##ggplot(data=daily, aes(daily$steps))+geom_histogram()

### 3. Mean and median number of steps taken each day

    #various ways of calculating mean and median
    daily_mean<-mean(daily$steps, na.rm=TRUE)
    daily_median<-median(daily$steps, na.rm=TRUE)
    summary(daily)

    ##          date        steps          interval     
    ##  2012-10-01: 1   Min.   :   41   Min.   :339120  
    ##  2012-10-02: 1   1st Qu.: 8841   1st Qu.:339120  
    ##  2012-10-03: 1   Median :10765   Median :339120  
    ##  2012-10-04: 1   Mean   :10766   Mean   :339120  
    ##  2012-10-05: 1   3rd Qu.:13294   3rd Qu.:339120  
    ##  2012-10-06: 1   Max.   :21194   Max.   :339120  
    ##  (Other)   :55   NA's   :8

Mean=1.076618910^{4}  
Median=10765

What is the average daily activity pattern?
-------------------------------------------

### 4. Time series plot of the average number of steps taken

    ##transforms data so it gets steps mean by interval
    interval5<-aggregate(steps ~ interval, activity, FUN = "mean")
    ##time series plot
    with(interval5, plot(interval, steps, type='l'))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    ##stores max steps
    max_steps_row <- which.max(interval5$steps)

### 5. The 5-minute interval that, on average, contains the maximum number of steps

max step interval = 835

max step = 206.1698113

Imputing missing values
-----------------------

### 6. Code to describe and show a strategy for imputing missing data

Strategy taken is to assume all NAs to be equivalent of mean of the
interval. For example, if date x, interval y has step value of NA, then
step value of NA would be replaced with average steps of interval y.

    imputed<-activity

    ## loop to replace any na value in steps
    for (i in 1:nrow(imputed)) {
      if (is.na(imputed$steps[i])) {
        replacement<-imputed$interval[i]
        imputed$steps[i]<-interval5$steps[interval5$interval==replacement] 
      }
    }

### 7. Histogram of the total number of steps taken each day after missing values are imputed

    #groups all intervals and steps by date, using imputed dataset which has replaced NAs
    imputed_daily<-group_by(imputed, date)
    imputed_daily<-summarise_each(imputed_daily, funs(sum))
    head(imputed_daily)

    ## # A tibble: 6 × 3
    ##         date    steps interval
    ##       <fctr>    <dbl>    <int>
    ## 1 2012-10-01 10766.19   339120
    ## 2 2012-10-02   126.00   339120
    ## 3 2012-10-03 11352.00   339120
    ## 4 2012-10-04 12116.00   339120
    ## 5 2012-10-05 13294.00   339120
    ## 6 2012-10-06 15420.00   339120

    hist(imputed_daily$steps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

    #create empty variable for now to store date
    imputed$date <- as.Date(imputed$date)
    #create a vector of weekdays
    weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
    #Use `%in%` and `weekdays` to create a logical vector
    #convert to `factor` and specify the `levels/labels`
    imputed$wDay <- factor((weekdays(imputed$date) %in% weekdays1),levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

    #split data by weekend and weekdays
    weekend_result<- subset(imputed, wDay=="weekend")
    head(weekend_result)

    ##      steps       date interval    wDay
    ## 1441     0 2012-10-06        0 weekend
    ## 1442     0 2012-10-06        5 weekend
    ## 1443     0 2012-10-06       10 weekend
    ## 1444     0 2012-10-06       15 weekend
    ## 1445     0 2012-10-06       20 weekend
    ## 1446     0 2012-10-06       25 weekend

    weekday_result<- subset(imputed, wDay=="weekday")

    #once data is split by weekend and weekdays, aggregate steps by interval and graph the results
    par(mfrow=c(2,1))
    weekend_interval<-aggregate(steps ~ interval, weekend_result, FUN = "mean")
    with(weekend_interval, plot(interval, steps, type='l', main="weekend avg"))

    weekday_interval<-aggregate(steps ~ interval, weekday_result, FUN = "mean")
    with(weekday_interval, plot(interval, steps, type='l', main="weekday avg"))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

### 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

All code included in this file.

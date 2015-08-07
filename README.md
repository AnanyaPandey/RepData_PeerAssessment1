Analyzing the Fitbit Data
=========================

Ananya Pandey
August 2015

Abstract
--------

This is the first minor project assignment in the **Reproducible Research** course in the Data Science specialization track. The purpose of this assignment is to answer a series of question, analyse the data and report in such a manner that the analysis is reproducible.

Introduction
------------

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data
----

The data for this assignment can be downloaded from the course web site:

-   Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

-   **steps**: Number of steps taking in a 5-minute interval (missing values are coded as `NA`)

-   **date**: The date on which the measurement was taken in YYYY-MM-DD format

-   **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Loading and preprocessing the data

Following the below procedure

1.  Load the data (i.e. `read.csv()`)

2.  Process/transform the data (if necessary) into a format suitable for our analysis

``` r
temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
unzip(temp)
unlink(temp)
data <- read.csv("activity.csv")
```

### What is mean total number of steps taken per day?

For this part we are ignoring the missing values in the dataset.

1.  Make a histogram of the total number of steps taken each day

``` r
agg <- aggregate(data$steps, by=list(date = data$date), FUN=sum, na.rm=TRUE)
names(agg) <- c("Date","Tota_Steps")
hist(agg$Tota_Steps, main = "Total Steps By Day", col="green", xlab = "Total Steps", ylab="Frequency")
```

![](Figures/Histogram.jpg)

1.  Calculate and report the **mean** and **median** total number of steps taken per day

``` r
aggmean <- mean(agg$Tota_Steps)
aggmedian <- median(agg$Tota_Steps)
```

The Aggregate mean is ***9354.23*** and aggregate median is ***10395***

### What is the average daily activity pattern?

1.  Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
stepsbyinterval <- aggregate(data$steps, by = list(Interval = data$interval),mean, na.rm=TRUE)
names(stepsbyinterval) <- c("Interval","Average_Steps")
plot(stepsbyinterval$Interval,stepsbyinterval$Average_Steps, type = "l", main = "Average Steps By Interval", xlab="Interval", ylab="Number Of Steps")
```

![](figures/AverageSpeedByInterval.jpg)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
Max_Interval <- stepsbyinterval[which.max(stepsbyinterval$Average_Steps),1]
```

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is 835

### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

``` r
incomplete <- sum(!complete.cases(data))
```

1.  Imputing the Missing values of data. If the steps in particular interval of a particular date is NA, then it is replaced by the average number of steps for that day

2.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), stepsbyinterval$Average_Steps[match(data$interval, stepsbyinterval$Interval)], data$steps))
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```

1.  Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
steps_by_daynew <- aggregate(steps ~ date, imputed_data, sum)
dev.off()
```

    ## null device 
    ##           1

``` r
hist(steps_by_daynew$steps)
hist(steps_by_daynew$steps, main = paste("Total Steps Each Day"), col="blue", xlab ="Number of Steps", ylab="Frequency", add = TRUE)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "green"), lwd=10)
```

Calculating mean and median for new data

``` r
meannew <- mean(steps_by_daynew$steps)
mediannew <- median(steps_by_daynew$steps)
print(meannew);print(mediannew)
```

    ## [1] 10589.69

    ## [1] 10766.19

-   The imputed data mean is 1.059 × 10<sup>4</sup>
-   The imputed data median is 1.0766 × 10<sup>4</sup>
-   The difference between the non-imputed mean and imputed mean is -176.4949
-   The difference between the non-imputed mean and imputed mean is 1.1887
-   The difference between total number of steps between imputed and non-imputed data is 7.5363 × 10<sup>4</sup>. Thus, there were 7.5363 × 10<sup>4</sup> more steps in the imputed data.

### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
stepbyintervalnew <- aggregate(steps ~ interval + dow, imputed_data, mean)
```

1.  Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:

``` r
library(lattice)
xyplot(stepbyintervalnew$steps ~ stepbyintervalnew$interval | stepbyintervalnew$dow , main = "Average Step Per Day By Interval", xlab="Interval",ylab="Steps",type="l")
```

![](figures/IntervalDifference.jpg)

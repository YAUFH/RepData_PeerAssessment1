Reproducible Research Project 1
================

Introduction
------------

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the ???quantified self??? movement ??? a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

-   Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as ????????) </br> date: The date on which the measurement was taken in YYYY-MM-DD format </br> interval: Identifier for the 5-minute interval in which measurement was taken </br> The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

load package
------------

``` r
library(ggplot2)
library(plyr)
library(lattice)
library(data.table)
```

load data
---------

``` r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
```

Reading csv Data into Data.Table.
---------------------------------

``` r
activity <- data.table::fread(input = "data/activity.csv")
```

process data
------------

``` r
activity$day <- weekdays(as.Date(activity$date))
activity$datetime <- as.POSIXct(activity$date, format= "%y-%m-%d")
cleandata <- activity[!is.na(activity$steps)]
```

1) What is mean total number of steps taken per day?
----------------------------------------------------

1.a) Calculate the total number of steps taken per day
------------------------------------------------------

``` r
totalsteps <- aggregate(activity$step ~ activity$date, FUN = sum)
colnames(totalsteps) <- c("date", "steps")
hist(totalsteps$steps, breaks=5, xlab="steps", main= "total steps/day")
```

![](PA1_Template_files/figure-markdown_github/unnamed-chunk-5-1.png)

1.b)Calculate and report the mean and median of the total number of steps taken per day
---------------------------------------------------------------------------------------

``` r
as.integer(mean(totalsteps$steps))
```

    ## [1] 10766

``` r
as.integer(median(totalsteps$steps))
```

    ## [1] 10765

2)What is the average daily activity pattern?
---------------------------------------------

2.a) Make a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
------------------------------------------------------------------------------------------------------------------------------------------------------------------------

2.a.i)create dataframe with average number of steps per interval
----------------------------------------------------------------

``` r
cleandata <- activity[!is.na(activity$steps),]
table2 <- ddply(cleandata,.(interval), summarize, Avg = mean(steps))
```

2.a.iii)create time series plot of average number of steps per interval
-----------------------------------------------------------------------

``` r
p2 <-ggplot(table2, aes(x=interval, y=Avg), xlab ="interval", ylab ="Average number of steps")
p2 +geom_line()+xlab("interval")+ylab("Average number of steps")+ggtitle("Average number of steps per interval")
```

![](PA1_Template_files/figure-markdown_github/unnamed-chunk-8-1.png)

2.b) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
------------------------------------------------------------------------------------------------------------------

2.b.i) Maximum steps by interval
--------------------------------

``` r
maxAVGsteps <-max(table2$Avg)
```

2.b.ii) interval contains the maximum average number of steps
-------------------------------------------------------------

``` r
table2[table2$Avg==maxAVGsteps,1]
```

    ## [1] 835

3) Imputing missing values
--------------------------

3.a)Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s
------------------------------------------------------------------------------------------------------------------------

``` r
nrow(activity[is.na(activity$steps),])
```

    ## [1] 2304

3.b) Fill missing value with average interval on the day of the week
--------------------------------------------------------------------

``` r
table3 <-ddply(cleandata, .(interval,day), summarize, Avg = mean(steps))
missingdata <- activity[is.na(activity$steps),]
merge1 <- merge(missingdata, table3, by=c("interval","day"))
```

3.c) create new dataset of same format, but with missing values being substituted
---------------------------------------------------------------------------------

3.c.i)reorder substituted dataset
---------------------------------

``` r
newdata1 <- merge1[,c(6,4,1,2,5)]
colnames(newdata1) <-c("steps", "date", "interval", "day", "datetime")
```

3.c.ii)merge substituted dataset with original dataset
------------------------------------------------------

``` r
merge2<- rbind(cleandata, newdata1)
```

3.d)Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
--------------------------------------------------------------------------------------------------------------------------------------------------

``` r
totalsteps2 <- aggregate(merge2$step ~ merge2$date, FUN = sum)
colnames(totalsteps2) <-c("data","steps")
as.integer(mean(totalsteps2$steps))
```

    ## [1] 10821

``` r
as.integer(median(totalsteps2$steps))
```

    ## [1] 11015

3.d.i)plot a histogram of total steps per day
---------------------------------------------

``` r
hist(totalsteps2$steps, breaks=5, xlab = "steps", main = "tatal steps per day without missing value", col = "blue")
hist(totalsteps$steps,breaks = 5,xlab = "steps", main = "total steps per day without missing value", col = "pink", add=T)
legend("topright", c("imputed data", "with NA"), fill = c("blue","pink"))
```

![](PA1_Template_files/figure-markdown_github/unnamed-chunk-16-1.png)

4)Are there differences in activity patterns between weekdays and weekends?
---------------------------------------------------------------------------

4.a) create new factor variable in the dataset indicating weekday and weekend
-----------------------------------------------------------------------------

``` r
merge2$category <-ifelse(merge2$day %in% c("Saturday", "Sunday"), "weekend", "weekday")
```

4.b) plot data by interval and category of weekday vs weekend
=============================================================

``` r
table4 <-ddply(merge2,.(interval, category), summarize, Avg = mean(steps))
xyplot(Avg~interval|category, data=table4, type ="l", layout = c(1,2), 
       main=" average steps per interval based on weekend and weekday",
       ylab = "average number of steps", xlab ="interval")
```

![](PA1_Template_files/figure-markdown_github/unnamed-chunk-18-1.png)

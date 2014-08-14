---
title: "Reproducible Research- Assessment 1"
author: "MC"
date: "Wednesday, August 13, 2014"
output: html_document


---
###Loading and preprocessing the data
Reading data from the working directory and aggregation by date

```r
library(graphics)
library(ggplot2)
library(knitr)
library(markdown)

data<-read.csv("activity.csv", header=TRUE, na.strings="NA")##ok 
data$date<-as.Date(data$date)
steps.date<-aggregate(steps~date, data = data, na.rm=TRUE, FUN = sum)
```
###What is mean total number of steps taken per day?
####Counting mean and median from agegated by days data also some simple statisics were made


```r
head(steps.date)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
tail(steps.date)
```

```
##          date steps
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

```r
summary(steps.date)
```

```
##       date                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

```r
mean<-round(mean(steps.date$steps),2)
median<-round(median(steps.date$steps),2)
```

```r
mean
```

```
## [1] 10766
```

```r
median
```

```
## [1] 10765
```
####First plots

Problem with understanding part with histogram, so two plots were made:
1. classic histogram
2. bar plot of total number of steps taken each day
Plots are saved as .png file 

```r
png(filename="plot1.png", height=480, width=480, 
    bg="white")
hist(steps.date$steps, xlab="sum of steps on each day", ylab="percent of all", main="Histogram")
dev.off()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```
## RStudioGD 
##         2
```

```r
png(filename="plot2.png", height=480, width=480, 
    bg="white")
barplot(steps.date$steps, names.arg=steps.date$date, xlab="Date", ylab="Total number of steps taken each day", main="Pseudohistogam")
dev.off()
```

```
## RStudioGD 
##         2
```
### What is the average daily activity pattern?
maximum numbers of steps wee counted, also linear plot were made and save as .png


```r
interval<-5
steps.interval<-aggregate(steps~interval, data=data, FUN="mean")
png(filename="plot3.png", height=480, width=480, 
    bg="white")
plot(steps.interval, type="l", xlab="5-minute intervals", ylab="average steps in interval", main="average daily activity pattern")
dev.off()
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```
## RStudioGD 
##         2
```

```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

###Imputing missing values

####Number of missing values in all document and in "steps" variable
in all document

```r
missing<-sum(is.na(data))
missing
```

```
## [1] 2304
```
in the steps variable

```r
missing_steps<-sum(is.na(data$steps))
missing_steps
```

```
## [1] 2304
```
#### Changing NA's values 


```r
data1<-read.csv("activity.csv", header=TRUE, na.strings="NA")
steps.date<-aggregate(steps~date, data = data1, na.rm=TRUE, FUN = "mean")

data1<-merge(data1, steps.date, by="date", suffixes=c("",".y"))
nas<-is.na(data1$steps)
data1$steps[nas]<-data1$steps.y[nas]
data1<-data1[, c(1:3)]
```
####Making histogram of data without NA's values

```r
png(filename="plot4.png", height=480, width=480, 
    bg="white")
hist(data1$steps, xlab="sum of steps on each day with median as a method fo NAs", ylab="percent of all", main="Histogram")
dev.off()
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```
## RStudioGD 
##         2
```

####Counting diffrence between mean ande median in raw file and after NA's removing

```r
mean.1<-round(mean(data1$steps),2)
mean.1
```

```
## [1] 37.38
```

```r
median.1<-round(median(data1$steps),2)
median.1
```

```
## [1] 0
```

```r
diff.mean<-mean-mean.1
diff.mean
```

```
## [1] 10729
```

```r
diff.median<-median-median.1
diff.median
```

```
## [1] 10765
```
Impact of loss of data rather low, if we use, aggegrated data, if we choose the orginal data set, and then aggregrate, the mean value shoud be diffrent

###Are there differences in activity patterns between weekdays and weekends?
####Creating a new variable identify weekdays and weekends

```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("sobota", "niedziela")) {
        "weekend"
    } else {
        "weekday"
    }
}
data1$daytype <- as.factor(sapply(data1$date, daytype))
```

####Ploting results

```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = data1, subset = data1$daytype == type, FUN = "mean")
    
    plot(steps.type, type = "l", main = type)
    
}
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

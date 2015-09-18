---
title: "Reproducible research peer assessment 1"
author: "Nicola Davide D'Avanzo"
date: "09/18/2015"
output: html_document
---

This document presents the results of peer assessments 1 of course Reproducible Research on coursera.

**Introduction**

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

**Data**

The data for this assignment can be downloaded from the course web site.

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

**Prepare the R Environment**

Throughout this report when writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code.

First, we set echo equal a TRUE and results equal a 'hold' as global options for this document. 

```{r, echo=TRUE}
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
```

**Load the required data**

The following statement is used to load the data using read.csv().

```{r, echo=TRUE}
#Clear the workspace
rm(list=ls())
#Load the raw activity data
x<-read.csv("/home//ndavanzo/Documents/Coursera/reproducible research/activity.csv",stringsAsFactors=FALSE)
```

I transform the data into a format suitable for the analysis

```{r, echo=TRUE}
x$date<-as.Date(x$date)
head(x)
```

**What is mean total number of steps taken per day?**

I calculate the total number of steps taken per day

```{r, echo=TRUE}
TS<-aggregate(steps ~ date, data = x, sum, na.rm = TRUE)
#Rename the attributes
names(TS)<-c("date","total")
head(TS)
```

I compute the histogram of the total number of steps each day

```{r, echo=TRUE}
HTS<-hist(TS$total,main=c("Total number of steps taken per day (NA removed)"), xlab=c("Total number of steps"),col="red")
```

I calculate the mean and median of the total number of steps taken per day

```{r, echo=TRUE}
MeanTS<-mean(TS$total,na.rm = TRUE)
MedianTS<-median(TS$total,na.rm=TRUE)
```

The mean is 10766.19 and the median is 10765.

**What is the average daily activity pattern?**

I compute the average of steps across all days for each interval

```{r, echo=TRUE}
AS<-aggregate(steps ~ interval, data = x, mean, na.rm = TRUE)
head(AS)
```

I make a plot of the time series of the 5-minute interval and the average number of steps taken, averaged across all days

```{r, echo=TRUE}
plot(AS,type="l",main="Average number of steps time series", xlab=c("5-Minute intervals"), ylab=c("Average number of steps across all days"),col="red")
```

I calculate which 5-minute interval contains the maximum number of steps

```{r, echo=TRUE}
MaxAS_Interval<-AS$interval[which(AS$steps==max(AS$steps))]
```

The 835th interval has maximum 206 steps.

**Imputing missing values**

I calculate the total number of missing values in the dataset

```{r, echo=TRUE}
NA_steps<-sum(is.na(x$steps))
```

The total number of missing values are 2304.

I create a function for filling in all of the missing values in the dataset with the mean for that 5-minute interval

```{r, echo=TRUE}
na_fill <- function(data, pervalue) {
        na_index <- which(is.na(data$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = data[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- data$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}
```

I create a new dataset that is equal to the original dataset but with the missing data filled in

```{r, echo=TRUE}
x_fill <- data.frame(  
        steps = na_fill(x, AS),  
        date = x$date,  
        interval = x$interval)
head(x_fill)
```

I calculate the total number of steps taken per day without missing values

```{r, echo=TRUE}
TSF<-aggregate(steps ~ date, data = x_fill, sum)
```

I compute the histogram of the total number of steps each day

```{r, echo=TRUE}
HTSF<-hist(TSF$steps,main=c("Total number of steps taken per day"), ylab=c("Steps count") , xlab=c("Number of steps"),col="green")
```

I calculate the mean and median of the total number of steps taken per day without missing values

```{r, echo=TRUE}
MeanTSF<-mean(TSF$steps)
MedianTSF<-median(TSF$steps)
```

The mean is 10766.19 and median is 10766.19.

As we can see these values sligthly differ from the estimates from the first part of the assignment.
Before filling the data:

- Mean : 10766.189
- Median: 10765

After filling the data:

- Mean : 10766.189
- Median: 10766.189

As you can see, comparing with the calculations done in the first section of this document, we observe that while the mean value remains unchanged, the median value has shifted and virtual matches to the mean.

**Are there differences in activity patterns between weekdays and weekends?**

I create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

```{r, echo=TRUE}
x_e<-data.frame(x_fill,day=weekdays(as.Date.factor(x$date)))
x_e$day<-as.character(x_e$day)
x_e$day[x_e$day==c("Saturday")|x_e$day==c("Sunday")]<-c("Weekend")
x_e$day[x_e$day!=c("Weekend")]<-c("Weekday")
x_e$day<-as.factor(x_e$day)
head(x_e)
```

I compute the average number of steps taken, averaged across all weekday days or weekend days

```{r, echo=TRUE}
A_X_E<-aggregate(steps~interval + day, data= x_e, mean)
```

I make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```{r, echo=TRUE}
library(lattice)
xyplot(steps ~ interval | day, A_X_E, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```
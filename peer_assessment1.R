#Clear the workspace
rm(list=ls())
#Load the raw activity data
x<-read.csv("activity.csv",stringsAsFactors=FALSE)
#Transform the data into a format suitable for the analysis
x$date<-as.Date(x$date)
head(x)

#Calculate the total number of steps taken per day
TS<-aggregate(steps ~ date, data = x, sum, na.rm = TRUE)
#Rename the attributes
names(TS)<-c("date","total")
head(TS)
#Compute the histogram of the total number of steps each day
HTS<-hist(TS$total,main=c("Total number of steps taken per day (NA removed)"), xlab=c("Total number of steps"),col="red")
#Calculate the mean and median of the total number of steps taken per day
MeanTS<-mean(TS$total,na.rm = TRUE)
MedianTS<-median(TS$total,na.rm=TRUE)

#Compute the average of steps across all days for each interval
AS<-aggregate(steps ~ interval, data = x, mean, na.rm = TRUE)
head(AS)
#Plot the time series of the 5-minute interval and the average number of steps taken, averaged across all days
plot(AS,type="l",main="Average number of steps time series", xlab=c("5-Minute intervals"), ylab=c("Average number of steps across all days"),col="red")
#Calculate which 5-minute interval contains the maximum number of steps
MaxAS_Interval<-AS$interval[which(AS$steps==max(AS$steps))]

#Calculate the total number of missing values in the dataset
NA_steps<-sum(is.na(x$steps))
#Create a function for filling in all of the missing values in the dataset with the mean for that 5-minute interval
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
#Create a new dataset that is equal to the original dataset but with the missing data filled in
x_fill <- data.frame(  
        steps = na_fill(x, AS),  
        date = x$date,  
        interval = x$interval)
head(x_fill)
#Calculate the total number of steps taken per day without missing values
TSF<-aggregate(steps ~ date, data = x_fill, sum)
#Compute the histogram of the total number of steps each day
HTSF<-hist(TSF$steps,main=c("Total number of steps taken per day"), ylab=c("Steps count") , xlab=c("Number of steps"),col="green")
#Calculate the mean and median of the total number of steps taken per day without missing values
MeanTSF<-mean(TSF$steps)
MedianTSF<-median(TSF$steps)
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day
x_e<-data.frame(x_fill,day=weekdays(as.Date.factor(x$date)))
x_e$day<-as.character(x_e$day)
x_e$day[x_e$day==c("Saturday")|x_e$day==c("Sunday")]<-c("Weekend")
x_e$day[x_e$day!=c("Weekend")]<-c("Weekday")
x_e$day<-as.factor(x_e$day)
#Compute the average number of steps taken, averaged across all weekday days or weekend days 
A_X_E<-aggregate(steps~interval + day, data= x_e, mean)
library(lattice)
#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
xyplot(steps ~ interval | day, A_X_E, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")

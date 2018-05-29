---
output: 
  html_document: 
    fig_caption: yes
    fig_width: 8
    keep_md: yes
---
Reproducible Research - Project 1
================================================================================
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:
 
 + Dataset: [Acitivity Monitoring Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) (52K)
        
The variables included in this dataset are:

 + steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
 + date: The date on which the measurement was taken in YYYY-MM-DD format
 + interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.  
 <br />  
 
#### Read in the datafile and change date to date data type.  

```r
library(lattice)
library(ggplot2)
activityMonitoringData <- read.csv("activity.csv", header=T ,sep=",",
                                   row.names=NULL,
                                   colClasses = c("integer","Date","integer"))
```
<br />     

### What is the mean total number of steps taken per day?

#### Set up the data and produce a histogram for the total number of steps per day.

```r
stepTotal <- data.frame(as.table(tapply(activityMonitoringData$steps, 
                                        activityMonitoringData$date,
                                        sum, na.rm=TRUE), row.names()))
names(stepTotal) <- c("Date", "TotalSteps")
stepTotal$Date <- as.Date(stepTotal$Date)
stepTotal$TotalSteps<- as.integer(stepTotal$TotalSteps)


ggplot(data=stepTotal, aes(y=TotalSteps, x=Date)) +
        geom_histogram(stat="identity", width=1)
```

![](PA1_template_files/figure-html/histogram_Total_Steps-1.png)<!-- -->
<br />  

#### What is the mean and median number of steps?


```r
options(scipen = 999)
meanTotalSteps <- mean(stepTotal$TotalSteps, na.rm=TRUE)
medianTotalSteps <- median(stepTotal$TotalSteps, na.rm=TRUE)
```

##### The mean number of steps per day is 9354.2295082.  
##### The median number of steps per day is 10395.  
<br />  

### What is the average daily pattern?

#### Determine the totals for each 5 minute increment, format and plot.  

```r
AveStepsPer5MinInt <- data.frame(as.table(tapply(activityMonitoringData$steps, 
                                                 activityMonitoringData$interval,
                                                 mean, na.rm=TRUE), row.names()))
names(AveStepsPer5MinInt) <- c("TimeOfDay", "MeanSteps")
AveStepsPer5MinInt$TimeOfDay <- as.character(AveStepsPer5MinInt$TimeOfDay)

plot(as.character(AveStepsPer5MinInt$TimeOfDay),AveStepsPer5MinInt$MeanSteps, 
     type = "l", xaxt="n", xlab= "Hour of Day (5 minute increments)",
     ylab= "Average Number of Steps", 
     main="Average Number of Steps Throughout a Day")
axis (side=1, at=c("0000","0300","0600","0900","1200","1500","1800","2100",
                   "2400"))
```

![](PA1_template_files/figure-html/Average_Daily_Pattern-1.png)<!-- -->

```r
MaxStepsTime <- AveStepsPer5MinInt[which.max(AveStepsPer5MinInt$MeanSteps),1 ]
stringi::stri_sub(MaxStepsTime,2,1) <- ":" #found from stackoverflow.com site
```

##### Time of day where average steps are the most is 8:35
<br />     

### Imputing missing values
   
#### Determine the number of rows with NAs.


```r
missingNAs <- nrow(activityMonitoringData[rowSums(is.na(activityMonitoringData))>0,])
```

##### The number of rows where step value is missing is 2304
<br />  

#### Creating a new dataset and replacing the NAs with the time of day mean and ploting new histogram for total steps in a day. Then reporting the new mean and median steps per day.


```r
activityMonitoringData1 <- activityMonitoringData
for (i in 1:nrow(activityMonitoringData1)){
        if (is.na(activityMonitoringData1$steps[i])) {
                activityMonitoringData1$steps[i] <- 
                        AveStepsPer5MinInt$MeanSteps[which(
                                AveStepsPer5MinInt$TimeOfDay == 
                                        activityMonitoringData1$interval[i])]
        }
}
stepTotal1 <- data.frame(as.table(tapply(activityMonitoringData1$steps, 
                                         activityMonitoringData1$date,
                                         sum, na.rm=TRUE), row.names()))
names(stepTotal1) <- c("Date", "TotalSteps")
stepTotal1$Date <- as.Date(stepTotal1$Date)
stepTotal1$TotalSteps<- as.integer(stepTotal1$TotalSteps)

ggplot(data=stepTotal1, aes(y=TotalSteps, x=Date)) +
        geom_histogram(stat="identity", width=1)
```

![](PA1_template_files/figure-html/No_NAs-1.png)<!-- -->

```r
ats <- mean(stepTotal1$TotalSteps)
mdts <- median(stepTotal1$TotalSteps)
```

##### The orignial historgram has multiple days where there were only NA values and when the NAs were populated those days now have values for them.  

##### The new mean number of steps per day is 10766.1639344 a difference of 1411.9344262 from when NAs were ignored.
##### The new median number of steps per day is 10766 a difference of 371 from when NAs were ignored.
<br />

### Are there differences in activity patterns between weekdays and weekends?

#### Creating a new factor variable in the filled-in dataset distiquishing a weekday day and weekend day. Then creating a panel plot containg a time series plot (i.e. type ="l") of the 5-minute interval(x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  


```r
# Converting the date to either Weekday or Weekend 
for (i in 1:nrow(activityMonitoringData1)) {
        wd <- weekdays(activityMonitoringData1[i,2])
        if (wd == "Monday" || wd == "Tuesday" || wd == "Wednesday" || 
            wd == "Thursday" || wd == "Friday" ) {
                activityMonitoringData1[i,4] <- "Weekday" 
                } else { activityMonitoringData1[i,4] <- "Weekend" }
}

# Renaming the new column created from the step above
colnames(activityMonitoringData1)[colnames(activityMonitoringData1)=="V4"] <- "DayOfWeekType"

# Spliting into two data frames to calculate the mean of each Day of Week type dataset.
weekdaydf <- activityMonitoringData1[activityMonitoringData1$DayOfWeekType=='Weekday',]
weekenddf <- activityMonitoringData1[activityMonitoringData1$DayOfWeekType=='Weekend',]

weekdayMean <- data.frame(as.table(tapply(weekdaydf$steps,
                                          weekdaydf$interval,
                                          mean), row.names()))
weekendMean <- data.frame(as.table(tapply(weekenddf$steps,
                                          weekenddf$interval,
                                          mean), row.names()))

# Renaming Data Columns of the dataframes for readability.
names(weekdayMean) <- c("TimeOfDay", "MeanSteps")
names(weekendMean) <- c("TimeOfDay", "MeanSteps")

# Putting an additional column in the new dataframes to idenify them as weekend or weekday.
weekdayMean$WeekDayType <- "Weekday"
weekendMean$WeekDayType <- "Weekend"

# Combining the the two dataframes into a single data frame.
allMeandf <- rbind(weekdayMean, weekendMean)

# Formating the Time of Day for plotting purposes.
allMeandf$TimeOfDay <- as.integer(as.character(allMeandf$TimeOfDay))

xyplot(MeanSteps ~ TimeOfDay | WeekDayType, data=allMeandf, layout = c(1,2),
       type='l', scales=list(x=list(at=seq(0,2355, by=100), rot=90)))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

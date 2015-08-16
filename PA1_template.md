# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Step 1: Check if file is present in working directory, if not download file

```r
if(!file.exists("ActivityData.zip")){
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url, destfile = "ActivityData.zip")
}
```

Step 2: unzip and read datafile into variable

```r
data <- read.csv(unz("./ActivityData.zip", filename = "activity.csv"), 
    colClasses = c("numeric", "Date", "numeric"))
```


## What is mean total number of steps taken per day?
First, group the data by date and summarize the number of steps taken each say using the dplyr package

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
Nsteps <- data %>%
    group_by(date) %>%
    summarize(nSteps = sum(steps))
```

Next a histogram was created which shows the distribution of the amount of steps taken per day

```r
hist(Nsteps$nSteps, breaks=seq(0, 25000, 2500),xlab = "number of steps", 
        main = "Distribution of the amount of \nsteps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Last, the mean and median were calculated

```r
meanTotalSteps <- mean(Nsteps$nSteps, na.rm=TRUE)
medianTotalSteps <- median(Nsteps$nSteps, na.rm=TRUE)
```
mean = 1.0766189\times 10^{4}, median = 1.0765\times 10^{4}
As you can see, there is a 1.1886792 difference in steps between the mean and median


## What is the average daily activity pattern?
The average amount of steps per measurement interval was calculated to obtain insight in the average daily activity pattern.

```r
StepsInterval <- data %>%
    group_by(interval) %>%
    summarize(meanSteps_interval = mean(steps, na.rm=TRUE))
```

The results are plotted in a time series plot (i.e. ` type = "l"`)

```r
plot(StepsInterval$interval, StepsInterval$meanSteps_interval, type = "l"
     , xaxt = "n", main = "Average number of steps \nduring the day"
     , ylab = "N steps", xlab = "Time (hours)")
axis(side=1, at=seq(0,2400,200), labels=seq(0,24,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

The time interval at which the subject was most active was calculated as follows:

```r
maxActivityInterval <- which(StepsInterval$meanSteps_interval==max(StepsInterval$meanSteps_interval)) # returns the index number with the highest amount of steps
maxActivityIntervalTime <- StepsInterval$interval[maxActivityInterval] # returns the time of the previously calculated index number 
```
The subject showed highest average activity at 835, which should be read as 08.35


## Imputing missing values
First, the amount of missing values was calculated

```r
missing <- sum(is.na(data$steps))
```
2304 of 17568 values are missing, which is 0.1311475 % of the total dataset

The indices of the missing values values were calculated prior to imputing the missing values.

```r
missingIndex <- which(is.na(data$steps))
```

A loop was used to go over every index where a value was missing. The missing values were imputed using the average number of steps of the time intervals as calculated in the previous section.

```r
DataNew <- data
for (i in missingIndex) {
    DataNew$steps[i] <- StepsInterval$meanSteps_interval[which(StepsInterval$interval == data$interval[i])]
} 
# which(StepsInterval$interval == data$interval[i]) returns index value of mean steps during time interval
# insert that index value in StepsInterval#meanSteps_interval to find the mean and apply it to the index of dataNew
```

As in the previous section, the average amount of steps per measurement interval was calculated to obtain insight in the changes in the average daily activity pattern as a consequence of imputing the missing values.

```r
NstepsNew <- DataNew %>%
    group_by(date) %>%
    summarize(nSteps = sum(steps))
```

A histogram was created based on the imputed data. The mean and median were also calculated.

```r
hist(NstepsNew$nSteps, breaks=seq(0, 25000, 2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

```r
NewMeanTotalSteps <- mean(NstepsNew$nSteps)
NewMedianTotalSteps <- median(NstepsNew$nSteps) # na.rm=TRUE is no longer necessary
```
The NewMean = 1.0766189\times 10^{4}, the NewMedian = 1.0766189\times 10^{4}.

By imputing the missing values with the average number of steps taken during each interval, there is no longer a difference between the mean and median number of steps taken.

## Are there differences in activity patterns between weekdays and weekends?
First a factor variable was created indicating wether a day was a *Weekday* or *Weekend*-day.

```r
Lang <- Sys.getlocale("LC_TIME") # save current system language to variable
Sys.setlocale("LC_TIME", "English") # set system language to 'English'
```

```
## [1] "English_United States.1252"
```

```r
DataNew$day <- weekdays(DataNew$date)
Sys.setlocale("LC_TIME", Lang) # revert system language to original
```

```
## [1] "Dutch_Netherlands.1252"
```

```r
DataNew$day <- sub("Saturday","Weekend",DataNew$day)
DataNew$day <- sub("Sunday","Weekend",DataNew$day)

DataNew$day <- sub("Monday","Weekday",DataNew$day)
DataNew$day <- sub("Tuesday","Weekday",DataNew$day)
DataNew$day <- sub("Wednesday","Weekday",DataNew$day)
DataNew$day <- sub("Thursday","Weekday",DataNew$day)
DataNew$day <- sub("Friday","Weekday",DataNew$day)

DataNew$day <- factor(DataNew$day)
```
*Unfortunatly my solution to making the factor variable is far from elegant, so please enlighten me with more elegant solutions!*
  
<br>

After the factor variable was created, the average amount of steps during *Weekend* and *Weekdays* was calculated using the dplyr package

```r
Weekend <- DataNew %>%
    filter(day=="Weekend") %>%
    group_by(interval) %>%
    summarize(meanSteps_interval = mean(steps))

Weekday <- DataNew %>%
    filter(day=="Weekday") %>%
    group_by(interval) %>%
    summarize(meanSteps_interval = mean(steps))
```

A time series panel-plot was created to display the differences in average activity between *Weekend* and *Weekdays*

```r
par(mfrow = (c(2,1)))
plot(Weekend$interval, Weekend$meanSteps_interval, type = "l"
     , xaxt = "n", yaxt = "n", ylim = c(0,200), main = "Average number of steps during the Weekend"
     , ylab = "N steps", xlab = "Time (hours)")
axis(side=1, at=seq(0,2400,200), labels=seq(0,24,2))
axis(side=2, at=seq(0,200,50), labels=seq(0,200,50))

plot(Weekday$interval, Weekday$meanSteps_interval, type = "l"
     , xaxt = "n", yaxt = "n", ylim = c(0,200), main = "Average number of steps during the Weekdays"
     , ylab = "N steps", xlab = "Time (hours)")
axis(side=1, at=seq(0,2400,200), labels=seq(0,24,2))
axis(side=2, at=seq(0,200,50), labels=seq(0,200,50))
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 
<br>
It can be seen that the subject remains slightly more active throught the day during the *Weekend*

<br>
**That is it for peer assignment 1 of Coursera's Reproducible research course from John Hopkins University. This was my first try at a R markdown document, so please feel free to provide me with any feedback or tips concerning my coding and writing skills.**

---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    fig_caption: yes
    keep_md: true
    toc: yes
    pdf_document: default
keep_md: true
self_contained: no    
---
## Packages used

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data
Downloading and unzipping file for processing. Read and load CSV file into a dataframe. 

```r
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile = "./data/activity.zip", method = "curl")

unzip(zipfile = "./data/activity.zip",exdir = "./data")
activity <- read.csv("./data/activity.csv")
activity$date <-as.Date(activity$date)
```

## Data Analysis
### Question 1 : What is mean total number of steps taken per day?
  a. Calculating total number of steps per day

```r
stepsperday <- activity %>% group_by(date) %>%
  summarize(sumsteps = sum(steps, na.rm = TRUE))
```
  b.  Make a histogram of the results

```r
hist(stepsperday$sumsteps, main = "Histogram of Daily Steps", 
     col = "darkblue", xlab = "Steps", ylim = c(0,30))
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->
  
  c. Calculate and report the mean and median total number of steps taken per day

```r
meansteps <- round(mean(stepsperday$sumsteps))
mediansteps <- round(median(stepsperday$sumsteps))
print(paste("The mean is", meansteps))
```

[1] "The mean is 9354"

```r
print(paste("The median is ", mediansteps))
```

[1] "The median is  10395"

### Question 2: What is the average daily activity pattern?
  a.    Make a time series plot of the 5-minute interval (x-axis), and the average number of steps taken, averaged across all days (y-axis)
  

```r
stepsperinterval <- activity %>% group_by(interval) %>%
  summarize(meansteps1 = mean(steps, na.rm = TRUE))

plot(stepsperinterval$meansteps1 ~ stepsperinterval$interval,
     col = "darkgreen", type = "l", xlab = "5 minute intervals",
     ylab = "Average number of steps", main = "Steps By Time Interval")
```

![](PA1_template_files/figure-html/time-series-plot-1.png)<!-- -->

  b. Which 5-minute interval, on average across all days in the dataset, contains the maximum number of steps?

```r
print(paste("The 5-minute interval containing the most steps on average is",
            stepsperinterval$interval[which.max(stepsperinterval$meansteps1)]))
```

[1] "The 5-minute interval containing the most steps on average is 835"

```r
print(paste("Average steps for that interval is", round(max(stepsperinterval$meansteps1))))
```

[1] "Average steps for that interval is 206"

### Question 3: Imputing missing values
  a. Calculate and report the total number of missing values in the dataset. 

```r
print(paste("The total number of rows with missing values is",
            sum(is.na(activity$steps))))
```

[1] "The total number of rows with missing values is 2304"

  b. Devise a strategy for filling all the missing values in the dataset. 
  
  One of the strategies for filling all the values in the dataset is to input the average of the associated interval. This was already calculated in the previous step, and using a loop all NA values can be identified and then transformed by associating the appropriate interval with the "steps" value in the 'stepsperinterval' data frame created in the previous step.   
  
  c. Create a new dataset that is equal to the previous dataset but with all missing values filled in. 
  

```r
activitynoNA <- activity
for (i in 1:nrow(activity)) {
  if(is.na(activity$steps[i])){
    activitynoNA$steps[i] <- stepsperinterval$meansteps1[activitynoNA$interval[i] == stepsperinterval$interval]
  }
  
}
```

  d. Make a histogram of total number of steps taken each day


```r
stepsperday <- activitynoNA %>%
  group_by(date) %>%
  summarize(sumsteps = sum(steps, na.rm = TRUE))

hist(stepsperday$sumsteps, main = "Histogram of Daily Steps",
     col = "lightblue", xlab = "Steps")
```

![](PA1_template_files/figure-html/new-histogram-1.png)<!-- -->

  d. (contd.) Calculate and report the mean and median total number of steps taken each day. Has the value changed from what was initially reported? What is the impact of imputing missing data on the estimates of total number of steps?
  

```r
meanNA <- round(mean(stepsperday$sumsteps), digits = 2)
medianNA <- round(median(stepsperday$sumsteps), digits = 2)

print(paste("The mean is", mean(meanNA)))
```

[1] "The mean is 10766.19"

```r
print(paste("The median is", median(medianNA)))
```

[1] "The median is 10766.19"

### Question 4: Are there differences in activity patterns between weekdays and weekends?

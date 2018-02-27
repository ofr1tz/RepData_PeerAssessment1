---
title: "Reproducible Research: Peer Assessment 1"
author: "ofr1tz"
output: 
  html_document:
    keep_md: true
---

## Loading packages

Let's load the tidyverse package for smoother data transformation and visualisation!


```r
if (!require(tidyverse)) {
      install.packages("tidyverse")
      library(tidyverse)
}
if (!require(lubridate)) {
      install.packages("lubridate")
      library(lubridate)
}
if (!require(xtable)) {
      install.packages("xtable")
      library(xtable)
}
```


## Loading and preprocessing the data

We unzip the provided archive file, read in the comma-separated value file as a tibble and parse the date column.


```r
unzip("activity.zip")
dat <- as.tibble(read.csv("activity.csv")) %>%
    mutate(date=lubridate::ymd(date))
```


## What is the mean total number of steps taken per day?

First, we sum up the steps data grouped by date.


```r
steps_day <- dat %>%
    group_by(date) %>%
    summarise(steps=sum(steps))
```

Then we make a histogram of the total number of steps taken per day.


```r
ggplot(steps_day, aes(x=steps)) + 
    geom_histogram(bins=20) +
    ggtitle("Total number of steps taken per day") +
    theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

We calculate the mean and median of steps taken per day.


```r
mean_steps_day <- format(mean(steps_day$steps, na.rm=TRUE), scientific=FALSE, big.mark=",")
median_steps_day <- format(median(steps_day$steps, na.rm=TRUE), scientific=FALSE, big.mark=",")
```

The **median** total number of steps taken per day is **10,765**. 
At **10,766.19**, the **mean** is very close to the median. This is an indication of a rather symmetrical distribution of the values.


## What is the average daily activity pattern?

We calculate the average number of steps taken per 5-minute interval.


```r
mean_steps_interval <- dat %>%
    group_by(interval) %>%
    summarise(steps=mean(steps, na.rm=TRUE))
```

We plot the average number of steps per interval as a time series.


```r
ggplot(mean_steps_interval, aes(x=interval, y=steps)) +
    geom_line() +
    ggtitle("Average number of steps per 5-minute interval") +
    theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_steps_interval <- mean_steps_interval %>%
    filter(steps == max(steps)) %>% 
    select(interval) 
```

The interval 835 contains the maximum number of steps on the average.

## Imputing missing values

Let's see how many missing values there are in each column of the dataset.


```r
count_na <- dat %>%
    summarise_all(funs(sum(is.na(.))))
print(count_na)
```

```
## # A tibble: 1 x 3
##   steps  date interval
##   <int> <int>    <int>
## 1  2304     0        0
```

There are 2,304 missing value rows in the dataset.  

As the number of steps taken seems to depend largely on the time of the day, we impute missing values by using the mean value for the specific interval across all days.


```r
dat <- dat %>%
    group_by(interval) %>% 
    mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```


Now we make a histogram of the total number of steps taken per day **based on the dataset with imputed data**.


```r
steps_day <- dat %>%
    group_by(date) %>%
    summarise(steps=sum(steps))

ggplot(steps_day, aes(x=steps)) + 
    geom_histogram(bins=20) +
    ggtitle("Total number of steps taken per day (imputed missing values)") +
    theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Then we calculate the mean and median of steps taken per day **based on the dataset with imputed data**.


```r
mean_steps_day <- format(mean(steps_day$steps, na.rm=TRUE), scientific=FALSE, big.mark=",")
median_steps_day <- format(median(steps_day$steps, na.rm=TRUE), scientific=FALSE, big.mark=",")
```

The **median** total number of steps taken per day now is **10,766.19**, and the **mean** is **10,766.19**.  As we have used a mean value for imputation, the mean has not changed, and the median has moved towards the mean.


## Are there differences in activity patterns between weekdays and weekends?

We create a new factor variable in the dataset with two levels -- "weekday" and "weekend".


```r
dat <- dat %>%
    mutate(type=as.factor(ifelse(wday(date)==1 | wday(date)==7, "weekend", "weekday"))) 
```

Let's plot the average number of steps taken per interval for weekdays and weekend.


```r
mean_steps_interval <- dat %>%
    group_by(interval, type) %>%
    summarise(steps=mean(steps, na.rm=TRUE))

ggplot(mean_steps_interval, aes(x=interval, y=steps)) +
    geom_line() +
    ggtitle("Average number of steps per 5-minute interval") +
    facet_grid(type~.) +
    theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
  
It seems that the test person is less active in the morning and more active in the evening on weekends.

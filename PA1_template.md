---
title: 'Johns Hopkins Especialization'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
#Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
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

```r
library(ggplot2)
MyData <- read.csv("activity.csv", header=TRUE, stringsAsFactors = FALSE, sep=",")
print ("Data loaded with read.csv")
```

```
## [1] "Data loaded with read.csv"
```
## What is mean total number of steps taken per day?

```r
NewGroup <- group_by(MyData, date)
NewGroup <- summarise(NewGroup, stepsPerDay = sum(steps, na.rm=TRUE))
print("Steps per day")
```

```
## [1] "Steps per day"
```

```r
ggplot(NewGroup, aes(x = date, y = stepsPerDay, color = I("red"))) + geom_bar(stat = "identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
print(paste("Mean of steps per day", mean(NewGroup$stepsPerDay, na.rm = TRUE)))
```

```
## [1] "Mean of steps per day 9354.22950819672"
```

```r
print(paste("Median of steps per day", median(NewGroup$stepsPerDay, na.rm = TRUE)))
```

```
## [1] "Median of steps per day 10395"
```

```r
ggplot(NewGroup, aes(stepsPerDay, color = I("red")) ) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png)<!-- -->


## What is the average daily activity pattern?


```r
ggplot(MyData, aes(x = interval, y = steps)) + geom_line()
```

```
## Warning: Removed 2 rows containing missing values (geom_path).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?

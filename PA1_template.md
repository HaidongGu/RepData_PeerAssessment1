# Reproducible Research: Peer Assessment 1
Haidong Gu  


## Loading and preprocessing the data

1. set working directory, it will be different for each person

```r
setwd("c:/haidong/github/ExData_Plotting1")
```

1. load data and process the data  


```r
csv_file <- "activity.csv"

if (!file.exists(csv_file)) {
    zip_file <- "activity.zip"
    if (!file.exists(zip_file)) {
        print("there is no data source for this project")
        q()
    }
    unzip(zip_file)
}

activity <- read.csv(file = csv_file, stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date,"%Y-%m-%d")

head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?


```r
daily_activity <- aggregate(formula = steps~date, data = activity, FUN = sum, na.rm=TRUE)
head(daily_activity)
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


## What is the average daily activity pattern?
1  


## Imputing missing values
1  


## Are there differences in activity patterns between weekdays and weekends?
1  

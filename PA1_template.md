# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Step One: Load the data

```r
df <- read.csv("activity.csv")
```

Step Two: Check the data

```r
head(df)
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
df.steps <- tapply(df$steps, df$date, Fun=sum, na.rm=True)
mean(df.steps, na.rm=TRUE)
```

```
## [1] 31
```

```r
median(df.steps, na.rm=TRUE)
```

```
## [1] 31
```

## What is the average daily activity pattern?
Trying to find the average daily activity pattern.  
We need to aggregate data and then plot.  
We are using the library (ggplot2).  
(make sure you have that library installed.)


```r
library(ggplot2)
df.averages <- aggregate(x=list(steps=df$steps), by=list(interval=df$interval), FUN=mean, na.rm=TRUE)
ggplot(data=df.averages, aes(x=interval, y=steps)) + geom_line() + xlab("Intervals set at 5 minutes") + ylab("Average of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

## Imputing missing values
This dataset has many missing values that are coded as NA. The very presence of the missing data may introduce what is known as bias into the data analysis process. We need to take care to address this and carefully impute the data using r. First we identify the number of missing items from the dataframe. Finally, we generate a table to identify the number of missing items in this dataset.


```r
df.missing <- is.na(df$steps)
table(df.missing)
```

```
## df.missing
## FALSE  TRUE 
## 15264  2304
```

We can replace the missing values with the mean value of the 5-minute intervals by using a function that is conditional on the is.na and number of steps. 


```r
na2fill <- function(steps, interval){
  fill <- NA
  if (!is.na(steps))
    fill <- c(steps)
  else
    fill <- (df.averages[df.averages$interval==interval, "steps"])
  return(fill)
}
filled.df <- df
filled.df$steps <- mapply(na2fill, filled.df$steps, filled.df$interval)
```

Calling the created object.


```r
head(filled.df)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Visualization of the filled in data set.


```r
filled_ds <- tapply(filled.df$steps, filled.df$date, FUN=sum)
qplot(filled_ds, binwidth=1000, xlab="Total Number of Steps per Day",main="Total Number of Steps per Day After Imputation")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 


## Are there differences in activity patterns between weekdays and weekends?

We have to look at the day of the week for every single measurement in the data. 
(I have to use cyrillic Weekday names. Feel free to use english ;-))


```r
week.identify <- function(date){
  day <- weekdays(date)
  #if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
  if (day %in% c("понедельник", "вторник", "среда", "четверг", "пятница"))
    return("Weekday")
  else if 
    #(day %in% c("Saturday", "Sunday"))
    (day %in% c("суббота", "воскресенье"))
    return("Weekend")
  else
    stop("Invalid Date")
}  

filled.df$date <- as.Date(filled.df$date)
filled.df$day <- sapply(filled.df$date, FUN=week.identify)
```


Checking, what we have.


```r
head(filled.df$day)
```

```
## [1] "Weekday" "Weekday" "Weekday" "Weekday" "Weekday" "Weekday"
```

Now, we have to visually explore the data that we created.  
Do people take more steps on the weekends or the weekdays? 

```r
avg <- aggregate(steps ~ interval + day, data=filled.df, mean)
ggplot(avg, aes(interval, steps))+geom_line()+ facet_grid(day ~ .) + xlab("Intervals at 5 minutes") + ylab("# of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

From the graph we see that weekday steps start out similar to the weekend steps. 

#Reproducible Research: Peer Assessment 1


##Loading and preprocessing the data

```r
data <- read.csv("activity.csv", header = TRUE)
clean_data <- data[which(data$steps != "NA"), ]
```


##What is mean total number of steps taken per day?

```r
total_per_day <- aggregate(clean_data$steps, by=list(clean_data$date),FUN = sum)
hist(total_per_day$x, main = "Number of Steps", xlab = "Total number of steps taken each day", 
     col = "light blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 


```r
#the mean of the total number of steps taken per day
mean(total_per_day$x)
```

```
## [1] 10766.19
```


```r
#the median of the total number of steps taken per day
median(total_per_day$x)
```

```
## [1] 10765
```


##What is the average daily activity pattern?

```r
#according different interval to compute the average of steps
avgStepsInterval <- tapply(data$steps,data$interval,mean, na.rm=TRUE)
plot(names(avgStepsInterval), avgStepsInterval, type="l", main = "Time Series Plot", xlab="5-minute Intervals", ylab="Avg Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


```r
#compute Which 5-minute interval contains the maximum number of steps
print(paste0("Interval ", as.numeric(names(which.max(avgStepsInterval))) , " contains the maximum number of steps."))
```

```
## [1] "Interval 835 contains the maximum number of steps."
```


##Imputing missing values
###Calculate and report the total number of missing values in the dataset

```r
print(paste0("There are ", sum(is.na(data)) , " missing values in the dataset."))
```

```
## [1] "There are 2304 missing values in the dataset."
```

###a strategy for filling in all of the missing values in the dataset and create a new datase after the missing data filled in

fill the the missing values in the dataset with the mean of 5-minute interval

```r
newdata <- data
meanInterval <-tapply(clean_data$steps, clean_data$interval,mean)
for (i in which(is.na(newdata)))
{
  newdata[i,1] <- meanInterval[((i-1)%%288)+1]
}
```

###Historgram of new newdata

```r
hist(tapply(newdata$steps,newdata$date,sum), main = paste("Histogram of Total Number of Steps Taken per Day"), xlab="Sum of Steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

###the mean and median of the total number of steps taken per day
of the newdata

```r
total_per_day_newdata <- aggregate(newdata$steps, by=list(newdata$date),FUN = sum)
print(paste0("Mean total number of steps taken per day is: ", mean(total_per_day_newdata$x)), digits = 2)
```

```
## [1] "Mean total number of steps taken per day is: 10766.1886792453"
```

```r
print(paste0("Median total number of steps taken per day is: ", median(total_per_day_newdata$x)))
```

```
## [1] "Median total number of steps taken per day is: 10766.1886792453"
```


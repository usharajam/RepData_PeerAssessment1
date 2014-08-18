Assignment 1:


```r
setwd("C:/Users/Amarnath/RepData_PeerAssessment1")
dfActivity <- read.csv("activity.csv")
```

Question 1
-------------


```r
stepsByDay <- aggregate(dfActivity$steps, by = list(date = as.Date(dfActivity$date)), sum)
hist(stepsByDay$x, xlab = "Steps", ylab = "Days", main = "Histogram of steps taken each day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```r
mean(stepsByDay$x, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(stepsByDay$x, na.rm = TRUE)
```

```
## [1] 10765
```

Question 2
----------

Calculate Average Daily Activity Pattern


```r
dfa_withoutNA <- na.omit(dfActivity)
meanStepsByInterval <- aggregate(dfa_withoutNA$steps, FUN ="mean", by = list(dfa_withoutNA$interval), na.action = na.omit)
byInterval <- aggregate(dfa_withoutNA$steps, FUN ="mean", by = list(interval = dfa_withoutNA$interval), na.action = na.omit)

plot(byInterval$interval, 
     byInterval$x, type = "l", 
     main = "Average daily Activity Pattern",
     xlab = "5 Minute Intervals",
     ylab = "Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Finding the Interval which has the maximum Steps


```r
meanStepsByInterval[which.max(meanStepsByInterval$x),1]
```

```
## [1] 835
```

Question 3
----------
Calculate the total Number of missing values in the Data Set

```r
  print("Count of Rows that has NAs:")
```

```
## [1] "Count of Rows that has NAs:"
```

```r
  sum(!complete.cases(dfActivity))
```

```
## [1] 2304
```
Strategy for Replacing NAs. Here NAs in Steps are replaced with Mean steps for each day. Note: if mean is NaN, then it is replaced with 0.


```r
## replace NAs
  require("plyr")
```

```
## Loading required package: plyr
```

```
## Warning: package 'plyr' was built under R version 3.1.1
```

```r
  impute.mean <- function(x) 
    {
     y <- replace(x, is.na(x), mean(x, na.rm = TRUE)) 
     replace(y, is.nan(y), 0)
    
    }
  dfa_imput <- ddply(dfActivity, ~ date, transform, steps = impute.mean(steps))
```

Now, The NAs are replaced with Mean steps for that day, the histogram of total steps taken each day is plotted.


```r
  imputStepsByDay <- aggregate(dfa_imput$steps, 
                          by = list(date = as.Date(dfa_imput$date)), sum)
  hist(imputStepsByDay$x, xlab = "Steps", ylab = "Days", 
       main = "Histogram of steps taken each day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

The mean and median of total steps taken in all days is listed below


```r
  mean(imputStepsByDay$x)
```

```
## [1] 9354
```

```r
  median(imputStepsByDay$x)
```

```
## [1] 10395
```

Mean values changed because I supplied average steps for the corresponding day. Looking at the data there is a day where we got NAs  for all the intervals. So in that case I supplied 0. So, the mean, median and  Histogram are changed somewhat.

Question 4
----------

Create a factor variable with "Weekday" and "Weekend" as values and add that column along with data frame for which we imputed values for NA above.


```r
  library(lattice)
  weekFactor <- sub("Saturday|Sunday", "Weekend", 
                    weekdays(as.Date(dfa_imput$date)))
  weekFactor <- replace(weekFactor, 
                        grep("Weekend", invert = TRUE, weekFactor), "Weekday")
  dfa_imput["week"] <- factor(weekFactor)

  meanByInterval <- ddply(dfa_imput, 
                          .(interval, week), 
                          summarise, 
                          steps=mean(steps))

  xyplot(steps ~ interval | week, 
         data = meanByInterval, layout = c(1, 2), type="l")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

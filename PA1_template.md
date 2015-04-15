---
title: "Act1"
author: "Jojo"
date: "Wednesday, April 15, 2015"
output: html_document
---

```r
setwd("C:\\Users\\Minerva\\Dropbox\\Desk2015\\Rwork\\Rprog\\Reproducible Res\\Assgn1")
act=read.csv("activity.csv")
attach(act)
```

```
## The following object is masked _by_ .GlobalEnv:
## 
##     steps
## 
## The following objects are masked from act (pos = 5):
## 
##     date, interval, steps
```

```r
head(act)
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

```r
summary(act)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

Mean total number of steps

```r
TotalSteps= aggregate(steps ~ date, data = act, sum,na.rm=TRUE)

hist(TotalSteps$steps, main = "Total steps by day", xlab = "day", col = "red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(TotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(TotalSteps$steps)
```

```
## [1] 10765
```

Average daily pattern


```r
cact <- tapply(act$steps, act$interval, mean, na.rm = TRUE)

plot(row.names(cact), cact, type = "l", xlab = "5-min interval", 
    ylab = "Average across all Days", main = "Average number of steps taken", 
    col = "red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
maxint=which.max(cact)
names(maxint)
```

```
## [1] "835"
```

Missing values


```r
actNa=sum(is.na(act))

AverageStep <- aggregate(steps ~ interval, data = act, FUN = mean)
fillNA <- numeric()

for (i in 1:nrow(act)) {
    obs <- act[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(AverageStep, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}

newact=act

newact$steps=fillNA

Total2 <- aggregate(steps ~ date, data = newact, sum, na.rm = TRUE)

hist(Total2$steps, main = "Total steps by day", xlab = "day", col = "red")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
mean(Total2$steps)
```

```
## [1] 10766.19
```

```r
median(Total2$steps)
```

```
## [1] 10766.19
```

Weekdays vs weekends


```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
act$daytype <- as.factor(sapply(act$date, daytype))

par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = act, subset = act$daytype == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 



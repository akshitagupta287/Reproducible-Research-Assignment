---
output:
  html_document: default
  pdf_document: default
---
# My Assignment Submission #

## Hope it turns out fine ##

Here is my code for uploading the data
```{r, cache=TRUE}
Data1=read.csv("activity.csv", header = TRUE, sep = ",")
Data=as.data.frame(Data1)
newdata <- na.omit(Data)
```
Data Processing for Histogram and Plotting
```{r, cache=TRUE}
Dates=as.data.frame(unique(newdata$date))
value=numeric(53)
for(i in 1:53)
{
  value[i]<- sum(newdata[newdata$date==Dates[i,1],1])
}

Steps=as.data.frame(value)


Q1= data.frame(Dates,Steps, stringsAsFactors = FALSE)
cols=c("Date","Steps")
colnames(Q1)=cols
hist(Q1$Steps, main = "Histogram of Total Number of Steps per Day", 
    xlab = "Total Number of Steps per Day", ylab = "Frequency", col = "blue", 
    breaks = 30)
```
<br/> Mean and Median Number of Steps taken each day
```{r, cache=TRUE}
options(scipen=999)
Avg=mean(Q1$Steps)
Med=median(Q1$Steps)
```
Mean number of steps per day is `r Avg`  
Median number of steps per day is `r Med`  
Time series plot of the average number of steps taken
```{r, cache=TRUE}
Intervals=as.data.frame(unique(newdata$interval))
avginterval=numeric(288)
for(i in 1:288)
{
  avginterval[i]<- mean(newdata[newdata$interval==Intervals[i,1],1])
}

AverageInt=as.data.frame(avginterval)
x=Intervals$`unique(newdata$interval)`
y=AverageInt$avginterval
plot(x,y,type = "l", xlab = "5-Minute Interval", ylab = "Average Steps")
```
<br/> The 5-minute interval that, on average, contains the maximum number of steps
```{r, cache=TRUE}
Q2=data.frame(Intervals,AverageInt)
Ans2=Q2[which.max(Q2$avginterval),1]
```
The 5 minute interval which contains maximum number of steps, on average is `r Ans2`  
Code to find number of rows with NA
```{r cache=TRUE}
Q3=subset(Data,is.na(Data$steps))
Q3[is.na(Q3)] <- 0
Ans3=nrow(Q3)
```
The total number of missing values in the dataset (i.e. the total number of rows with NAs) are `r Ans3`  
Code to describe and show a strategy for imputing missing data
```{r, cache=TRUE}
mean(Data$steps, na.rm = T)
Dataimpute <- Data
Dataimpute$steps[is.na(Dataimpute$steps)] <- mean(Dataimpute$steps, na.rm = T)
colSums(is.na(Dataimpute))
```
histogram of the total number of steps taken each day 
``` {r, cache=TRUE}
value2=numeric(53)
for(i in 1:53)
{
  value2[i]<- sum(Dataimpute[Dataimpute$date==Dates[i,1],1])
}

Steps2=as.data.frame(value2)
Q4= data.frame(Dates,Steps2, stringsAsFactors = FALSE)
cols=c("Date","Steps")
colnames(Q4)=cols
hist(Q4$Steps, main = "Histogram of Total Number of Steps per Day", 
    xlab = "Total Number of Steps per Day", ylab = "Frequency", col = "blue", 
    breaks = 30)
```
<br/> mean and median total number of steps taken per day
```  {r, cache=TRUE}
options(scipen=999)
Avg2=mean(Q4$Steps)
Med2=median(Q4$Steps)
```
Mean number of steps per day is `r Avg2`  
Median number of steps per day is `r Med2`  
Are there differences in activity patterns between weekdays and weekends?
```{r cache=TRUE}
Dataimpute$date <- as.Date(Dataimpute$date)
Dataimpute$weekdays <- weekdays(Dataimpute$date)
Dataimpute$weeks[(Dataimpute$weekdays == "Saturday" | Dataimpute$weekdays == "Sunday")] <- "weekend"
Dataimpute$weeks[!(Dataimpute$weekdays == "Saturday" | Dataimpute$weekdays == "Sunday")] <- "weekdays"
```
```{r, cache=TRUE}
library(plyr)
week_comp <- ddply(Dataimpute, c("interval", "weeks"), function(x) apply(x[1], 
    2, mean))
head(week_comp)
```
```{r, cache=TRUE}
library(lattice)
xyplot(steps ~ interval | weeks, data = week_comp, type = "l", xlab = "Interval", 
    ylab = "Number of steps", layout = c(1, 2))
```

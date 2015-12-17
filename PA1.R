
# Loading and preprocessing the data
if (!("activity.zip" %in% dir())){
        download.file('http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip','activity.zip')
}
activity = read.csv(unzip("activity.zip"))
summary(activity)


# What is mean total number of steps taken per day?
## For this part of the assignment, you can ignore the missing values in the dataset.
## 1. Calculate the total number of steps taken per day
total.steps = summarise(group_by(activity,date), sum(steps, na.rm = T))
colnames(total.steps) <- c('date','steps')
## 2. Make a histogram of the total number of steps taken each day
hist(total.steps$steps, main = 'Total Number of Steps by Day', xlab = 'Number of Steps')
## 3. Calculate and report the mean and median of the total number of steps taken per day
mean.total.steps <- mean(total.steps$steps)
median.total.steps <- median(total.steps$steps)

# What is the average daily activity pattern?
## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
##    and the average number of steps taken, averaged across all days (y-axis)
average.steps = summarise(group_by(activity,interval), mean(steps,na.rm = T))
colnames(average.steps) <- c('interval','steps')
plot(average.steps$interval, average.steps$steps
     , type = 'l', xlab = '5 min interval', ylab = 'Avg number of steps')
## 2. Which 5-minute interval, on average across all the days in the dataset, 
##    contains the maximum number of steps?
average.steps$interval[which.max(average.steps$steps)]

# Imputing missing values
## 1. Calculate and report the total number of missing values in the dataset 
sum(!complete.cases(activity))
summary(activity)
## 2. Devise a strategy for filling in all of the missing values in the dataset. 
##    Use the mean for that 5 min interval
## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
is.missing <- !complete.cases(activity)
new.activity <- activity 
new.activity$steps[is.missing] <- 
        average.steps$steps[match(activity$interval[is.missing], average.steps$interval)]
## 4. Make a histogram of the total number of steps taken each day and 
##    Calculate and report the mean and median total number of steps taken per day. 
new.total.steps <- summarise(group_by(new.activity,date), sum(steps, na.rm = T))
colnames(new.total.steps) <- c('date','steps')
hist(new.total.steps$steps, main = 'Total Number of Steps by Day', xlab = 'Number of Steps')
mean.new.total.steps <- mean(new.total.steps$steps)
median.new.total.steps <- median(new.total.steps$steps)
##    Do these values differ from the estimates from the first part of the assignment? 
##    What is the impact of imputing missing data on the estimates of the total daily number of steps?
compare <- data.frame(mean = c(mean.total.steps,mean.new.total.steps), 
                      median = c(median.total.steps,median.new.total.steps))
rownames(compare) <- c('Original Dataset','New Dataset')
compare

# Are there differences in activity patterns between weekdays and weekends?
## 1. Create a new factor variable in the dataset with two levels 
##    – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
new2.activity <- new.activity
wkday <- weekdays(as.Date(new2.activity$date))
wkday[wkday == 'Sunday' | wkday == 'Saturday'] <- 'weekend'
wkday[!wkday == 'weekend'] <- 'weekday'
new2.activity <- cbind(new2.activity,wkday)
## 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
##    and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
average2.steps = summarise(group_by(new2.activity,interval,wkday), mean(steps,na.rm = T))
colnames(average2.steps) <- c('interval','wkday','steps')
xyplot(steps~ interval | wkday, data = average2.steps, layout = c(1,2), type = 'l', ylab = 'Avg number of steps')



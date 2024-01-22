## 1. load the data into Rstudie, already did 'Extract all' in windows libary

library(readxl)
activity <- read.csv("C:/Users/bwr590/Downloads/repdata_data_activity/activity.csv")
View(activity)

summary (activity)

##2.  what is mean total number of steps taken per day?

Steps_a_day <- tapply(activity$steps, activity$date, sum, na.rm=FALSE)
head (Steps_a_day)

library(ggplot2)

qplot (Steps_a_day, xlab= 'total steps a day', ylab= 'Frequency',binwidth = 1000)

mean_steps_a_day <- mean (Steps_a_day, na.rm = TRUE) 

median_Steps_a_day <- median (Steps_a_day, na.rm = TRUE)

##3. What is the average daily activity pattern?

Steps_interval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
plot(steps~interval,data=Steps_interval,type="l")
max(Steps_interval$steps)
Steps_interval[which.max(Steps_interval$steps),]$interval

##4. Imputing missing values

missing_values <- sum(is.na(activity), na.rm = TRUE)
cat("Total number of missing values in the dataset:", missing_values, "\n")

activity_filled <- apply(activity, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
summary(activity_filled)

activity_filled <- transform(activity, steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

total_steps_per_day <- aggregate(steps ~ date, activity_filled, sum)

hist(total_steps_per_day$steps, main = "Histogram of Total Steps per Day", xlab = "Total Steps", col = "skyblue", border = "black")

mean_steps <- mean(total_steps_per_day$steps)
median_steps <- median(total_steps_per_day$steps)

cat("Mean total number of steps per day:", mean_steps, "\n")
cat("Median total number of steps per day:", median_steps, "\n")

##no impact difference after imputing missing data on the estimates of the daily number of steps. This is because I filled the missing data with the mean. 

##5. Are there differences in activity patterns between weekdays and weekends?

activity_filled$date <- as.Date(activity_filled$date)
activity_filled$day_type <- ifelse(weekdays(activity_filled$date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "weekday", "weekend")
activity_filled$day_type <- factor(activity_filled$day_type, levels = c("weekday", "weekend"))
head(activity_filled)

Steps_per_interval <- aggregate(x=list(meanSteps=activity_filled$steps), by=list(interval=activity_filled$interval, day_type=activity_filled$day_type), FUN=mean, na.rm=TRUE)

# Create a panel plot
ggplot(data=Steps_per_interval, aes(x=interval, y=meanSteps)) +
  geom_line(aes(color=day_type)) +
  facet_wrap(~day_type, scales="free_y", ncol=1) +
  xlab("5-Minute Interval") +
  ylab("Average Steps Taken") +
  ggtitle("Average Steps per 5-Minute Interval - Weekday vs. Weekend") +
  theme_minimal()

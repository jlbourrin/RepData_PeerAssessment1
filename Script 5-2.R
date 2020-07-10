library(dplyr)
library(ggplot2)

# Upload data from the internet, save it in dedicated folder "Data", and unzip it
if (!file.exists("Data")){
      dir.create("Data")}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
              destfile = "data/rawdata.zip", method = "curl")
unzip("data/rawdata.zip", exdir = "Data")
activity <- read.csv("data/activity.csv", header = TRUE, na.strings = "NA")


# 1 - What is mean total number of steps taken per day?
# Assumption: missing values in the dataset are ignored

# 1.a - Histogram of the total number of steps taken each day

clean_activity <- activity[!is.na(activity$steps),]
per_day <- clean_activity %>% group_by(date) %>% summarize(steps = sum(steps))
hist(per_day$steps, col = "blue")
# Save plot as PNG file
dev.copy(png, file = "plot-1.png",width= 480, height= 480)
dev.off()

# 1.b - Mean and median total number of steps taken per day
format(mean(per_day$steps), nsmall=1, big.mark=",")
format(median(per_day$steps), nsmall=1, big.mark=",")

# 2 - What is the average daily activity pattern?

# 2.a - Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and
#       the average number of steps taken, averaged across all days (y-axis)

per_interval <- clean_activity %>% group_by(interval) %>% summarize(avg_steps = mean(steps))
plot(per_interval, type ="l", col = "darkred", lwd = 2,
     main = "Average daily activity pattern",
     sub = "Daily observations from 10/01/2012 to 11/30/2012", 
     xlab = "5-minute intervals",
     ylab ="Average nb of steps")
# Save plot as PNG file
dev.copy(png, file = "plot-2.png",width= 480, height= 480)
dev.off()


# 2.b - Which 5-minute interval, on average across all the days in the dataset,
#       contains the maximum number of steps?

per_interval$interval[which(per_interval$avg_steps == max(per_interval$avg_steps))]

# 3 - Dealing with missing values

# 3.a - Total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity$steps))

# 3.b - Devise a strategy for filling in all of the missing values in the dataset.
#       The strategy does not need to be sophisticated. For example, you could use 
#       the mean/median for that day, or the mean for that 5-minute interval, etc.

# Method = use the average nb of steps per day for each 5-minute interval
# Data required to fill in the NA gaps are already computed/stored in per_interval

# 3.c - Create a new dataset that is equal to the original dataset but with the missing data
#       filled in.
activity_new <- activity
activity_new$steps[is.na(activity_new$steps)] <- per_interval$avg_steps[match(activity_new$interval,per_interval$interval)]

# 3.d - Make a histogram of the total number of steps taken each day and calculate and report 
#       the mean and median total number of steps taken per day.
#       Do these values differ from the estimates from the first part of the assignment?
#       What is the impact of imputing missing data on the estimates of the total daily number of steps?

per_day2 <- activity_new %>% group_by(date) %>% summarize(steps = sum(steps))
par(mfcol = c(1,2))
hist(per_day$steps, col = "blue", main = "Histogram ignoring missing data")
hist(per_day2$steps, col = "green", main = "Histogram with filled-in data")
# Save plot as PNG file
dev.copy(png, file = "plot-3.png",width= 480, height= 480)
dev.off()

format(mean(per_day2$steps), nsmall=1, big.mark=",")
format(median(per_day2$steps), nsmall=1, big.mark=",")


# 4 - Are there differences in activity patterns between weekdays and week- ends?
#     For this part the weekdays() function may be of some help here.
#     Use the dataset with the filled-in missing values for this part.


# 4.a - Create a new factor variable in the dataset with two levels – “weekday”
#       and “weekend” indicating whether a given date is a weekday or weekend day.

weekend <- c("Saturday", "Sunday")

convert_date <- function(x) {
      x <- weekdays(as.Date(x))
      if(x %in% weekend) {
            day <- "weekend"
      } else {
            day <- "weekday"
      }
}

day_type <- matrix(unlist(lapply(activity_new$date, convert_date)))
activity_new <- cbind(activity_new,day_type)
activity_new$day_type <- factor(activity_new$day_type)


# 4.b - Make a panel plot containing a time series plot (i.e. type = "l") 
#       of the 5-minute interval (x-axis) and the average number of steps taken, 
#       averaged across all weekday days or weekend days (y-axis).
per_interval2 <- activity_new %>% group_by(interval, day_type) %>% summarize(avg_steps = mean(steps))
p <- ggplot(per_interval2, aes(x=interval,y=avg_steps)) + geom_line() + facet_grid(.~day_type)
print(p)
# Save plot as PNG file
dev.copy(png, file = "plot-4.png",width= 480, height= 480)
dev.off()
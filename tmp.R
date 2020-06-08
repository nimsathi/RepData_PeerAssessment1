library(dplyr)
library(lubridate)
library(ggplot2)

# load the data
data <- read.csv(unzip('activity.zip'), header=TRUE)
data$date <- ymd(data$date)
str(data)

# total number of steps taken per day
stepsPerDay <- data %>%
  na.omit() %>%
  group_by(date) %>%
  summarize(steps = sum(steps))

# histogram of total steps taken each day
g <- ggplot(data=stepsPerDay, aes(steps)) + 
  geom_histogram(binwidth=1000, fill="#0072B2", color="black") + 
  labs(x="Total steps", y="Count", title="Frequency of total steps")
g

meanStepsPerDay <- mean(stepsPerDay$steps)
medianStepsPerDay <- median(stepsPerDay$steps)


# average daily activity pattern
stepsPerInterval <- data %>%
  na.omit() %>%
  group_by(interval) %>%
  summarize(averageSteps = mean(steps))

# time series plot
g <- ggplot(data = stepsPerInterval, aes(x=interval, y=averageSteps)) + 
  geom_line(color="#009E73") + 
  labs(x="Interval", y="Average steps", title="Average steps per 5 min interval")
g

# determine max average steps and the interval in which it occured
maxAverageSteps <- stepsPerInterval$averageSteps[which.max(stepsPerInterval$averageSteps)]
maxInterval <- stepsPerInterval$interval[which.max(stepsPerInterval$averageSteps)]


# number of incomplete records
numIncompleteObservations <- sum(!complete.cases(data))

# impute missing data

meanForInterval <- function(intervalId) {
  filter(stepsPerInterval, interval == intervalId)$averageSteps
}

data2 <- data %>%
  group_by(date) %>%
  mutate(steps = ifelse(is.na(steps), meanForInterval(interval), steps))

stepsPerDay2 <- data2 %>%
  group_by(date) %>%
  summarize(steps = sum(steps))

g <- ggplot(data = stepsPerDay2, aes(x=steps)) + 
  geom_histogram(binwidth=1000, fill="#0072B2", color="black")
g

meanStepsPerDay2 <- mean(imputedStepsPerDay$steps)
medianStepsPerDay2 <- median(imputedStepsPerDay$steps)

# are there differences in activity patterns between weekdays and weekends?
data2 <- data2 %>%
  mutate(type = ifelse(wday(date, label=TRUE) %in% c('Sat','Sun'), 'weekend', 'weekday'))
data$type <- as.factor(data$type)

stepsPerInterval2 <- data2 %>%
  group_by(type, interval) %>%
  mutate(averageSteps = mean(steps))

g <- ggplot(data=stepsPerInterval2,aes(x=stepsPerInterval2$interval, y=stepsPerInterval2$averageSteps, color=type)) +
  geom_line() + 
  labs(x="Interval", y="Average steps", title="Average steps per 5 min interval") + 
  facet_wrap(~ type, nrow = 2, ncol=1)
g


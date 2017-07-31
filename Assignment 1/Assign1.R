

#1. Code for reading in the dataset and/or processing the data

activity <- read.csv("D:\\Training\\DataSci\\Reproducible Research\\Assignment 1\\Activity\\activity.csv")
head(activity)


activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
str(activity)


#2. Histogram of the total number of steps taken each day
actbyDate <-  aggregate(steps ~ date, activity, sum)
with(actbyDate,hist(steps,col="light blue",breaks=10,main="# of Steps per Day",xlab="# of steps"))



#3. Mean and median number of steps taken each day

mediansteps <- median(actbyDate$steps,na.rm=TRUE)
avgsteps <- mean(actbyDate$steps,na.rm=TRUE)
avgsteps

#4. Time series plot of the average number of steps taken

avgActbyInterval <- aggregate(steps ~ interval, activity, mean,na.rm=TRUE)


library(ggplot2)
ggplot(avgActbyInterval,aes(x=interval,y=steps,group=0)) +
  geom_line(col="light blue", size=1) + 
  labs(title="Avg # of steps by interval",x="Daily interval",y="Avg # of steps") + 
  theme_bw()
#5. The 5-minute interval that, on average, contains the maximum number of steps

maxnumberofsteps <- round(max(avgActbyInterval$steps),digits=0)
intervalmax <- avgActbyInterval[which.max(avgActbyInterval$steps),]


#6. Code to describe and show a strategy for imputing missing data

sum(is.na(activity))

summary(activity)


library(data.table)
activityFilled <- activity

# Merge with the lookup df (avgActbyInternal)
activityFilled <- merge(avgActbyInterval,activityFilled, by="interval")

#filling NAs with the reference values
for(i in 1:nrow(activityFilled)) {if (is.na(activityFilled[i,]$steps.y)) {activityFilled[i,]$steps.y = activityFilled[i,]$steps.x }}

#Removing the extra column
activityFilled <- subset(activityFilled,select=-steps.x)
#renaming the steps.y column back to steps
colnames(activityFilled)<-c("interval","steps","date")


summary(activityFilled)
#7. Histogram of the total number of steps taken each day after missing values are imputed

actFilledbyDate <-  aggregate(steps ~ date, activityFilled, sum)
with(actFilledbyDate,hist(steps,col="blue",breaks=10,main="# of Steps per Day with NAs imputed",xlab="# of steps"))


medianstepsF <- median(actFilledbyDate$steps,na.rm=TRUE)
avgstepsF <- mean(actFilledbyDate$steps,na.rm=TRUE)

#8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

# My locale is in Spanish and R Studio doesn't allow me to change Sys.setlocale("LC_TIME", "en_US") or other similar locale value
# 

weekend <- c("Saturday","Sunday")
dayofweek <- weekdays(activityFilled$date)

for (i in 1:length(dayofweek)){
  if (dayofweek[i] %in% weekend){
    dayofweek[i] = "weekend"
  } else {
    dayofweek[i] = "weekday"
  }
}

# Append the vector to the dataframe
activityFilledWeek <- cbind(activityFilled,dayofweek)

str(activityFilledWeek)
#9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
#Averages are going to be calculated separately. First subsetting
actFilledweekday <- subset(activityFilledWeek, dayofweek=="weekday")
actFilledweekend <- subset(activityFilledWeek, dayofweek=="weekend")

#Average calculation
avgActFilledweekdaybyInterval <- aggregate(steps ~ interval, actFilledweekday, mean,na.rm=TRUE)
#Dayofweek=weekday is added
dayofweek<-"weekday"
avgActFilledweekdaybyInterval <- cbind(avgActFilledweekdaybyInterval,dayofweek)

avgActFilledweekendbyInterval <- aggregate(steps ~ interval, actFilledweekend, mean,na.rm=TRUE)
#Dayofweek=weekend is added
dayofweek <- "weekend"
avgActFilledweekendbyInterval <- cbind(avgActFilledweekendbyInterval,dayofweek)

#The 2 dataframes are merged together
avgActFilledbyInterval <-rbind(avgActFilledweekdaybyInterval,avgActFilledweekendbyInterval)


ggplot(avgActFilledbyInterval, aes(x=interval, y=steps,color=dayofweek)) + 
  geom_line() +
  facet_wrap(~dayofweek, nrow=2, ncol=1) +
  ggtitle("Avg # of Steps comparison - Weekday vs. Weekend")+
  labs(x="Interval", y="Avg # of Steps") +
  theme_bw()+theme(legend.position="none") #no background and legend
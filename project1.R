# Libraries
library(ggplot2)

# 1.  Code for reading in the dataset and/or processing the data
    if(!file.exists("./data/activity.csv")) {
        actData <- read.csv("./data/activity.csv", as.is = TRUE)
        dim(actData)
        names(actData)
        head(actData)
        str(actData)
        sum(is.na(actData$steps))/dim(actData)[[1]]
        
        # Data without NA values
        realAct <- actData[complete.cases(actData), ]
        summary(realAct)
    }

#2.  Histogram of the total number of steps taken each day
    #  Getting the steps per day by using tapply.
    #  The data does not have NAs values.
    stepsPerDay <- with(realAct, tapply(steps, date, FUN=sum))
    hist(stepsPerDay, main = "Total number of steps each day", xlab = "Total number of steps")

#  3.  Mean and median number of steps taken each day
    summary(stepsPerDay)
    meanSteps = round(mean(stepsPerDay, na.rm = TRUE))
    medianSteps = median(stepsPerDay, na.rm = TRUE)
    print (paste("Mean number of steps taken each day is ", 
                 meanSteps, 
                 " and the median number of steps taken each day is  ", 
                 medianSteps
                 )
           )

#  4.  Time series plot of the average number of steps taken
    # Average steps per interval.
    aggInterval <- aggregate(steps ~ interval, realAct, mean)
    
    # Plot the time series with appropriate labels and heading
    plot(aggInterval$interval, 
         aggInterval$steps, 
         type='l', 
         col=1, 
         main="Average number of steps taken", 
         xlab="Time Intervals", 
         ylab="Average number of steps"
    )

#5.  The 5-minute interval that, on average, contains the maximum number of steps

    # Identify the 5-minute interval index.
    intIndex <- which.max(aggInterval$steps)
    
    # Identify the specific interval and the average steps for that interval
    print (paste("The interval with the highest average steps is ", 
                 aggInterval[intIndex, ]$interval, 
                 ".  The maximum number of steps based on that interval is ", 
                 round(aggInterval[intIndex, ]$steps, 
                       digits = 1
                       )
                 )
           )
    

# 6.  Code to describe and show a strategy for imputing missing data
    # Calculate the number of rows with missing values. Also done in step #1
    dataNA <- actData[!complete.cases(actData), ]
    nrow(dataNA)
    
    # Loop thru all the rows of activity, find the one with NA for steps.
    # For each identify the interval for that row
    # Then identify the avg steps for that interval in avg_steps_per_interval
    # Substitute the NA value with that value
    
#7. Histogram of the total number of steps taken each day after missing 
    # values are imputed
    for (i in 1:nrow(actData)) {
        if(is.na(actData$steps[i])) {
            val <- aggInterval$steps[which(aggInterval$interval == actData$interval[i])]
            actData$steps[i] <- val 
        }
    }
    
    # Aggregate the steps per day with the imputed values
    imputeSteps <- aggregate(steps ~ date, actData, sum)
    
    # Draw a histogram of the value 
    hist(imputeSteps$steps, 
        col="blue",
        main = "Histogram of total number of steps per day (Imputed)", 
        xlab = "Steps per day")
   
    
    # Compute the mean and median of the imputed value
    # Calculate the mean and median of the total number of steps taken per day
    round(mean(imputeSteps$steps))
    ## [1] 10766
    median(imputeSteps$steps)
    ## [1] 10766.19 
    
#8. Panel plot comparing the average number of steps taken per 5-minute 
    # interval across weekdays and weekends
    day <- weekdays(as.Date(actData$date))
    daylevel <- vector()
    for (i in 1:nrow(actData)) {
        if (day[i] == "Saturday") {
            daylevel[i] <- "Weekend"
        } else if (day[i] == "Sunday") {
            daylevel[i] <- "Weekend"
        } else {
            daylevel[i] <- "Weekday"
        }
    }
    actData$daylevel <- daylevel
    actData$daylevel <- factor(actData$daylevel)
    
    stepsByDay <- aggregate(steps ~ interval + daylevel, data = actData, mean)
    names(stepsByDay) <- c("interval", "daylevel", "steps")
    
    #Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:
        
    library(lattice)   
     xyplot(steps ~ interval | daylevel, 
            stepsByDay, 
            type = "a", 
            grid = TRUE,
            layout = c(2, 1), 
            xlab = "Interval",
            ylab = "Number of steps")



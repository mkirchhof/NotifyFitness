# Contains functions to import files included in a Notfiy & Fitness backup
# Author: Michael Kirchhof
# Created: 10.05.20
#
# USAGE:
# Export a backup in Notify & Fitness in the settings. Hand over the returned
# "backup.nak" file to readBackup:
# readBackup(file = "backup.nak")
# Or, you can use functions to read all (or a single) files of a specific type:
# readSteps(folder = ".")
# readStepsSingle(file = "logReportSteps0.bak")
# (for all available file types, just check out the function defined below)
#
# NOTE:
# Created for R 3.6.3 and Notify & Fitness 9.0.6 for Android 8.0.0 on a Mi Band 3
# Some features, such as WorkoutData, are not implemented yet, because I did
# not need them. Feel free to commit :)

library(rjson)

# jsonToMatrix - reads a .bak file and turns it into a matrix
jsonToMatrix <- function(file){
  raw <- fromJSON(file = file)
  
  # Not all entries must have all data, so see what is available first:
  allTypes <- unique(as.character(unlist(sapply(raw[[1]], names))))
  
  df <- t(sapply(raw[[1]], function(x){
    x <- unlist(x)
    x <- x[allTypes]
    names(x) <- allTypes
    return(x)
  }))
  
  return(df)
}

# numericToTime - Turns the numeric time values into posixlt times
# CAUTION: Not sure if timezones are correct
numericToTime <- function(x){
  as.POSIXlt(as.numeric(x) %/% 1000, origin = "1970-01-01")
}

# readWorkout - reads a single workout file and returns a dataframe
readWorkoutSingle <- function(file = "logReportWorkout.bak"){
  df <- as.data.frame(jsonToMatrix(file), stringsAsFactors = FALSE)
  
  # type conversions
  df$endDateTime <- numericToTime(df$endDateTime)
  df$startDateTime <- numericToTime(df$startDateTime)
  df$heartAvg <- as.numeric(df$heartAvg)
  df$steps <- as.numeric(df$steps)
  df$xCalories <- as.numeric(df$xCalories)
  df$xDistance <- as.numeric(df$xDistance)
  df$xMinutesPause <- as.numeric(df$xMinutesPause)
  
  # Lengths of activities are computed falsely. Recalculate them:
  df$minutes <- as.numeric(difftime(df$endDateTime, df$startDateTime, units = "secs"))
  
  # Rename the activities (INCOMPLETE)
  df$type <- paste0("activity", df$type)
  df$type[df$type == "activity12"] <- "biking"
  df$type[df$type == "activity15"] <- "gymnastics"
  df$type[df$type == "activity55"] <- "jogging"
  
  # Rename some columns, because data is given in seconds, not minutes
  names(df)[names(df) == "minutes"] <- "lengthSeconds"
  names(df)[names(df) == "xMinutesPause"] <- "pauseSeconds"
  names(df)[names(df) == "startDateTime"] <- "startTime"
  names(df)[names(df) == "endDateTime"] <- "endTime"
  names(df)[names(df) == "xCalories"] <- "calories"
  names(df)[names(df) == "xDistance"] <- "distance"
  
  # sort by time
  df <- df[order(df$startTime), ]
  
  return(df)
}

# readWorkoutData - TODO
# readWorkoutData <- function(file = "logReportWorkoutData.bak"){
#   
# }

# readHeartSingle - reads a single heart data file and returns a dataframe
readHeartSingle <- function(file = "logReportHeart0.bak"){
  df <- as.data.frame(jsonToMatrix(file), stringsAsFactors = FALSE)
  
  # type conversions
  df$hidden <- df$hidden == "true"
  df$intensity <- as.numeric(df$intensity)
  df$isActivityValue <- df$isActivityValue == "true"
  df$isWorkout <- df$isWorkout == "true"
  df$syncedGFit <- as.numeric(df$syncedGFit)
  df$timestamp <- numericToTime(df$timestamp)
  
  # Rename some columns for consistency
  names(df)[names(df) == "intensity"] <- "heartRate"
  names(df)[names(df) == "hidden"] <- "isHidden"
  names(df)[names(df) == "timestamp"] <- "time"
  
  # sort by time
  df <- df[order(df$time), ]
  
  return(df)
}

# readSleepSingle - reads a single sleep report file and returns a dataframe
readSleepSingle <- function(file = "logReportSleep.bak"){
  df <- as.data.frame(jsonToMatrix(file), stringsAsFactors = FALSE)
  
  # type conversions:
  df$endDateTime <- numericToTime(df$endDateTime)
  df$heartRateAvg <- as.numeric(df$heartRateAvg)
  df$startDateTime <- numericToTime(df$startDateTime)
  df$totalMinutes <- as.numeric(df$totalMinutes)
  df$totalNREM <- as.numeric(df$totalNREM)
  df$totalREM <- as.numeric(df$totalREM)
  df$userModified <- df$userModified == "true"
  
  # Change minutes to seconds for consistency
  df$totalMinutes <- df$totalMinutes * 60
  df$totalNREM <- df$totalNREM * 60
  df$totalREM <- df$totalREM * 60
  
  # Rename some columns for consistency
  names(df)[names(df) == "totalMinutes"] <- "lengthSeconds"
  names(df)[names(df) == "totalNREM"] <- "lengthNonREMSeconds"
  names(df)[names(df) == "totalREM"] <- "lengthREMSeconds"
  names(df)[names(df) == "heartRateAvg"] <- "heartRateAvg_BROKEN"
  names(df)[names(df) == "userModified"] <- "isUserModified"
  names(df)[names(df) == "startDateTime"] <- "startTime"
  names(df)[names(df) == "endDateTime"] <- "endTime"
  
  # sort by time
  df <- df[order(df$startTime), ]
  
  return(df)
}

# readSleepDaySingle - reads a sleep file aggregated by day and returns a dataframe
readSleepDaySingle <- function(file = "logReportSleepDay.bak"){
  df <- as.data.frame(jsonToMatrix(file), stringsAsFactors = FALSE)
  
  # type conversions:
  df$dayDate <- numericToTime(df$dayDate)
  df$endDateTime <- numericToTime(df$endDateTime)
  df$heartRateAvg <- as.numeric(df$heartRateAvg)
  df$startDateTime <- numericToTime(df$startDateTime)
  df$totalMinutes <- as.numeric(df$totalMinutes)
  df$totalNREM <- as.numeric(df$totalNREM)
  df$totalREM <- as.numeric(df$totalREM)
  df$userModified <- df$userModified == "true"
  
  # Change minutes to seconds for consistency
  df$totalMinutes <- df$totalMinutes * 60
  df$totalNREM <- df$totalNREM * 60
  df$totalREM <- df$totalREM * 60
  
  # Rename some columns for consistency
  names(df)[names(df) == "totalMinutes"] <- "lengthSeconds"
  names(df)[names(df) == "totalNREM"] <- "lengthNonREMSeconds"
  names(df)[names(df) == "totalREM"] <- "lengthREMSeconds"
  names(df)[names(df) == "heartRateAvg"] <- "heartRateAvg_BROKEN"
  names(df)[names(df) == "userModified"] <- "isUserModified"
  names(df)[names(df) == "startDateTime"] <- "startTime"
  names(df)[names(df) == "endDateTime"] <- "endTime"
  names(df)[names(df) == "dayDate"] <- "time"
  
  # sort by time
  df <- df[order(df$startTime), ]
  
  return(df)
}

# readSleepIntervalSingle - reads a single file with single sleep intervals
#                           and returns a dataframe
readSleepIntervalSingle <- function(file = "logReportSleepInterval0.bak"){
  df <- as.data.frame(jsonToMatrix(file), stringsAsFactors = FALSE)
  
  # type conversions:
  df$endDateTime <- numericToTime(df$endDateTime)
  df$heartRateAvg <- as.numeric(df$heartRateAvg)
  df$startDateTime <- numericToTime(df$startDateTime)
  
  # rename sleep types (INCOMPLETE):
  df$type <- paste0("sleeptype", df$type)
  df$type[df$type == "sleeptype4"] <- "light"
  df$type[df$type == "sleeptype5"] <- "deep"
  df$type[df$type == "sleeptype7"] <- "awake"
  
  # rename columns for consistency
  names(df)[names(df) == "startDateTime"] <- "startTime"
  names(df)[names(df) == "endDateTime"] <- "endTime"
  
  # sort by time
  df <- df[order(df$startTime), ]
  
  return(df)
}

# readStepsSingle - reads a single steps file and returns a dataframe
readStepsSingle <- function(file = "logReportSteps0.bak"){
  df <- as.data.frame(jsonToMatrix(file), stringsAsFactors = FALSE)
  
  # type conversions:
  df$dateTime <- numericToTime(df$dateTime)
  df$hidden <- df$hidden == "true"
  df$last <- df$last == "true"
  df$steps <- as.numeric(df$steps)
  df$syncedGFit <- as.numeric(df$syncedGFit)
  
  # rename columns for consistency
  names(df)[names(df) == "hidden"] <- "isHidden"
  names(df)[names(df) == "last"] <- "isLast"
  names(df)[names(df) == "dateTime"] <- "time"
  
  # sort by time
  df <- df[order(df$time), ]
  
  return(df)
}

# readWeightSingle - reads a single weight file and returns a dataframe
readWeightSingle <- function(file = "logReportWeight0.bak"){
  df <- as.data.frame(jsonToMatrix(file), stringsAsFactors = FALSE)
  
  # type conversions:
  df$syncedGFit <- as.numeric(df$syncedGFit)
  df$timestamp <- numericToTime(df$timestamp)
  df$value <- as.numeric(df$value)
  
  # rename columns for consistency
  names(df)[names(df) == "timestamp"] <- "time"
  names(df)[names(df) == "value"] <- "weight"
  
  # sort by time
  df <- df[order(df$time), ]
  
  return(df)
}

# readHeart - reads all heart reports in a folder and returns a dataframe
readHeart <- function(folder = "."){
  # Get a list of all heart files available
  allFiles <- list.files(folder, full.names = TRUE)
  heartFiles <- allFiles[grepl("logReportHeart[[:digit:]]+\\.bak$", allFiles)]
  
  if(is.null(heartFiles) || length(heartFiles) == 0){
    warning("No heart rate files found. Returning NULL")
    return(NULL)
  }
  
  # read and append the heartFiles
  dataList <- lapply(heartFiles, readHeartSingle)
  df <- do.call(rbind, dataList)
  
  # sort by time
  df <- df[order(df$time), ]
  
  return(df)
}

# readSleep - reads the sleep file in a folder and returns a dataframe
readSleep <- function(folder = "."){
  # Get a list of all sleep files available
  allFiles <- list.files(folder, full.names = TRUE)
  sleepFiles <- allFiles[grepl("logReportSleep\\.bak$", allFiles)]
  
  if(is.null(sleepFiles) || length(sleepFiles) == 0){
    warning("No sleep files found. Returning NULL")
    return(NULL)
  }
  
  if(length(sleepFiles) > 1){
    warning("More than one sleep file found. Returning only the first.")
    sleepFiles <- sleepFiles[1]
  }
  
  return(readSleepSingle(sleepFiles))
}

# readSleepDay - reads the sleep per day file in a folder and returns a dataframe
readSleepDay <- function(folder = "."){
  # Get a list of all sleep day files available
  allFiles <- list.files(folder, full.names = TRUE)
  sleepFiles <- allFiles[grepl("logReportSleepDay\\.bak$", allFiles)]
  
  if(is.null(sleepFiles) || length(sleepFiles) == 0){
    warning("No sleep files found. Returning NULL")
    return(NULL)
  }
  
  if(length(sleepFiles) > 1){
    warning("More than one sleep day file found. Returning only the first.")
    sleepFiles <- sleepFiles[1]
  }
  
  return(readSleepDaySingle(sleepFiles))
}

# readSleepInterval - reads all sleep interval files in a folder and returns a dataframe
readSleepInterval <- function(folder = "."){
  # Get a list of all sleep interval files available
  allFiles <- list.files(folder, full.names = TRUE)
  sleepFiles <- allFiles[grepl("logReportSleepInterval[[:digit:]]+\\.bak$", allFiles)]
  
  if(is.null(sleepFiles) || length(sleepFiles) == 0){
    warning("No sleep interval files found. Returning NULL")
    return(NULL)
  }
  
  # read and append the sleepFiles
  dataList <- lapply(sleepFiles, readSleepIntervalSingle)
  df <- do.call(rbind, dataList)
  
  # sort by time
  df <- df[order(df$startTime), ]
  
  return(df)
}

# readSteps - reads all step files in a folder and returns a dataframe
readSteps <- function(folder = "."){
  # Get a list of all steps files available
  allFiles <- list.files(folder, full.names = TRUE)
  stepFiles <- allFiles[grepl("logReportSteps[[:digit:]]+\\.bak$", allFiles)]
  
  if(is.null(stepFiles) || length(stepFiles) == 0){
    warning("No step files found. Returning NULL")
    return(NULL)
  }
  
  # read and append the stepFiles
  dataList <- lapply(stepFiles, readStepsSingle)
  df <- do.call(rbind, dataList)
  
  # sort by time
  df <- df[order(df$time), ]
  
  return(df)
}

# readWeight - reads all weight files in a folder and returns a dataframe
readWeight <- function(folder = "."){
  # Get a list of all weight files available
  allFiles <- list.files(folder, full.names = TRUE)
  weightFiles <- allFiles[grepl("logReportWeight[[:digit:]]+\\.bak$", allFiles)]
  
  if(is.null(weightFiles) || length(weightFiles) == 0){
    warning("No weight files found. Returning NULL")
    return(NULL)
  }
  
  # read and append the weightFiles
  dataList <- lapply(weightFiles, readWeightSingle)
  df <- do.call(rbind, dataList)
  
  # sort by time
  df <- df[order(df$time), ]
  
  return(df)
}

# readWorkout - reads the workout file in a folder and returns a dataframe
readWorkout <- function(folder = "."){
  # Get a list of all workout files available
  allFiles <- list.files(folder, full.names = TRUE)
  workoutFiles <- allFiles[grepl("logReportSleepDay\\.bak$", allFiles)]
  
  if(is.null(workoutFiles) || length(workoutFiles) == 0){
    warning("No workout found. Returning NULL")
    return(NULL)
  }
  
  if(length(workoutFiles) > 1){
    warning("More than one workout file found. Returning only the first.")
    workoutFiles <- workoutFiles[1]
  }
  
  return(readSleepDaySingle(workoutFiles))
}

# readBackup - reads a ".bak" backup and returns a list of dataframes containing
#              all data inside it
readBackup <- function(file = "backup.nak"){
  # unzip the backup
  tdir <- tempdir()
  unzip(zipfile = file, exdir = tdir)
  
  return(list(
    "heart" = readHeart(tdir),
    "sleep" = readSleep(tdir),
    "sleepDay" = readSleepDay(tdir),
    "sleepInterval" = readSleepInterval(tdir),
    "steps" = readSteps(tdir),
    "weight" = readWeight(tdir),
    "workout" = readWorkout(tdir)
  ))
}

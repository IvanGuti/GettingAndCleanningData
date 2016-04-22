# GettingAndCleanningData
Coursera Project

The script run_analysis.r will follow the next instructions:

You can download the script and obtain TidyDataset2.txt , the cleaned output.

Load necessary packages and unzip the archives in the working directory. I use function for sctructure the exercise.

library(dplyr)
library(tidyr)


# 1-Merges the training and the test sets to create one data set.

Tree functions, for SubjectFunction, Xfunction (x_test, x_train) and Yfunction (y_test, y_train) converted in variables, and the final merge.

SubjectFunction <- function() {
  SubjTest <- read.table('./UCI HAR Dataset/test/subject_test.txt')
  SubjTrain <- read.table('./UCI HAR Dataset/train/subject_train.txt')
  SubjFull <- rbind(SubjTrain, SubjTest)
  names(SubjFull) <- "subject"
  SubjFull
}
.
XFunction <- function() {
  XTest <- read.table('./UCI HAR Dataset/test/X_test.txt')
  XTrain <- read.table('./UCI HAR Dataset/train/X_train.txt')
  XFull  <- rbind(XTrain, XTest)
}

YFunction <- function() {
  YTest <- read.table('./UCI HAR Dataset/test/y_test.txt')
  YTrain <- read.table('./UCI HAR Dataset/train/y_train.txt')
  TFull  <- rbind(YTrain, YTest)
}

Subjectdata <- SubjectFunction()
Xdataset <- XFunction()
Ydataset <- YFunction()

# Merge in one set
dataCombine <- cbind(Subjectdata, Xdataset)
Data <- cbind(Ydataset, dataCombine) 

#str(Subjectdata)
#data.frame':	10299 obs. of  1 variable:
#$ subject: int  1 1 1 1 1 1 1 1 1 1 ...
 
 #> str(Ydataset)
#data.frame':	10299 obs. of  1 variable:
 #$ activity: Factor w/ 6 levels "LAYING","SITTING",..: 3 3 3 3 3 3 3 3 3 3 ...
 
 #str(Xdataset)
 #data.frame':	10299 obs. of  561 variables:
 #$ V1  : num  0.289 0.278 0.28 0.279 0.277 ...
 
 #> str(Data)
#'data.frame':	10299 obs. of  563 variables:

2)Extracts only the measurements on the mean and standard deviation for each measurement.

#The function select features.txt and extract the indicated measurements, filtering the data and getting in a XFiltered variable

Measurements <- function() {
  features <- read.table('./UCI HAR Dataset/features.txt', header=FALSE, col.names=c('id', 'name'))
  SelectedFeatures <- grep('mean\\(\\)|std\\(\\)', features$name)
  names(SelectedFeatures) <- gsub("mean", "Mean", names(SelectedFeatures)) # capitalize M
  names(SelectedFeatures) <- gsub("std", "Std", names(SelectedFeatures)) # capitalize S
  names(SelectedFeatures) <- gsub("-", "", names(SelectedFeatures)) # remove "-" in column names 
  FilteredDataset <- Xdataset[, SelectedFeatures]
  names(FilteredDataset) <- features[features$id %in% SelectedFeatures, 2]
  FilteredDataset
}
XFiltered <- Measurements()

#str(FullDatasetMeasurements)
'data.frame':	10299 obs. of  68 variables:

3)Uses descriptive activity names to name the activities in the data set

ActivityLabels <- read.table('./UCI HAR Dataset/activity_labels.txt', header=FALSE, col.names=c('id', 'name'))

# str(ActivityLabels)
#data.frame':	6 obs. of  2 variables:
# $ id  : int  1 2 3 4 5 6
# $ name: Factor w/ 6 levels "LAYING","SITTING",..: 4 6 5 2 3 1

4)Appropriately labels the data set with descriptive activity names.

Ydataset[, 1] = ActivityLabels[Ydataset[, 1], 2]
names(Ydataset) <- "activity"

5 a) Intermediate dataset with required measurements.


#I made a intermediate step for a data frame with the required info before the final step.

FullDatasetMeasurements <- cbind(Subjectdata, Ydataset, XFiltered)
write.table(FullDatasetMeasurements, "./UCI HAR Dataset/FullDatasetMeasurements.txt")

#> str(FullDatasetMeasurements)
#'data.frame':	10299 obs. of  68 variables:

5 b) Creates the final, independent tidy data set with the average of each variable for each activity and each subject.

measurements <- FullDatasetMeasurements[, 3:dim(FullDatasetMeasurements)[2]]
TidyDataset <- aggregate(measurements, list(FullDatasetMeasurements$subject, FullDatasetMeasurements$activity), mean)
names(TidyDataset)[1:2] <- c('subject', 'activity')
write.table(TidyDataset, "./UCI HAR Dataset/TidyDataset2.txt")



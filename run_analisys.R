# run_analysis.R' script  :

# 1 Merges the training and the test sets to create one data set.

SubjectFunction <- function() {
  SubjTest <- read.table('./UCI HAR Dataset/test/subject_test.txt')
  SubjTrain <- read.table('./UCI HAR Dataset/train/subject_train.txt')
  SubjFull <- rbind(SubjTrain, SubjTest)
  names(SubjFull) <- "subject"
  SubjFull
}

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


# 2 Extracts only the measurements on the mean and standard deviation for each measurement.

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

# 3) Uses descriptive activity names to name the activities in the data set

ActivityLabels <- read.table('./UCI HAR Dataset/activity_labels.txt', header=FALSE, col.names=c('id', 'name'))


# 4) Appropriately labels the data set with descriptive activity names.

Ydataset[, 1] = ActivityLabels[Ydataset[, 1], 2]
names(Ydataset) <- "activity"


#5 a) Intermediate dataset with required measurements.

FullDatasetMeasurements <- cbind(Subjectdata, Ydataset, XFiltered)
write.table(FullDatasetMeasurements, "./UCI HAR Dataset/FullDatasetMeasurements.txt")



# 5 b) Creates the final, independent tidy data set with the average of each variable for each activity and each subject.

measurements <- FullDatasetMeasurements[, 3:dim(FullDatasetMeasurements)[2]]
TidyDataset <- aggregate(measurements, list(FullDatasetMeasurements$subject, FullDatasetMeasurements$activity), mean)
names(TidyDataset)[1:2] <- c('subject', 'activity')
write.table(TidyDataset, "./UCI HAR Dataset/TidyDataset2.txt")

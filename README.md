# GettingAndCleanningData
Coursera Project

## Load necessary packages
library(dplyr)
library(tidyr)


# 1. Merges the training and the test sets to create one data set.

##Read data from the files into the variables
trainDat <- read.table("./X_train.txt", header=FALSE, sep = "")
trainDat <- cbind(trainDat, read.table("./subject_train.txt"), 
            read.table("./y_train.txt"))
testDat <- read.table("./X_test.txt", header=FALSE, sep = "")
testDat <- cbind(testDat, read.table("./subject_test.txt"), 
           read.table("./y_test.txt"))
           
##Concatenate
dataFull <- rbind(trainDat, testDat)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

   ##Read the features data.
   
features <- read.table("./features.txt", header=FALSE, stringsAsFactors=FALSE)
features <- make.names(features[,"V2"])

  ## Create a variable extracting standard deviation and mean.
  
mean <- dataFull[,grep(pattern="std|mean", x=features, ignore.case=TRUE)]

# 3. Uses descriptive activity names to name the activities in the data set
 
   ## Read activity names data
   
Labels <- read.table("./activity_labels.txt", header=FALSE, stringsAsFactors=FALSE)
Labels <- apply(Labels, 1, function(x) unlist(strsplit(x, split=" ")))

   ## Apply factor for manage the activity names in the full data set
   
dataFull[,563] <- factor(as.factor(dataFull[,563]), labels=Labels[2,])

# 4. Appropriately labels the data set with descriptive activity names. 


   ## Read features data and add in the full data set
   
   
features <- read.table("./features.txt", header=FALSE, stringsAsFactors=FALSE)
features <- make.names(features[,"V2"])
features[562] = "subject"
features[563] = "activity"
colnames(dataFull) <- features
for (i in 1:length(colnames)) 
{
  colnames[i] = gsub("\\()","",colnames[i])
  colnames[i] = gsub("^(t)","time",colnames[i])
  colnames[i] = gsub("^(f)","freq",colnames[i])
  colnames[i] = gsub("([Gg]ravity)","Gravity",colnames[i])
  colnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colnames[i])
  colnames[i] = gsub("[Gg]yro","Gyro",colnames[i])
  colnames[i] = gsub("AccMag","AccMagnitude",colnames[i])
  colnames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colnames[i])
  colnames[i] = gsub("JerkMag","JerkMagnitude",colnames[i])
  colnames[i] = gsub("GyroMag","GyroMagnitude",colnames[i])
};

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
labels <- colnames(dataFull)[-c(562,563)]


     ## Function for the average of each variable for each activity and each subject
     
     
second <- lapply(X=labels, FUN=function(x) tapply(dataFull[[x]], list(dataFull$activity, dataFull$subject), mean))
names(second) <- labels


write.table(second, file = "tidySet2.txt", row.name=FALSE)

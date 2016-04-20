# 1. Merges the training and the test sets to create one data set.
## Load necessary packages
library(dplyr)
library(tidyr)

trainDat <- read.table("./X_train.txt", header=FALSE, sep = "")
trainDat <- cbind(trainDat, read.table("./subject_train.txt"), 
            read.table("./y_train.txt"))
testDat <- read.table("./X_test.txt", header=FALSE, sep = "")
testDat <- cbind(testDat, read.table("./subject_test.txt"), 
           read.table("./y_test.txt"))
dataFull <- rbind(trainDat, testDat)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("./features.txt", header=FALSE, stringsAsFactors=FALSE)
features <- make.names(features[,"V2"])
mean <- dataFull[,grep(pattern="std|mean", x=features, ignore.case=TRUE)]

# 3. Uses descriptive activity names to name the activities in the data set
Labels <- read.table("./activity_labels.txt", header=FALSE, stringsAsFactors=FALSE)
Labels <- apply(Labels, 1, function(x) unlist(strsplit(x, split=" ")))
dataFull[,563] <- factor(as.factor(dataFull[,563]), labels=Labels[2,])

# 4. Appropriately labels the data set with descriptive activity names. 
features <- read.table("./features.txt", header=FALSE, stringsAsFactors=FALSE)
features <- make.names(features[,"V2"])
features[562] = "subject"
features[563] = "activity"
colnames(dataFull) <- features

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
labels <- colnames(dataFull)[-c(562,563)]
second <- lapply(X=labels, FUN=function(x) tapply(dataFull[[x]], list(dataFull$activity, dataFull$subject), mean))
names(second) <- labels
write.table(second, file = "tidySet2.txt", row.name=FALSE)
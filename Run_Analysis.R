#install.packages("sqldf")
library(sqldf)

setwd("/Users/harishagarwal/Desktop/Harish/Data Science Courses/Coursera/Data Cleaning/UCI HAR Dataset")

# Reading the files, and training data into R
features        <- read.table("./features.txt",header=FALSE)
activityLabel   <- read.table("./activity_labels.txt",header=FALSE)
subjectTrain    <-read.table("./train/subject_train.txt", header=FALSE)
xTrain          <- read.table("./train/X_train.txt", header=FALSE)
yTrain          <- read.table("./train/y_train.txt", header=FALSE)

# Assign column names to the data above.

colnames(activityLabel)<-c("activityId","activityType")
colnames(subjectTrain) <- "subId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"

#Merging training Data.

trainData <- cbind(yTrain,subjectTrain,xTrain)

#Reading the test Data

subjectTest    <-read.table("./test/subject_test.txt", header=FALSE)
xTest         <- read.table("./test/X_test.txt", header=FALSE)
yTest         <- read.table("./test/y_test.txt", header=FALSE)

# Assign column names.

colnames(subjectTest) <- "subId"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityId"

# Merging test Data
testData <- cbind(yTest,subjectTest,xTest)

# Merge the training and the test sets to create one data set.

finalData <- rbind(trainData,testData)

# Extract only the measurements on the mean and standard deviation for each measurement.

colnames(finalData)
data_mean_std = finalData[, grepl("mean|std|activityId|subId", colnames(finalData))]
head(data_mean_std)

# Use descriptive activity names to name the activities in the data set

data_mean_std <- merge(data_mean_std, activityLabel, by = "activityId", match = "first")
head(data_mean_std)

# Appropriately label the data set with descriptive variable names.

# Remove parentheses

names(data_mean_std) <- gsub("\\(|\\)", "", names(data_mean_std))

# Correct syntax in names

names(data_mean_std) <- make.names(names(data_mean_std))

# Add descriptive names

names(data_mean_std) <- gsub("Acc", "Acceleration", names(data_mean_std))
names(data_mean_std) <- gsub("^t", "Time", names(data_mean_std))
names(data_mean_std) <- gsub("^f", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("BodyBody", "Body", names(data_mean_std))
names(data_mean_std) <- gsub("mean", "Mean", names(data_mean_std))
names(data_mean_std) <- gsub("std", "Std", names(data_mean_std))
names(data_mean_std) <- gsub("Freq", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("Mag", "Magnitude", names(data_mean_std))

names(data_mean_std)

# From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)
melted_data = melt(data_mean_std, id.vars = c("activityId", "subId", "activityType"), variable.name = "metric")
melted_data_avg = melted_data %>% group_by(activityId, subId,activityType, metric) %>% summarise(avg = mean(value))
tidy_data = melted_data_avg %>% dcast(activityId + subId + activityType ~ metric, value.var = "avg")

write.table(tidy_data, "tidy_data.txt", row.names = FALSE) 

setwd("C:/work/Training/DataScience&MachineLearning/Course3/")
library(dplyr)
library(data.table)
library(tidyr)
library(reshape2)


training_subject <- tbl_df(read.table( "UCI HAR Dataset/train/subject_train.txt"))
test_subject  <- tbl_df(read.table("UCI HAR Dataset/test/subject_test.txt" ))


training_activity_label <- tbl_df(read.table("UCI HAR Dataset/train/Y_train.txt"))
test_activity_label  <- tbl_df(read.table("UCI HAR Dataset/test/Y_test.txt" ))

training_data <- tbl_df(read.table("UCI HAR Dataset/train/X_train.txt" ))
test_data  <- tbl_df(read.table("UCI HAR Dataset/test/X_test.txt" ))

feature_data <- tbl_df(read.table("UCI HAR Dataset/features.txt"))
activity_Label<- tbl_df(read.table("UCI HAR Dataset/activity_labels.txt"))

# Merges the training and the test sets to create one data set.
subject_data <- rbind(training_subject, test_subject)
setnames(subject_data, "V1", "subject")
activity_label_data<- rbind(training_activity_label, test_activity_label)
setnames(activity_label_data, "V1", "activityNum")

#combine the DATA training and test files
training_dataset <- rbind(training_data, test_data)

# Appropriately labels the data set with descriptive variable names.

setnames(feature_data, names(feature_data), c("featureNum", "featureName"))
colnames(training_dataset) <- feature_data$featureName


setnames(activity_Label, names(activity_Label), c("activityNum","activityName"))

# Merge           
subject_activity_data<- cbind(subject_data, activity_label_data)
training_dataset <- cbind(subject_activity_data, training_dataset)

# Extracts only the measurements on the mean and standard deviation for each measurement.
sta_data <- grep("mean\\(\\)|std\\(\\)",feature_data$featureName,value=TRUE) 


sta_data <- union(c("subject","activityNum"), sta_data)
training_dataset<- subset(training_dataset,select=sta_data) 

#Uses descriptive activity names to name the activities in the data set
training_dataset <- merge(activity_Label, training_dataset , by="activityNum", all.x=TRUE)
training_dataset$activityName <- as.character(training_dataset$activityName)

## creates  independent tidy data set with the average of each variable for each activity and each subject.
training_dataset$activityName <- as.character(training_dataset$activityName)
aggr_data<- aggregate(. ~ subject - activityName, data = training_dataset, mean) 
training_dataset<- tbl_df(arrange(aggr_data,subject,activityName))

names(training_dataset)<-gsub("std()", "SD", names(training_dataset))
names(training_dataset)<-gsub("mean()", "MEAN", names(training_dataset))
names(training_dataset)<-gsub("^t", "time", names(training_dataset))
names(training_dataset)<-gsub("^f", "frequency", names(training_dataset))
names(training_dataset)<-gsub("Acc", "Accelerometer", names(training_dataset))
names(training_dataset)<-gsub("Gyro", "Gyroscope", names(training_dataset))
names(training_dataset)<-gsub("Mag", "Magnitude", names(training_dataset))
names(training_dataset)<-gsub("BodyBody", "Body", names(training_dataset))
write.table(training_dataset, "final_dataset.txt", row.name=FALSE)

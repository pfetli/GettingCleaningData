## === Assignment: Getting and Cleaning Data

## == Description and Task

# The purpose of this project is to demonstrate your ability to collect, 
# work with, and clean a data set. The goal is to prepare tidy data that 
# can be used for later analysis. You will be graded by your peers on a 
# series of yes/no questions related to the project. You will be required 
# to submit: 1) a tidy data set as described below, 2) a link to a Github 
# repository with your script for performing the analysis, and 3) a code 
# book that describes the variables, the data, and any transformations or 
# work that you performed to clean up the data called CodeBook.md. 
# You should also include a README.md in the repo with your scripts. 
# This repo explains how all of the scripts work and how they are connected.

# Here are the data for the project:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# You should create one R script called run_analysis.R that does the following.
# 
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names.
# 5) From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.

## == Start code ==
getwd()
# = Download, unzip data
filename <- "dataset.zip" # define the name of the file
if(!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
  unzip(filename)
}
list.files()
list.files("UCI HAR Dataset/")


# = Load features and activity labels
# 'features.txt': List of all features.
dat.features <- read.table("UCI HAR Dataset/features.txt")
str(dat.features)
# 'activity_labels.txt': Links the class labels with their activity name.
dat.activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")
str(dat.activitylabels)


# = Define the selection: Extract only the data on mean and standard deviation
# Extract only the data on mean and standard deviation -> Define selection
dat.wanted <- grep(".*mean.*|.*std.*", dat.features[,2])
# Make the variablenames nice and readable
dat.wanted.names <- dat.features[dat.wanted,2]
dat.wanted.names = gsub('-mean', 'Mean', dat.wanted.names)
dat.wanted.names = gsub('-std', 'Std', dat.wanted.names)
dat.wanted.names <- gsub('[-()]', '', dat.wanted.names)


# = Load and subset data: Training data
# 'train/X_train.txt': Training set.
dat.trainset <- read.table("UCI HAR Dataset/train/X_train.txt")[dat.wanted]
# train/y_train.txt': Training labels.
dat.trainactivity <- read.table("UCI HAR Dataset/train/y_train.txt")
# 'train/subject_train.txt': Each row identifies the subject who 
# performed the activity for each window sample. Its range is from 1 to 30.
dat.trainsubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")

# = Load  and subset data: Test data -> load subset
# 'test/X_test.txt': Test set.
dat.testset <- read.table("UCI HAR Dataset/test/X_test.txt")[dat.wanted]
# 'test/y_test.txt': Test labels.
dat.testactivity <- read.table("UCI HAR Dataset/test/Y_test.txt")
# 'test/subject_test.txt': Each row identifies the subject who performed 
# the activity for each window sample. Its range is from 1 to 30.
dat.testsubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")


# = Merges the training and the test sets to create one data set.
# Combine the data by columns
train.cbind <- cbind(dat.trainsubjects, dat.trainactivity, dat.trainset)
test.cbind <- cbind(dat.testsubjects, dat.testactivity, dat.testset)
# Combine data by row
dat.rbind <- rbind(train.cbind,test.cbind)


# = Appropriately labels the data set with descriptive variable names
colnames(dat.rbind) <- c("subject","activity",dat.wanted.names)


# = Uses descriptive activity names to name the activities in the data set
dat.rbind$activity <- factor(dat.rbind$activity, 
                             levels=dat.activitylabels[,1], 
                             labels=dat.activitylabels[,2])


# = From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.
library(dplyr)
# Calculate mean
dat.mean <- dat.rbind %>% group_by(subject, activity) %>%
  summarise_each(funs(mean(.,na.rm=TRUE)))
# Export table
write.table(dat.mean, "tidy_data.txt", row.names = FALSE, quote = FALSE)





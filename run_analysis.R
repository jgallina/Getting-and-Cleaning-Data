#You should create one R script called run_analysis.R that does the following. 
        #Part 1 - Merges the training and the test sets to create one data set.
        #Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
        #Part 3 - Uses descriptive activity names to name the activities in the data set
        #Part 4 - Appropriately labels the data set with descriptive variable names. 
        #Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#Clear workspace to free up available RAM 
rm(list = ls())

#The libraries we will use to clean the data are called "data.table" and "dplr"
#Check if these packages are installed, if not, install required packages 
list.of.packages <- c("dplyr", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}
#Load the packages 
library(dplyr)
library(data.table)


#Create in working directory a folder "data" to store files in 
if(!file.exists("data")) {
        dir.create("data")
}

#Download the files into the data folder that we've just created. 
# download.file >> Method = "curl" required when working with mac and https. 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile= "./data/UCIData.zip", method = "curl")
list.files("./data")
dateDownloaded <- date()
dateDownloaded
unzip("./data/UCIData.zip", exdir= "./data") 


# Read the features, Read the names of the activities. 
Feature_Names <- read.table("./data/UCI HAR Dataset/features.txt", header = FALSE) 
Activity_Labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE) 


# Read training and test data sets. There are three training files and three test files. 

subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt", header = FALSE) 

subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt", header = FALSE) 

# Merge the data
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest) 

# Name the columns. Note that we need to transpose Feature_Names because it's stored in rows. 
colnames(features) <- t(Feature_Names[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

# Part 1 - Merge to create one data set 
AllData <- cbind(features, activity, subject) 

# Part 2 - Extract only the measurements on the mean and standard deviation
columns_with_meanSTD <- grep(".*Mean.*|.*Std.*", names(AllData),ignore.case=TRUE)
requiredColumns <- c(columns_with_meanSTD, 562, 563)
extractedData <- AllData[,requiredColumns]

# Part 3 - Uses descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6) { 
        extractedData$Activity[extractedData$Activity == i] <- as.character(Activity_Labels[i,2])
        }
extractedData$Activity <- as.factor(extractedData$Activity)

# Part 4 - Appropriately labels the data set with descriptive variable names. 
names(extractedData)
names(extractedData) <- gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData) <- gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData) <- gsub("BodyBody", "Body", names(extractedData))
names(extractedData) <- gsub("Mag", "Magnitude", names(extractedData))
names(extractedData) <- gsub("^t", "Time", names(extractedData))
names(extractedData) <- gsub("^f", "Frequency", names(extractedData))
names(extractedData) <- gsub("tBody", "TimeBody", names(extractedData))
names(extractedData) <- gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("angle", "Angle", names(extractedData))
names(extractedData) <- gsub("gravity", "Gravity", names(extractedData))

# Part 5 - Create a second, independent tidy data set with the average of each variable for each activity and each subject.
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject, tidyData$Activity),]
write.table(tidyData, file = "./data/Tidy.txt", row.names = FALSE) 

#read the tidy file back into R 
tidy <- read.table("./data/Tidy.txt")







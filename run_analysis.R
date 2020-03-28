#Coursera Getting and Cleaning data week 4 Project

library(dplyr)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "D:/Coursera/JH_3_Getting and cleaning data/data/UCI HAR Dataset.zip"

if(!file.exists(destfile))
{
  download.file(url = url,destfile = destfile,mode = 'wb')
  
}

if(!file.exists("D:/Coursera/JH_3_Getting and cleaning data/data/UCI HAR Dataset"))
{
  unzip(destfile)
}

dateDownloaded <- date()

setwd("D:/Coursera/JH_3_Getting and cleaning data/data/UCI HAR Dataset")

#read feature file (train and test)

Features_train <- read.table("./train/X_train.txt",header = FALSE)
Features_test <- read.table("./test/X_test.txt", header = FALSE)

#read activities file

Activity_train <- read.table("./train/y_train.txt",header = FALSE)
Activity_test <- read.table("./test/y_test.txt",header = FALSE)

#read subject file

Subject_train <- read.table("./train/subject_train.txt",header = FALSE)
Subject_test <- read.table("./test/subject_test.txt", header = FALSE)

#read activity labels

activitylabels <- read.table("./activity_labels.txt", header = FALSE)

#read features

features <- read.table("./features.txt",header = FALSE)


#merging data

Featuresdata <- rbind(Features_train,Features_test)
Subjectdata <- rbind(Subject_train,Subject_test)
Activitydata <- rbind(Activity_train,Activity_test)

#renaming columns in activitydata

names(Activitydata) <- "activityNumber"
names(activitylabels) <- c("activityNumber","activity")

#get factors of activity
Activity <- left_join(Activitydata,activitylabels,"activityNumber")[,2]

#rename subjectdata and featuredata columns
names(Subjectdata) <- "Subject"
names(Featuresdata) <- features$V2


NewDataset <- cbind(Subjectdata,Activity)
NewDataset <- cbind(NewDataset,Featuresdata)


###Create New datasets by extracting only the measurements on the mean and standard deviation for each measurement
subfeatures <- features$V2[grep("mean\\(\\)|std\\(\\)",features$V2)]
datanames <- c("Subject", "Activity", as.character(subfeatures))
NewDataset <- subset(NewDataset,select = datanames)

#renaming with more descriptive activity names

names(NewDataset) <- gsub("^t","time",names(NewDataset))
names(NewDataset) <- gsub("^f","frequency",names(NewDataset))
names(NewDataset) <- gsub("Acc","Accelerometer",names(NewDataset))
names(NewDataset) <- gsub("Gyro","Gyroscope",names(NewDataset))
names(NewDataset) <- gsub("Mag","Magnitude",names(NewDataset))
names(NewDataset) <- gsub("BodyBody","Body",names(NewDataset))

#create second new tidy dataset

secondNewdataset <- aggregate(.~Subject + Activity, NewDataset,mean)
secondNewdataset <- secondNewdataset[order(secondNewdataset$Subject,secondNewdataset$Activity),]

#save tidy dataset

write.table(secondNewdataset,file = "tidydata.txt",row.name = FALSE)

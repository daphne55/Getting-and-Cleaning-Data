#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
#The goal is to prepare tidy data that can be used for later analysis. 
#required to submit: 
#1) a tidy data set as described below, 
#2) a link to a Github repository with your script for performing the analysis, and 
#3) a code book that describes the variables, the data, and any transformations or work that you performed 
#to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. 
#This repo explains how all of the scripts work and how they are connected. 

#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#Here are the data for the project:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#You should create one R script called run_analysis.R that does the following. 
#1) Merges the training and the test sets to create one data set.
#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#3) Uses descriptive activity names to name the activities in the data set
#4) Appropriately labels the data set with descriptive variable names. 
#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


#download data
library(httr) 
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "instancia.zip"
if(!file.exists(file)){
  print("descargando")
  download.file(url, file)
}

#unzip and create folders
datafolder <- "UCI HAR Dataset"
resultsfolder <- "results"
if(!file.exists(datafolder)){
  print("unzip file")
  unzip(file, list = FALSE, overwrite = TRUE)
} 
if(!file.exists(resultsfolder)){
  print("create results folder")
  dir.create(resultsfolder)
} 

#read txt and covnert to data.frame
gettables <- function (filename,cols = NULL){
  print(paste("Getting table:", filename))
  f <- paste(datafolder,filename,sep="/")
  data <- data.frame()
  if(is.null(cols)){
    data <- read.table(f,sep="",stringsAsFactors=F)
  } else {
    data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
  }
  data
}

#run and check gettables
features <- gettables("features.txt")

#read data and build database
getdata <- function(type, features){
  print(paste("Getting data", type))
  subject_data <- gettables(paste(type,"/","subject_",type,".txt",sep=""),"id")
  y_data <- gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
  x_data <- gettables(paste(type,"/","X_",type,".txt",sep=""),features$V2)
  return (cbind(subject_data,y_data,x_data))
}

#run and check getdata
test <- getdata("test", features)
train <- getdata("train", features)

#save the resulting data in the indicated folder
saveresults <- function (data,name){
  print(paste("saving results", name))
  file <- paste(resultsfolder, "/", name,".csv" ,sep="")
  write.csv(data,file)
}

##actions

#1) Merges the training and the test sets to create one data set.
install.packages("plyr")
library(plyr)
data <- rbind(train, test)
data <- arrange(data, id)
# read data into data frames
subject_train <- read.table("subject_train.txt")
subject_test <- read.table("subject_test.txt")
X_train <- read.table("X_train.txt")
X_test <- read.table("X_test.txt")
y_train <- read.table("y_train.txt")
y_test <- read.table("y_test.txt")

# add column name for subject files
names(subject_train) <- "subjectID"
names(subject_test) <- "subjectID"

# add column names for measurement files
featureNames <- read.table("features.txt")
names(X_train) <- featureNames$V2
names(X_test) <- featureNames$V2

# add column name for label files
names(y_train) <- "activity"
names(y_test) <- "activity"

# combine files into one dataset
train <- cbind(subject_train, y_train, X_train)
test <- cbind(subject_test, y_test, X_test)
combined <- rbind(train, test)

#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# determine which columns contain "mean()" or "std()"
meanstdcols <- grepl("mean\\(\\)", names(combined)) |
    grepl("std\\(\\)", names(combined))

# ensure that we also keep the subjectID and activity columns
meanstdcols[1:2] <- TRUE

# remove unnecessary columns
combined <- combined[, meanstdcols]

#3) Uses descriptive activity names to name the activities in the data set
activity_labels <- gettables("activity_labels.txt")

#4) Appropriately labels the data set with descriptive variable names. 
# convert the activity column from integer to factor
combined$activity <- factor(combined$activity, labels=c("Walking",
    "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))

#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# create the tidy data set
melted <- melt(combined, id=c("subjectID","activity"))
tidy <- dcast(melted, subjectID+activity ~ variable, mean)

# write the tidy data set to a file
write.csv(tidy, "tidy.csv", row.names=FALSE)
saveresults(tidy_dataset,"tidy_dataset")

##Set Working directory
  setwd("~/Documents/MOOC/1 - Data Scientist/3 - Getting and Cleaning Data/W4")
  
##Get the File if don't exist
  if(!file.exists("./data")){dir.create("./data")}
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile="./data/TidyDataset.zip",method="curl")
  
##Unzip the File
  unzip(zipfile="./data/TidyDataset.zip",exdir="./data")
  
  ##Set new WK
  setwd(file.path("./data" , "UCI HAR Dataset"))
  
##Reading Files
  
  #Read Features Files
  dataFeaturesTest  <- read.table("./test/X_test.txt",header = FALSE)
  dataFeaturesTrain  <- read.table("./train/X_train.txt",header = FALSE)
  
  #Read Activity Files
  dataActivityTest  <- read.table("./test/Y_test.txt",header = FALSE)
  dataActivityTrain <- read.table("./train/Y_train.txt",header = FALSE)
  
  #Read Subject Files
  dataSubjectTest  <- read.table("./test/subject_test.txt",header = FALSE)
  dataSubjectTrain  <- read.table("./train/subject_train.txt",header = FALSE)

## 1) Merges the training and the test sets to create one data set.
  
##Combining subTable by Row
  dataFeatures <- rbind(dataFeaturesTrain, dataFeaturesTest)
  dataActivity<- rbind(dataActivityTrain, dataActivityTest)
  dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
  
  ##Naming
  #Header of each Subject/Activity vector
    names(dataSubject)<-c("subject")
    names(dataActivity)<- c("activity")
  #Get and set the features names
    dataFeaturesName  <- read.table("./features.txt",header = FALSE)
    names(dataFeatures) <- dataFeaturesName[,2]
  
##Combining All Table
    dataSubAct<-cbind(dataSubject,dataActivity)
    data<-cbind(dataFeatures,dataSubAct)
    
## 2) Extracts only the measurements on the mean and standard deviation for each measurement.

  #Now we need to get the numbers associated to the mean and stdev features
    subFeaturesNames<-dataFeaturesName[,2][grep("mean\\(\\)|std\\(\\)", dataFeaturesName[,2])]
  
  #Subset column from dataset with only mean, std, activity and subject
    selectedCols<-c(as.character(subFeaturesNames), "subject", "activity" )
    subData<-subset(data,select=selectedCols)
    
## 3) Uses descriptive activity names to name the activities in the data set.
    
  #Read Activity labels
    activityLabels <- read.table("./activity_labels.txt",header = FALSE)
    names(activityLabels) <- c("activityId", "activityName")

  #Factor Activity Number by Activity Name
    subData$activity<-factor(subData$activity,levels=activityLabels$activityId, labels=activityLabels$activityName)
    
## 4) Appropriately labels the data set with descriptive variable names.
    
    names(subData)<-gsub("Acc", "Accelerometer", names(subData))
    names(subData)<-gsub("Gyro", "Gyroscope", names(subData))
    names(subData)<-gsub("Mag", "Magnitude", names(subData))
    names(subData)<-gsub("^t", "time", names(subData))
    names(subData)<-gsub("^f", "frequency", names(subData))
    names(subData)<-gsub("BodyBody", "Body", names(subData))
    
## 5) Create a second, independent tidy data set with the average of each variable for each activity and each subject.
    pdata <- subData[,1:66]
    subDataAvg <- aggregate(pdata,list(subData$subject, subData$activity), mean)
    
    write.table(subDataAvg, file = "Cleandata.txt",row.name=FALSE)
    
## Coursera Getting and Cleaning Data Course Project 

## This R script called run_analysis.R will do the following:

## After downloaded from: 
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## Will perform the following steps on the UCI HAR Dataset:

## 1. Merge the training and the test sets to create one data set. 
## 2. Extract only the measurements on the mean and standard deviation for each measurement.
## 3. Use descriptive activity names to name the activities in the data set 
## 4. Appropriately label the data set with descriptive activity names.  
## 5. Creates a second, independent tidy data set with the average of each variable for each 
##    activity and each subject. 

run_analysis <- function(){

        # Clean up workspace
        rm(list=ls())

        ## 1. Merge the training and the test sets to create one data set.
        ## print("Start 1. Merge the training and the test sets to create one data set")
        
        ## set working directory to the location where the UCI HAR Dataset was unzipped
        setwd("D:/Coursera/Getting and Cleaning Data Course Project/UCI HAR Dataset")

        ## Read data from files features.txt, activity_labels.txt, subject_train.txt, 
        ##                      x_train.txt and y_train.txt
        features = read.table('./features.txt',header=FALSE)
        activityType = read.table('./activity_labels.txt',header=FALSE)
        subjectTrain = read.table('./train/subject_train.txt',header=FALSE)
        xTrain = read.table('./train/x_train.txt',header=FALSE)
        yTrain = read.table('./train/y_train.txt',header=FALSE)

        ## Assign column names to the data loaded
        colnames(activityType) = c('activityId','activityType')
        colnames(subjectTrain) = "subjectId"
        colnames(xTrain) = features[,2]
        colnames(yTrain) = "activityId"

        ## Create the final training set by merging yTrain, subjectTrain and xTrain
        trainingData = cbind(yTrain,subjectTrain,xTrain)

        ## Read data from files subject_test.tx, x_test.txt and y_test.txt
        subjectTest = read.table('./test/subject_test.txt',header=FALSE)
        xTest = read.table('./test/x_test.txt',header=FALSE)
        yTest = read.table('./test/y_test.txt',header=FALSE)

        ## Assign column names to the test data loaded
        colnames(subjectTest) = "subjectId"
        colnames(xTest) = features[,2] 
        colnames(yTest) = "activityId"

        ## Create final test set by merging data xTest, yTest and subjectTest
        testData = cbind(yTest,subjectTest,xTest)

        ## Combine training and test data to create the final data set
        finalData = rbind(trainingData,testData)

        ## Create a vector for the column names for the finalData, which will be used
        ## to extracts the required mean() & stddev()
        colNames  = colnames(finalData)
        ## print("End 1. Merge the training and the test sets to create one data set")
        
        ## 2. Extract only the measurements on the mean and standard deviation for each measurement.
        ## print("Star 2. Extract only the measurements on the mean and standard deviation for each measurement")
        ## Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns 
        ## and FALSE for others
        logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) 
                         | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) 
                         & !grepl("mean..-",colNames) | grepl("-std..",colNames) 
                         & !grepl("-std()..-",colNames)) 

        ## Subset finalData table based on the logicalVector to keep required columns
        finalData = finalData[logicalVector==TRUE] 
        ## print("End 2. Extract only the measurements on the mean and standard deviation for each measurement")
        
        ## 3. Use descriptive activity names to name the activities in the data set
        ## print("Star 3. Use descriptive activity names to name the activities in the data set")
        ## Merge the finalData set with the acitivityType table to include descriptive activity names
        finalData = merge(finalData,activityType,by='activityId',all.x=TRUE)
        
        ## Updating the colNames vector to include the new column names after merge
        colNames  = colnames(finalData)
        ## print("End 3. Use descriptive activity names to name the activities in the data set")
        
        ## 4. Appropriately label the data set with descriptive activity names
        ## print("Star 4. Appropriately label the data set with descriptive activity names")
        ## Cleaning up the variable names
        for (i in 1:length(colNames)){ 
          colNames[i] = gsub("\\()","",colNames[i]) 
          colNames[i] = gsub("-std$","StdDev",colNames[i]) 
          colNames[i] = gsub("-mean","Mean",colNames[i]) 
          colNames[i] = gsub("^(t)","time",colNames[i]) 
          colNames[i] = gsub("^(f)","freq",colNames[i]) 
          colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i]) 
          colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i]) 
          colNames[i] = gsub("[Gg]yro","Gyro",colNames[i]) 
          colNames[i] = gsub("AccMag","AccMagnitude",colNames[i]) 
          colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i]) 
          colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i]) 
          colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i]) 
        } ## end for

        ## Reassigning the new column names to the finalData set
        colnames(finalData) = colNames 
        ## print("End 4. Appropriately label the data set with descriptive activity names")
        
        ## 5. Create a second, independent tidy data set with the average of each variable 
        ##    for each activity and each subject. 
        ## print("Star 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject")
        ## Create a new table, finalDataNoActivityType without the activityType column
        finalDataNoActivityType  = finalData[,names(finalData) != 'activityType']
        
        ## Summarizing the finalDataNoActivityType table to include just the mean of each variable 
        ## for each activity and each subject
        tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != 
                        c('activityId','subjectId')], 
                        by=list(activityId=finalDataNoActivityType$activityId, 
                        subjectId = finalDataNoActivityType$subjectId),mean) 
        
        ## Merging the tidyData with activityType to include descriptive acitvity names
        tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE)
        
        ## Create the tidyData set 
        write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t') 
        ## print("End 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject")
} ## end run_analysis function

## Getting and Cleaning Data - PROJECT
## run_analysis.R 
library(dplyr)
library(plyr)

##step 1
#read in all required files
subject_test <- read.table("./test/subject_test.txt")
X_test <- read.table("./test/X_test.txt")
Y_test <- read.table("./test/Y_test.txt")

subject_train <- read.table("./train/subject_train.txt")
X_train <- read.table("./train/X_train.txt")
Y_train <- read.table("./train/Y_train.txt")

features <- read.table("./features.txt")
activity_labels <- read.table("./activity_labels.txt")

#bind X_test & X_train data
mtesttrain <- bind_rows (X_test,X_train)


##step 2
#select mean & std measurements 

colnames(mtesttrain) <- features$V2
mtesttrain2 <- mtesttrain[, !duplicated(colnames(mtesttrain))]
mttmeanstd<-select(mtesttrain2, contains("mean"), contains("std"))

##step 3  
#merge test n train labels

#activity labels
mtesttrainlabels <- bind_rows (Y_test, Y_train)
mttactivity <- bind_cols (mtesttrainlabels,mttmeanstd)

#subject column 30 volunteers
mtesttrainsubjectlabels <- bind_rows(subject_test, subject_train)
mttactsub <- bind_cols(mtesttrainsubjectlabels, mttactivity)
colnames(mttactsub)[1] <- "Subject"
colnames(mttactsub)[2] <- "Activity"

names(activity_labels) = c("Activity", "Activity desc")
#test data
mttactivityTTT <- mttactsub


mttactivityTTT <- join(mttactivityTTT, activity_labels, by = "Activity", type="left", match="all")
head(mttactivityTTT)



#step 4
#give better descriptive variable names
names(mttactivityTTT)
names(mttactivityTTT)<- gsub("tBodyAcc", "time_BodyAccelerometer",names(mttactivityTTT))
names(mttactivityTTT)<- gsub("tGravityAcc", "time_GravityAccelerometer", names(mttactivityTTT))
names(mttactivityTTT)<- gsub("tBodyGyro", "time_BodyGyroscope", names(mttactivityTTT))
names(mttactivityTTT)<- gsub("fBodyAcc", "FastFourierTransform_BodyAccelerometer", names(mttactivityTTT))
names(mttactivityTTT)<- gsub("fBodyGyro", "FastFourierTransform_BodyGyroscope", names(mttactivityTTT))
names(mttactivityTTT)<-gsub("fBodyBodyAcc", "FastFourierTransform_BodyBodyAccelerometer", names(mttactivityTTT))
names(mttactivityTTT)<-gsub("fBodyBodyGyro", "FastFourierTransform_BodyBodyGyroscope", names(mttactivityTTT))



##step 5
drops <- c("angle(time_BodyAccelerometerMean,gravity)",
           "angle(time_BodyAccelerometerJerkMean),gravityMean)" ,
           "angle(time_BodyGyroscopeMean,gravityMean)" ,
           "angle(time_BodyGyroscopeJerkMean,gravityMean)" ,
           "angle(X,gravityMean)",
           "angle(Y,gravityMean)",
           "angle(Z,gravityMean)",
           "Activity")
mttactivityTTT<-mttactivityTTT[,!(names(mttactivityTTT) %in% drops)]
names(mttactivityTTT)


#reorder by column index  
mttactivityTTT <- mttactivityTTT[c(1,81,2:80)]


#average of each variable for each activity and each subject

colnames(mttactivityTTT)[2] <- "Activity"

final <-
  mttactivityTTT %>%
  group_by(Activity, Subject) %>%
  summarise_each(funs(mean(., na.rm=TRUE)))

#create a txt file of finalised dataset
write.table(final, file ="./final.txt",row.name=FALSE)


 

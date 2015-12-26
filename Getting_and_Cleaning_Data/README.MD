==================================================================
Getting and Cleaning Data Project
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Chealee
John Hopkins University
==================================================================

The script takes in two datasets (test and train) which was carried out by a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) 
on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate 
of 50Hz. 

The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the 
test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec 
and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a 
Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore 
a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency 
domain. See 'features_info.txt' for more details. 

How the script works:
======================================

- It reads in 8 files 
	1. subject_test 	(test subject id)
	2. X_test 		(test data)
	3. Y_test		(test activity data)
	4. subject_train	(train subject id)
	5. X_train		(train data)
	6. Y_train		(train activity data)
	7. features		(features label)
	8. activity_labels	(activity labels)

- Step One
	- It merges the test (30%) and train (70%) datasets

- Step Two 
	- Add column names to Step One's combined dataset and retrieve columns of dataset
	  that contains "mean" or "std"

- Step Three
	- Merge the activity column and its respective activity labels
	- Covert the activity numbers to its respective activites. E.g Activity 2 -> Standing

- Step Four
	- Provide meaningful column names
	- The following are converted:
		"tBodyAcc"	->	"time_BodyAccelerometer"
		"tGravityAcc"	->	"time_GravityAccelerometer"
		"tBodyGyro"	->	"time_BodyGyroscope"
		"fBodyAcc"	->	"FastFourierTransform_BodyAccelerometer"
		"fBodyGyro"	->	"FastFourierTransform_BodyGyroscope"
		"fBodyBodyAcc"	->	"FastFourierTransform_BodyBodyAccelerometer"
		"fBodyBodyGyro"	->	"FastFourierTransform_BodyBodyGyroscope"

- Step Five
	- Removal of non-related columns which uses the mean and std in their calculations
	  These are ommitted as they do not represent the original mean and std calculation values:
		- angle(time_BodyAccelerometerMean,gravity)
		- angle(time_BodyAccelerometerJerkMean),gravityMean)
		- angle(time_BodyGyroscopeMean,gravityMean)
		- angle(time_BodyGyroscopeJerkMean,gravityMean)
		- angle(X,gravityMean)
		- angle(Y,gravityMean)
		- angle(Z,gravityMean)
	- Derive an independent dataset that calculates the average of each variable 
	  for each activity and each subject


To read in the final text file:
===============================
1. set working directory to "UCI HAR Dataset" folder
2. R code to read in the final dataset:
	final <- read.table("./final.txt")
3. There are 180 observations and 81 variables



INITIAL DATA PROVIDED IN THIS PROJECT
The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. 
Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are 
radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a 
Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. 
Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

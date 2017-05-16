# CODEBOOK
## Background
This is a submission of the Data Science Course 3 course-end project.  Objective was to tidy up and summarize the cell-phone activity identification data (see reference at end).  The tidied data consists of selected fields from the features data that was provided.  A summary dataset, consisting of the average of each of the selected features per test subject per activity was also generated. 
## Source Data and Description
Researchers had 30 subjects perform one of 6 selected activities like walking, laying, going upstairs etc. while carrying a gyroscope-equipped cellphone.  The subjects were divided into two groups – training and test.  Each observation consisted of 50 readings per second (50 Hz) of the 3-axis gyroscopic data recorded by the cell phone while a subject carried out one of the activities.  There were multiple observations of the same subject performing the same activity.  This raw gyroscope data underwent preliminary processing to create 128 “frames” per observation for 9 different variables.   These pre-processed data have been provided in the Inertial Signals folders.  As part of this assignment, this inertial signal data was not used.
The features data, were generated from the above inertial data. Researchers generated secondary and tertiary data, for example jerk (rate of change of acceleration), frequency-domain data (by running a fast-Fourier transform on certain inertial data) and angular velocities around the three axes etc.
Final data are presented as 561 features – one feature vector per observation.  Each observation corresponds to one of the 30 subjects performing one of the 6 selected activities one time.   The researchers’ premise is that analyzing these 561 variables, they should be able to build a model that can use cell phone inertial data to predict what activities the subject performs.
## Objective
* Use the two features datasets (test and train) consisting of 561 values per observation
* Combine test and training data together
* Of the 561 features, select only those that contain a mean or std. deviation of other variables
* Produce a tidied dataset that can be utilized for further analysis easily
* The dataset should have meaningful and neat column names
* Produce a summary dataset consisting of the average of each selected feature per subject per activity

## Approach
* From the list of 561 features, find the ones that contain a mean or std.dev. of other variables.  86 such features were found.   Only these will be retained in the output.
* Clean up the column headings (original feature names contain punctuation like commas and parenthesis which are not acceptable in most languages as names (including in R).
* to each record, append identifiers that indicate what that line represents – Activity that was being performed, subject who was performing it, whether it came from the test or training set and which observation it was in that set (line number in the original test or train file – for traceability). De-normalize the Activity column, replacing Activity ID by Activity Name.
* Save the tidied dataset in a .Rdata file.  Also display a few sample records using View.
* Summarize the 86 features to calculate average of each per subject, per activity.
* Save this summary dataset as another .Rdata object and view it using View.
* Clean up all variables except the two output  data frames.

# File/Field Descriptions
## File Name - tidiedFeatures.Rdata
### Contents and Organization
Contains the 86 selected fields that represent mean or std.dev. of other fields.  To this, 4 additional fields have been added to aid in analysis and for traceability back to the original data.  Each line represents one observation (one subject performing one activity).
### Units and Scale 
None of the 86 measures have any units as all the feature values are normalized data and range from -1 to 1.  Units of the original measures were not provided by the authors of the research.
All factors are floating point numbers from -1.0 to +1.0 with 8 significant digits.
### Field Descriptions
* Group:   Origin -  Calculated. "test" or "train" depending on which data file this line was read from
* Observation:   Origin -  Calculated. numeric sequence number - line number in the original test or train file
* SubjectID:   Origin -  Calculated. subject id - read from the test or train subject chain which contains the subject ID for each corresponding observation
* Activity:   Origin -  Calculated. Activity Name - from Activity ID which was read from the Activity Chain for test or train
* tBodyAcc_mean_X:   Origin -  tBodyAcc-mean()-X
* tBodyAcc_mean_Y:   Origin -  tBodyAcc-mean()-Y
* tBodyAcc_mean_Z:   Origin -  tBodyAcc-mean()-Z
* tBodyAcc_std_X:   Origin -  tBodyAcc-std()-X
* tBodyAcc_std_Y:   Origin -  tBodyAcc-std()-Y
* tBodyAcc_std_Z:   Origin -  tBodyAcc-std()-Z
* tGravityAcc_mean_X:   Origin -  tGravityAcc-mean()-X
* tGravityAcc_mean_Y:   Origin -  tGravityAcc-mean()-Y
* tGravityAcc_mean_Z:   Origin -  tGravityAcc-mean()-Z
* tGravityAcc_std_X:   Origin -  tGravityAcc-std()-X
* tGravityAcc_std_Y:   Origin -  tGravityAcc-std()-Y
* tGravityAcc_std_Z:   Origin -  tGravityAcc-std()-Z
* tBodyAccJerk_mean_X:   Origin -  tBodyAccJerk-mean()-X
* tBodyAccJerk_mean_Y:   Origin -  tBodyAccJerk-mean()-Y
* tBodyAccJerk_mean_Z:   Origin -  tBodyAccJerk-mean()-Z
* tBodyAccJerk_std_X:   Origin -  tBodyAccJerk-std()-X
* tBodyAccJerk_std_Y:   Origin -  tBodyAccJerk-std()-Y
* tBodyAccJerk_std_Z:   Origin -  tBodyAccJerk-std()-Z
* tBodyGyro_mean_X:   Origin -  tBodyGyro-mean()-X
* tBodyGyro_mean_Y:   Origin -  tBodyGyro-mean()-Y
* tBodyGyro_mean_Z:   Origin -  tBodyGyro-mean()-Z
* tBodyGyro_std_X:   Origin -  tBodyGyro-std()-X
* tBodyGyro_std_Y:   Origin -  tBodyGyro-std()-Y
* tBodyGyro_std_Z:   Origin -  tBodyGyro-std()-Z
* tBodyGyroJerk_mean_X:   Origin -  tBodyGyroJerk-mean()-X
* tBodyGyroJerk_mean_Y:   Origin -  tBodyGyroJerk-mean()-Y
* tBodyGyroJerk_mean_Z:   Origin -  tBodyGyroJerk-mean()-Z
* tBodyGyroJerk_std_X:   Origin -  tBodyGyroJerk-std()-X
* tBodyGyroJerk_std_Y:   Origin -  tBodyGyroJerk-std()-Y
* tBodyGyroJerk_std_Z:   Origin -  tBodyGyroJerk-std()-Z
* tBodyAccMag_mean:   Origin -  tBodyAccMag-mean()
* tBodyAccMag_std:   Origin -  tBodyAccMag-std()
* tGravityAccMag_mean:   Origin -  tGravityAccMag-mean()
* tGravityAccMag_std:   Origin -  tGravityAccMag-std()
* tBodyAccJerkMag_mean:   Origin -  tBodyAccJerkMag-mean()
* tBodyAccJerkMag_std:   Origin -  tBodyAccJerkMag-std()
* tBodyGyroMag_mean:   Origin -  tBodyGyroMag-mean()
* tBodyGyroMag_std:   Origin -  tBodyGyroMag-std()
* tBodyGyroJerkMag_mean:   Origin -  tBodyGyroJerkMag-mean()
* tBodyGyroJerkMag_std:   Origin -  tBodyGyroJerkMag-std()
* fBodyAcc_mean_X:   Origin -  fBodyAcc-mean()-X
* fBodyAcc_mean_Y:   Origin -  fBodyAcc-mean()-Y
* fBodyAcc_mean_Z:   Origin -  fBodyAcc-mean()-Z
* fBodyAcc_std_X:   Origin -  fBodyAcc-std()-X
* fBodyAcc_std_Y:   Origin -  fBodyAcc-std()-Y
* fBodyAcc_std_Z:   Origin -  fBodyAcc-std()-Z
* fBodyAcc_meanFreq_X:   Origin -  fBodyAcc-meanFreq()-X
* fBodyAcc_meanFreq_Y:   Origin -  fBodyAcc-meanFreq()-Y
* fBodyAcc_meanFreq_Z:   Origin -  fBodyAcc-meanFreq()-Z
* fBodyAccJerk_mean_X:   Origin -  fBodyAccJerk-mean()-X
* fBodyAccJerk_mean_Y:   Origin -  fBodyAccJerk-mean()-Y
* fBodyAccJerk_mean_Z:   Origin -  fBodyAccJerk-mean()-Z
* fBodyAccJerk_std_X:   Origin -  fBodyAccJerk-std()-X
* fBodyAccJerk_std_Y:   Origin -  fBodyAccJerk-std()-Y
* fBodyAccJerk_std_Z:   Origin -  fBodyAccJerk-std()-Z
* fBodyAccJerk_meanFreq_X:   Origin -  fBodyAccJerk-meanFreq()-X
* fBodyAccJerk_meanFreq_Y:   Origin -  fBodyAccJerk-meanFreq()-Y
* fBodyAccJerk_meanFreq_Z:   Origin -  fBodyAccJerk-meanFreq()-Z
* fBodyGyro_mean_X:   Origin -  fBodyGyro-mean()-X
* fBodyGyro_mean_Y:   Origin -  fBodyGyro-mean()-Y
* fBodyGyro_mean_Z:   Origin -  fBodyGyro-mean()-Z
* fBodyGyro_std_X:   Origin -  fBodyGyro-std()-X
* fBodyGyro_std_Y:   Origin -  fBodyGyro-std()-Y
* fBodyGyro_std_Z:   Origin -  fBodyGyro-std()-Z
* fBodyGyro_meanFreq_X:   Origin -  fBodyGyro-meanFreq()-X
* fBodyGyro_meanFreq_Y:   Origin -  fBodyGyro-meanFreq()-Y
* fBodyGyro_meanFreq_Z:   Origin -  fBodyGyro-meanFreq()-Z
* fBodyAccMag_mean:   Origin -  fBodyAccMag-mean()
* fBodyAccMag_std:   Origin -  fBodyAccMag-std()
* fBodyAccMag_meanFreq:   Origin -  fBodyAccMag-meanFreq()
* fBodyBodyAccJerkMag_mean:   Origin -  fBodyBodyAccJerkMag-mean()
* fBodyBodyAccJerkMag_std:   Origin -  fBodyBodyAccJerkMag-std()
* fBodyBodyAccJerkMag_meanFreq:   Origin -  fBodyBodyAccJerkMag-meanFreq()
* fBodyBodyGyroMag_mean:   Origin -  fBodyBodyGyroMag-mean()
* fBodyBodyGyroMag_std:   Origin -  fBodyBodyGyroMag-std()
* fBodyBodyGyroMag_meanFreq:   Origin -  fBodyBodyGyroMag-meanFreq()
* fBodyBodyGyroJerkMag_mean:   Origin -  fBodyBodyGyroJerkMag-mean()
* fBodyBodyGyroJerkMag_std:   Origin -  fBodyBodyGyroJerkMag-std()
* fBodyBodyGyroJerkMag_meanFreq:   Origin -  fBodyBodyGyroJerkMag-meanFreq()
* angle_tBodyAccMean_gravity:   Origin -  angle(tBodyAccMean,gravity)
* angle_tBodyAccJerkMean_gravityMean:   Origin -  angle(tBodyAccJerkMean),gravityMean)
* angle_tBodyGyroMean_gravityMean:   Origin -  angle(tBodyGyroMean,gravityMean)
* angle_tBodyGyroJerkMean_gravityMean:   Origin -  angle(tBodyGyroJerkMean,gravityMean)
* angle_X_gravityMean:   Origin -  angle(X,gravityMean)
* angle_Y_gravityMean:   Origin -  angle(Y,gravityMean)
* angle_Z_gravityMean:   Origin -  angle(Z,gravityMean)

## File Name - summarizedData.Rdata
### Contents and Organization
Contains means of the 86 selected feature fields grouped by subject and activity.
### Units and Scale 
None of the 86 measures have any units as all the feature values are normalized data and range from -1 to 1.  Units of the original measures were not provided by the authors of the research.
All factors are floating point numbers from -1.0 to +1.0 with 8 significant digits.
### Field Descriptions
* SubjectID:   Origin -  Subject ID from the original tidied data.
* Activity:   Origin -  Activity Name from the original tidied data
* mean(tBodyAcc_mean_X):   Origin -  average of tBodyAcc-mean()-X
* mean(tBodyAcc_mean_Y):   Origin -  average of tBodyAcc-mean()-Y
* mean(tBodyAcc_mean_Z):   Origin -  average of tBodyAcc-mean()-Z
* mean(tBodyAcc_std_X):   Origin -  average of tBodyAcc-std()-X
* mean(tBodyAcc_std_Y):   Origin -  average of tBodyAcc-std()-Y
* mean(tBodyAcc_std_Z):   Origin -  average of tBodyAcc-std()-Z
* mean(tGravityAcc_mean_X):   Origin -  average of tGravityAcc-mean()-X
* mean(tGravityAcc_mean_Y):   Origin -  average of tGravityAcc-mean()-Y
* mean(tGravityAcc_mean_Z):   Origin -  average of tGravityAcc-mean()-Z
* mean(tGravityAcc_std_X):   Origin -  average of tGravityAcc-std()-X
* mean(tGravityAcc_std_Y):   Origin -  average of tGravityAcc-std()-Y
* mean(tGravityAcc_std_Z):   Origin -  average of tGravityAcc-std()-Z
* mean(tBodyAccJerk_mean_X):   Origin -  average of tBodyAccJerk-mean()-X
* mean(tBodyAccJerk_mean_Y):   Origin -  average of tBodyAccJerk-mean()-Y
* mean(tBodyAccJerk_mean_Z):   Origin -  average of tBodyAccJerk-mean()-Z
* mean(tBodyAccJerk_std_X):   Origin -  average of tBodyAccJerk-std()-X
* mean(tBodyAccJerk_std_Y):   Origin -  average of tBodyAccJerk-std()-Y
* mean(tBodyAccJerk_std_Z):   Origin -  average of tBodyAccJerk-std()-Z
* mean(tBodyGyro_mean_X):   Origin -  average of tBodyGyro-mean()-X
* mean(tBodyGyro_mean_Y):   Origin -  average of tBodyGyro-mean()-Y
* mean(tBodyGyro_mean_Z):   Origin -  average of tBodyGyro-mean()-Z
* mean(tBodyGyro_std_X):   Origin -  average of tBodyGyro-std()-X
* mean(tBodyGyro_std_Y):   Origin -  average of tBodyGyro-std()-Y
* mean(tBodyGyro_std_Z):   Origin -  average of tBodyGyro-std()-Z
* mean(tBodyGyroJerk_mean_X):   Origin -  average of tBodyGyroJerk-mean()-X
* mean(tBodyGyroJerk_mean_Y):   Origin -  average of tBodyGyroJerk-mean()-Y
* mean(tBodyGyroJerk_mean_Z):   Origin -  average of tBodyGyroJerk-mean()-Z
* mean(tBodyGyroJerk_std_X):   Origin -  average of tBodyGyroJerk-std()-X
* mean(tBodyGyroJerk_std_Y):   Origin -  average of tBodyGyroJerk-std()-Y
* mean(tBodyGyroJerk_std_Z):   Origin -  average of tBodyGyroJerk-std()-Z
* mean(tBodyAccMag_mean):   Origin -  average of tBodyAccMag-mean()
* mean(tBodyAccMag_std):   Origin -  average of tBodyAccMag-std()
* mean(tGravityAccMag_mean):   Origin -  average of tGravityAccMag-mean()
* mean(tGravityAccMag_std):   Origin -  average of tGravityAccMag-std()
* mean(tBodyAccJerkMag_mean):   Origin -  average of tBodyAccJerkMag-mean()
* mean(tBodyAccJerkMag_std):   Origin -  average of tBodyAccJerkMag-std()
* mean(tBodyGyroMag_mean):   Origin -  average of tBodyGyroMag-mean()
* mean(tBodyGyroMag_std):   Origin -  average of tBodyGyroMag-std()
* mean(tBodyGyroJerkMag_mean):   Origin -  average of tBodyGyroJerkMag-mean()
* mean(tBodyGyroJerkMag_std):   Origin -  average of tBodyGyroJerkMag-std()
* mean(fBodyAcc_mean_X):   Origin -  average of fBodyAcc-mean()-X
* mean(fBodyAcc_mean_Y):   Origin -  average of fBodyAcc-mean()-Y
* mean(fBodyAcc_mean_Z):   Origin -  average of fBodyAcc-mean()-Z
* mean(fBodyAcc_std_X):   Origin -  average of fBodyAcc-std()-X
* mean(fBodyAcc_std_Y):   Origin -  average of fBodyAcc-std()-Y
* mean(fBodyAcc_std_Z):   Origin -  average of fBodyAcc-std()-Z
* mean(fBodyAcc_meanFreq_X):   Origin -  average of fBodyAcc-meanFreq()-X
* mean(fBodyAcc_meanFreq_Y):   Origin -  average of fBodyAcc-meanFreq()-Y
* mean(fBodyAcc_meanFreq_Z):   Origin -  average of fBodyAcc-meanFreq()-Z
* mean(fBodyAccJerk_mean_X):   Origin -  average of fBodyAccJerk-mean()-X
* mean(fBodyAccJerk_mean_Y):   Origin -  average of fBodyAccJerk-mean()-Y
* mean(fBodyAccJerk_mean_Z):   Origin -  average of fBodyAccJerk-mean()-Z
* mean(fBodyAccJerk_std_X):   Origin -  average of fBodyAccJerk-std()-X
* mean(fBodyAccJerk_std_Y):   Origin -  average of fBodyAccJerk-std()-Y
* mean(fBodyAccJerk_std_Z):   Origin -  average of fBodyAccJerk-std()-Z
* mean(fBodyAccJerk_meanFreq_X):   Origin -  average of fBodyAccJerk-meanFreq()-X
* mean(fBodyAccJerk_meanFreq_Y):   Origin -  average of fBodyAccJerk-meanFreq()-Y
* mean(fBodyAccJerk_meanFreq_Z):   Origin -  average of fBodyAccJerk-meanFreq()-Z
* mean(fBodyGyro_mean_X):   Origin -  average of fBodyGyro-mean()-X
* mean(fBodyGyro_mean_Y):   Origin -  average of fBodyGyro-mean()-Y
* mean(fBodyGyro_mean_Z):   Origin -  average of fBodyGyro-mean()-Z
* mean(fBodyGyro_std_X):   Origin -  average of fBodyGyro-std()-X
* mean(fBodyGyro_std_Y):   Origin -  average of fBodyGyro-std()-Y
* mean(fBodyGyro_std_Z):   Origin -  average of fBodyGyro-std()-Z
* mean(fBodyGyro_meanFreq_X):   Origin -  average of fBodyGyro-meanFreq()-X
* mean(fBodyGyro_meanFreq_Y):   Origin -  average of fBodyGyro-meanFreq()-Y
* mean(fBodyGyro_meanFreq_Z):   Origin -  average of fBodyGyro-meanFreq()-Z
* mean(fBodyAccMag_mean):   Origin -  average of fBodyAccMag-mean()
* mean(fBodyAccMag_std):   Origin -  average of fBodyAccMag-std()
* mean(fBodyAccMag_meanFreq):   Origin -  average of fBodyAccMag-meanFreq()
* mean(fBodyBodyAccJerkMag_mean):   Origin -  average of fBodyBodyAccJerkMag-mean()
* mean(fBodyBodyAccJerkMag_std):   Origin -  average of fBodyBodyAccJerkMag-std()
* mean(fBodyBodyAccJerkMag_meanFreq):   Origin -  average of fBodyBodyAccJerkMag-meanFreq()
* mean(fBodyBodyGyroMag_mean):   Origin -  average of fBodyBodyGyroMag-mean()
* mean(fBodyBodyGyroMag_std):   Origin -  average of fBodyBodyGyroMag-std()
* mean(fBodyBodyGyroMag_meanFreq):   Origin -  average of fBodyBodyGyroMag-meanFreq()
* mean(fBodyBodyGyroJerkMag_mean):   Origin -  average of fBodyBodyGyroJerkMag-mean()
* mean(fBodyBodyGyroJerkMag_std):   Origin -  average of fBodyBodyGyroJerkMag-std()
* mean(fBodyBodyGyroJerkMag_meanFreq):   Origin -  average of fBodyBodyGyroJerkMag-meanFreq()
* mean(angle_tBodyAccMean_gravity):   Origin -  average of angle(tBodyAccMean,gravity)
* mean(angle_tBodyAccJerkMean_gravityMean):   Origin -  average of angle(tBodyAccJerkMean),gravityMean)
* mean(angle_tBodyGyroMean_gravityMean):   Origin -  average of angle(tBodyGyroMean,gravityMean)
* mean(angle_tBodyGyroJerkMean_gravityMean):   Origin -  average of angle(tBodyGyroJerkMean,gravityMean)
* mean(angle_X_gravityMean):   Origin -  average of angle(X,gravityMean)
* mean(angle_Y_gravityMean):   Origin -  average of angle(Y,gravityMean)
* mean(angle_Z_gravityMean):   Origin -  average of angle(Z,gravityMean)

## References and Acknowledgement
All input data and descriptions copyright of Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.  Reference - Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012.  Downloaded from https://d396qusza40orc.cloudfront.net/getdata/projectfiles/ on Friday, ?May ?12, ?2017, ??3:16:13 PM US Eastern Time.


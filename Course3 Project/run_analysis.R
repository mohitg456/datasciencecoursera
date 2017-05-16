# This script merges and tidies the features data in the test and train sets.
# It then creates a summary data set with average of each variable by subjectg and activity
#
# Both files are saved as .Rdata files.
# Samples of both files are shown using View
#
# Description of Input Data:  
# Each file has 561 measure/feature-values per line.  
# Each line in each file corresponds to one observation of a subject performing one activity.
# Which subject and which activity a given line/observation corresponds to, is in the subject and 
# "Y" files for the corresponding set which contain one line per line of data in each data file
#
#
# * * * IMPORTANT * * *
# Before running this script, working directory should be set to the folder where the UCI HAR Dataset folder was unzipped
# Output file will be a data frame in .Rdata format saved in a subfolder "output" in the working directory


library(dplyr)
library(tidyr) 


# Stop if the working direcory is incorrect
if (!dir.exists("UCI HAR Dataset")) {
	stop("Please first set wd to the folder which contains the unzipped 'UCI HAR Dataset' subfolder")
}

tidiedFeatures <- tbl_df(data.frame())     # empty dplyr tbl_df hold tidied data

activity_labels_file_name <- "UCI HAR Dataset/activity_labels.txt"
test_subject_file_name    <- "UCI HAR Dataset/test/subject_test.txt"
test_activity_file_name   <- "UCI HAR Dataset/test/y_test.txt"
train_subject_file_name   <- "UCI HAR Dataset/train/subject_train.txt"
train_activity_file_name  <- "UCI HAR Dataset/train/y_train.txt"
feature_file_name         <- "UCI HAR Dataset/features.txt"

# read the files containing sequence of subjects and sequence of activities. 
# Convert to vector using [, 1].
testSubjectSeq <-  read.table(test_subject_file_name,  colClasses="integer") [, 1]
testActivitySeq<-  read.table(test_activity_file_name, colClasses="integer") [, 1]
trainSubjectSeq <- read.table(train_subject_file_name,  colClasses="integer") [, 1]
trainActivitySeq<- read.table(train_activity_file_name, colClasses="integer") [, 1]

# Read in the activity code lookup file (activity id, activity name)
activity_labels <- read.table(activity_labels_file_name, col.names=c("ID", "Name"))

# Convert both activity sequences to activity names (walking, laying etc.)
testActivitySeqNames <- activity_labels$Name[testActivitySeq]  
trainActivitySeqNames<- activity_labels$Name[trainActivitySeq]

# Read the feature names file.
# select feature names that contain "mean" or "std" (in any mix of upper or lower case)
# replace all punctuation characters with underscore
# (ensure that two or more adjacent punctuations are collapsed to a single underscore)
# remove underscores at end of a feature name 

feat_names <- read.table(feature_file_name, header=FALSE, 
						 stringsAsFactors=FALSE, col.names=c("ID", "Name"))

meanStdCols<- grepl("mean|std",feat_names$Name, ignore.case = TRUE)

feat_names$Name <- gsub("[[:punct:]]+", "_", feat_names$Name)
feat_names$Name <- gsub("_$", "", feat_names$Name)

################################################################################################
# Define a function that will be called twice (once each for test and train) and will  
# load, tidy and return back the selected data one by one 
################################################################################################
ReadTidyDataFiles <- function (test_or_train) {
    # test_or_train is "test" or "train"
    # Description: 
	#   1. reads the features data from given file 
	#   2. Adds additional columns - Group=(train/test), Observation=line# in file, SubjectID(1-30) and Activity name
	#   3. Selects only the fields we want
	#   4. appends to the tidiedFeatures table
	# Returns:
	#   Data frame containing the tidied test or train data.  Calling routine must merge the two

		
	# read the features data file (chosen fields only)
	# add 4 columns - Group=(train/test), Observation=line# in file, SubjectID(1-30) and Activity name
	# select nly the fields we identified, drop all others

	data_file_name <- paste0("UCI HAR Dataset/", test_or_train, "/x_",test_or_train, ".txt")
	print (data_file_name)

		read.table(data_file_name, header=F, col.names=feat_names$Name
		)   %>% 
	    mutate(  
			Group=test_or_train, 
			Observation=seq_along(Group), 
	    	SubjectID= if (test_or_train=="test") testSubjectSeq       else trainSubjectSeq, 
	    	Activity=  if (test_or_train=="test") testActivitySeqNames else trainActivitySeqNames 
    	) %>%
		select (Group:Activity, one_of(feat_names$Name[meanStdCols]) )  %>%
		return()
}
# End of function ReadTidyDataFiles()


for (test_or_train in c("test", "train")) {
	tmp <- ReadTidyDataFiles(test_or_train)
	tidiedFeatures <- rbind(tidiedFeatures, tmp)
}

# save the output to an R object that can be loaded back
if (!dir.exists("output")) dir.create ("output")
save(tidiedFeatures, file="output/tidiedFeatures.Rdata")

write.table(tidiedFeatures, "tidiedFeatures.txt", row.names = F)



##################################################################################
## Now create summarized file of means of all data columns by Activity amd Subject
##################################################################################

# first build a character vectore of the form "mean(var1) mean(var2).....)
cc<-paste0("mean(", feat_names$Name[meanStdCols], ")")

# group by activity and subject and summarize mean of all 86 feature variables
# use the SE function "summarize_" so we can pass a list of columns we want means for 
# instead of typing mean(var1), mean(var2).... 86 times

summarizedData <- group_by(tidiedFeatures, Activity, SubjectID) %>%
	summarize_(.dots=cc)
	  

if (!dir.exists("output")) dir.create ("output")
save(summarizedData, file="output/summarizedData.Rdata")

write.table(summarizedData, "summarizedData.txt", row.names = F)

###show a both output sets
View(sample_n(as.data.frame(tidiedFeatures), 30), "SAMPLE of tidied data"  )
View(summarizedData, "Summarized Data"  )


### cleanup all data variables except the two output tables 
rm(list=grep("tidiedFeatures|summarizedData", ls(), invert=T, value=T))
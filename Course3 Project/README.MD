# README
## Script Name: run_analysis.R
### Prerequisites for Running
1. Ensure that you have downloaded and unzipped the “UCI HAR Dataset” folder from the project site.
2. Ensure that the working directory is set to the folder in which the “UCI HAR Dataset” subfolder resides.
3. Ensure you have write access to the WD as output files will be written to it.
### How to run it
* Set the working directory correctly and run the script.  Was tested only in RStudio but should run in R Console as well.
### Input 
* Needs all the files provided on the research web site.  All file names and locations must be unchanged in folder “UCI HAR Dataset”.  Files in the Inertial Signals folder are not used.
### Output
Creates a folder “output” in the WD and creates two files in .Rdata format.  These files can be read-in using the R load command.  It also opens a sample of the tidied data frame in View.  Also opens the summarized data frame in View. 
* tidiedFeatures.Rdata – a data frame containing the tidied data from test and train.
* summarizedData.Rdata – Summarized data.
### Description of the Processing
* This script merges and tidies the features data in the test and train sets.  It then creates a summary data set with average of each variable by subject and activity.   Both files are data frame which are saved as .Rdata files in a folder called “output” (created if it does not exist).  Samples of both files are shown using View. 

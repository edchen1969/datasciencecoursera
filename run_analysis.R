# run_analysis.r File Description:
#
# This code will perform the following steps on the UCI HAR Dataset downloaded from: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Preliminaries
print("Checking files and packages ............ ")
# check data folder
if(!file.exists("data")){dir.create("data")}

# load packages "downloader" and "plyr"
list.of.packages   <- c("downloader", "plyr")
    new.packages   <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)
library("downloader")

# download, unzip, delete zip file
zip.URL    <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zip.local  <- "./data/human_activity_data.zip"

if(!file.exists(zip.local)){
   download(zip.URL, dest=zip.local, mode="wb") 
      unzip(zip.local, exdir = "./data")
     unlink(zip.local)
}
#set working directory to the location where the UCI HAR Dataset was unzipped
original_wd = getwd()
setwd('./data/UCI HAR Dataset/')

# 1. Merges the training and the test sets to create one data set
print("Merging the training and the test sets . ")
# Read in the feature and activity data from files
features.df       <- read.table('./features.txt',header=FALSE)
activity.label.df <- read.table('./activity_labels.txt',header=FALSE)

# Assigin column names to the data
colnames(activity.label.df) <- c('activityId','activityType')
print("Preparing the training set ............. ")
# Read in the training data from files
subject.train.df  <- read.table('./train/subject_train.txt',header=FALSE) 
x.train.df        <- read.table('./train/x_train.txt',header=FALSE)
y.train.df        <- read.table('./train/y_train.txt',header=FALSE)

# Assigin column names to the imported data 
colnames(subject.train.df)  <- "subjectId"
colnames(x.train.df)        <- features.df[,2]
colnames(y.train.df)        <- "activityId"

# combine training data
training.df <- cbind( subject.train.df, 
                      y.train.df,
                      x.train.df
                     )

print("Preparing the test set ................. ")
# Read in the test data from files
subject.test.df <- read.table('./test/subject_test.txt',header=FALSE)
x.test.df       <- read.table('./test/x_test.txt',header=FALSE)
y.test.df       <- read.table('./test/y_test.txt',header=FALSE)

# Assign column names to the imported data 
colnames(subject.test.df) <- "subjectId"
colnames(x.test.df)       <- features.df[,2]
colnames(y.test.df)       <- "activityId"


# merge test data
test.df <- cbind( subject.train.df, 
                  y.train.df,
                  x.train.df
)
print("Merging the training and the test sets . ")
# Combine training and test data
entire.df <- rbind(training.df, test.df)

print("Extracting columns ..................... ")
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

columns.wanted <- grep("-mean\\(\\)|-std\\(\\)", features.df[, 2])
columns.wanted <- columns.wanted + 2  # the first 2 columns is subject ID and activity ID
columns.wanted <-  c(c(1,2), columns.wanted)  # add subject ID and activity ID back
analyzed.df <- entire.df[,columns.wanted]

print("Adding activity names .................. ")
# 3. Uses descriptive activity names to name the activities in the data set.
library(plyr)
analyzed.df <- arrange(join(analyzed.df, activity.label.df), activityId)

print("Adjusting column labels ................ ")
# 4. Appropriately label the data set with descriptive activity names. 
analyzed.columns  = colnames(analyzed.df)
for (i in 1:length(analyzed.columns)) 
{
    analyzed.columns[i] = gsub("\\()","",analyzed.columns[i])
    analyzed.columns[i] = gsub("-std$","StdDev",analyzed.columns[i])
    analyzed.columns[i] = gsub("-mean","Mean",analyzed.columns[i])
    analyzed.columns[i] = gsub("^(t)","time",analyzed.columns[i])
    analyzed.columns[i] = gsub("^(f)","freq",analyzed.columns[i])
    analyzed.columns[i] = gsub("([Gg]ravity)","Gravity",analyzed.columns[i])
    analyzed.columns[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",analyzed.columns[i])
    analyzed.columns[i] = gsub("[Gg]yro","Gyro",analyzed.columns[i])
    analyzed.columns[i] = gsub("AccMag","AccMagnitude",analyzed.columns[i])
    analyzed.columns[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",analyzed.columns[i])
    analyzed.columns[i] = gsub("JerkMag","JerkMagnitude",analyzed.columns[i])
    analyzed.columns[i] = gsub("GyroMag","GyroMagnitude",analyzed.columns[i])
}

colnames(analyzed.df) = analyzed.columns
write.table(analyzed.df, "./human_activity_data_merged.txt")

print("Finalizing tidy data set ............... ")
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# drop the activityType column
analyzed.df  <- analyzed.df[,names(analyzed.df) != 'activityType']

# aggregate the mean of each variable for each activity and each subject
#tidy.df    <- aggregate(analyzed.df[,names(analyzed.df) != c('activityId','subjectId')],by=list(activityId=analyzed.df$activityId,subjectId = analyzed.df$subjectId),mean)
tidy.df    <- aggregate(analyzed.df[,3:ncol(analyzed.df)],
                        by=list(activityId = analyzed.df$activityId,subjectId = analyzed.df$subjectId), 
                        FUN=mean)

# join the acitvity names back
tidy.df    <- arrange(join(tidy.df, activity.label.df), activityId)
write.table(tidy.df, './human_activity_data_tidy.txt',row.names=FALSE,sep='\t')

print("tidy data set has been generated.")
setwd(original_wd)
require(knitr)
require(markdown)

knit("Codebook.Rmd", output="codebook.md", encoding="ISO8859-1", quiet=TRUE)
markdownToHTML("codebook.md", "codebook.html")


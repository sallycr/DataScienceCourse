# read test data 

library(matrixStats)
library(dplyr)

# Function : readData 
# Description : reading measurement data from input file with label files
# Input : 
#    - measurementFile : measure data of each observation 
#    - activityFile : activity label of each observation 
#    - subjectFile : subject id of each observation 
# Output : 
#    - Dataframe of (subject_id, activity, measurement varriables)
readData <- function(dataPath, measurementFile, activityFile, subjectFile){
    # files to read 
    activityLabelFile <- paste(dataPath,"activity_labels.txt", sep="/")
    featureFile <- paste(dataPath,"features.txt", sep="/")
    measurementFile <- paste(dataPath, measurementFile, sep="/")
    activityFile <- paste(dataPath, activityFile, sep="/")
    subjectFile <- paste(dataPath, subjectFile, sep="/")
    
    # load label information for activities and features
    activityLabel <- read.table(activityLabelFile, sep=" ", 
                        header=FALSE, col.names=c("activity_id", "activity"))
    featureLabel <- read.table(featureFile, sep=" ", 
                        header=FALSE, col.names=c("id", "name"))
    
    # change the labels to lowercase 
    activityLabel$activity <- tolower(activityLabel$activity)
    featureLabel$name <- tolower(featureLabel$name)
    
    # load data from the input file location 
    measurementData <- read.table(measurementFile, header=FALSE)
    activityData <- read.table(activityFile, col.names=c("activity_id"), header=FALSE)
    subjectData <- read.table(subjectFile, col.names=c("subject_id"), header=FALSE)
    
    # set names with minimum string replacement that leads an error in dplyr.  
    measurementData <- setNames(measurementData, featureLabel$name)

    activityData$activity <- factor(activityData$activity_id, labels=activityLabel$activity)
    cbind(subjectData, activityData, measurementData)
}

# Function : laodTrainingAndTestData 
# Description : load Training and Test data from the project data directory
# Input : 
#    - dataPath : project data path
# Output : 
#    - Data table of training and testing data
loadTrainingAndTestData <- function(dataPath) {
    testData <- readData(dataPath, 
                         "test/x_test.txt", 
                         "test/y_test.txt", 
                         "test/subject_test.txt")
    trainData <- readData(dataPath,
                          "train/x_train.txt", 
                          "train/y_train.txt", 
                          "train/subject_train.txt")
    rbind(trainData, testData)
}

# Function : selectData 
# Description : select the mean and std measurement varaibles only  
# Input : 
#    - wearableDt : full data table that inlcudes all measurements 
# Output : 
#    - datatable with mean and std measurements along with subject id and acitivy
selectMeanAndStdMeasurement <- function(wearableDt) {
    colNames <- grep("(mean|std)\\(\\)", names(wearableDt), value=TRUE, 
                             ignore.case=TRUE)
    selectedDt <- wearableDt[c("subject_id","activity",colNames)]
    
    # replace - and () to _ for clean naming.  
    names <- names(selectedDt)
    names <- gsub("\\(\\)","", names)
    names <- gsub("-", "_", names)
    names <- gsub("_(std|mean)_(x|y|z)","_\\2_\\1", names)
    
    setNames(selectedDt, names)
}

# Function : readData 
# Description : reading measurement data from input file with label files
# Input : 
#    - measurementFile : measure data of each observation 
#    - activityFile : activity label of each observation 
#    - subjectFile : subject id of each observation 
# Output : 
#    - Dataframe of (subject_id, activity, measurement varriables)
createTidyData <- function(cleanData) {
    cleanData %>% 
        group_by(subject_id, activity) %>% 
        summarise_each(funs(mean))
}

# Function : downloadAndUnzipProjectData 
# Description : download wearable project data and extract to local path
# Input : 
#    - url : zip file url for wearable project data 
# Output : 
#    - local project path
downloadAndUnzipProjectData <- function(url = 
       "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"){
    zipFile <- "project_data.zip"
    projectPath <- "./project_data"

    download.file(url,zipFile)
    unzip (zipFile, exdir = projectPath)
    paste(projectPath,"UCI HAR Dataset", sep="/")
}

runWearableAnalysis <- function() {
    dataPath <- downloadAndUnzipProjectData()
    fullData <- loadTrainingAndTestData(dataPath)
    cleanData <- selectMeanAndStdMeasurement(fullData)
    tidyDataOfSubjectAcitivity <- createTidyData(cleanData)
    
    write.csv(tidyDataOfSubjectAcitivity, file="wearableData.csv")
    write.table(tidyDataOfSubjectAcitivity, row.name=FALSE, file="wearableData.txt")    
}


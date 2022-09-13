## Necessary Libraries to de Assignment
library(dplyr)
library(data.table)

features_names <- read.table("UCI HAR Dataset/features.txt", header = F)
activity_levels <- read.table("UCI HAR Dataset/activity_labels.txt", header = F)

## Loading test data
features_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = F)
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = F)
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = F)
features_test <- features_test %>% mutate(activity = activity_test, subject = subject_test)

# loading train data
features_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = F)
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = F)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = F)
features_train <- features_train %>% mutate(activity = activity_train, subject = subject_train)

# Merge test and train data
features_complete <- rbind(features_test, features_train)

# Defining activity by names
features_complete$activity <- as.factor(ifelse(features_complete$activity == 1, activity_levels$V2[1],
                                               ifelse(features_complete$activity == 2, activity_levels$V2[2],
                                                      ifelse(features_complete$activity == 3, activity_levels$V2[3],
                                                             ifelse(features_complete$activity == 4, activity_levels$V2[4],
                                                                    ifelse(features_complete$activity == 5, activity_levels$V2[5], activity_levels$V2[6]))))))

# tidy data set with the average of each variable for each activity and each subject
tidyDataTable <- group_by(features_complete, activity, subject)
tidyDataTable<-tidyDataTable%>%summarise_all(mean)


## Changing to descriptive variable names
features_names$V2 <- gsub("BodyBody","Body", features_names$V2)
features_names$V2 <- gsub("tBody","time.domain.body", features_names$V2)
features_names$V2 <- gsub("tGravity","time.domain.gravity",features_names$V2)
features_names$V2 <- gsub("fBody","FastFourierTransform.domain.body",features_names$V2)
features_names$V2 <- gsub("fGravity","FastFourierTransform.domain.gravity",features_names$V2)
features_names$V2 <- gsub("Acc",".accelerometer",features_names$V2)
features_names$V2 <- gsub("Gyro",".gyroscope",features_names$V2)
features_names$V2 <- gsub("Jerk",".JerkSignal",features_names$V2)
features_names$V2 <- gsub("Mag",".EuclideanNormDimensionalMagnitude",features_names$V2)
features_names$V2 <- gsub("mean()", "MeanValue",features_names$V2)
features_names$V2 <- gsub("std()", "StandardDeviation",features_names$V2)
features_names$V2 <- gsub("mad()", "MedianAbsoluteDeviation",features_names$V2)
features_names$V2 <- gsub("max()", "LargestValueArray",features_names$V2)
features_names$V2 <- gsub("min()", "SmallestValueArray",features_names$V2)
features_names$V2 <- gsub("sma()", "SignalMagnitudeArea",features_names$V2)
features_names$V2 <- gsub("energy()", "EnergyMeasure",features_names$V2)
features_names$V2 <- gsub("iqr()", "InterquartileRange",features_names$V2)
features_names$V2 <- gsub("entropy()", "SignalEntropy",features_names$V2)
features_names$V2 <- gsub("arCoeff()", "AutorregresionCoefficientsBurgOrder4",features_names$V2)
features_names$V2 <- gsub("correlation()","CorrelationCoefficientBetween2Signals",features_names$V2)
features_names$V2 <- gsub("maxInds()","LargestMagnitudeIndexFrecuencyComponent",features_names$V2)
features_names$V2 <- gsub("meanFreq()","WeightedAverageFrequencyToObtainMeanFrequency",features_names$V2)
features_names$V2 <- gsub("skewness()", "FrecuencyDomainSignalSkewness",features_names$V2)
features_names$V2 <- gsub("kurtosis()", "FrecuencyDomainSignalKurtosis",features_names$V2)
features_names$V2 <- gsub("bandsEnergy()", "FrecuencyIntervalEnergy64binsFFT",features_names$V2)
features_names$V2 <- gsub("angle()", "AngleBetweenVectors",features_names$V2)
names(features_complete)[1:561] <- features_names$V2
names(tidyDataTable)[3:563] <- features_names$V2

# Extract measurements on the mean and standard deviation for each measurement
measureMeanSD <- select(features_complete,matches('MeanValue|StandardDeviation'))

write.table(tidyDataTable, "tidyDataTable.txt", row.name=FALSE)

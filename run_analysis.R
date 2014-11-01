##########################################################################################################
## Course: Getting and Cleaning Data 
## Student: Chhatra Karki

# Deliverables
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#Read different data items to local variables
subjects = read.table("UCI HAR Dataset/train/subject_train.txt", col.names=c("subject_id"))
x_train = read.table("UCI HAR Dataset/train/X_train.txt")
y_train = read.table("UCI HAR Dataset/train/y_train.txt", col.names=c("activity_id"))
subject_test = read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject_id"))
X_test = read.table("UCI HAR Dataset/test/X_test.txt")
y_test = read.table("UCI HAR Dataset/test/y_test.txt", col.names=c("activity_id")) 
features = read.table("UCI HAR Dataset/features.txt", col.names=c("feature_id", "feature_label"),)  
activity_labels = read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activity_id", "activity_label"),)

# Assing IDs
subjects$ID <- as.numeric(rownames(subjects))
x_train$ID <- as.numeric(rownames(x_train))
y_train$ID <- as.numeric(rownames(y_train))
subject_test$ID <- as.numeric(rownames(subject_test))
X_test$ID <- as.numeric(rownames(X_test))
y_test$ID <- as.numeric(rownames(y_test))


###################################################################################
#Deliverable 1: Merges the training and the test sets to create one data set."
###################################################################################

# Now we read data, let's do some merge activities
train <- merge(subjects, y_train, all=TRUE)
train <- merge(train, x_train, all=TRUE)
test <- merge(subject_test, y_test, all=TRUE) 
test <- merge(test, X_test, all=TRUE) 

#combine train and test
test_train <- rbind(train, test)

######################################################################################################
#Deliverable 2: Extracts only the measurements on the mean and standard deviation for each measurement.
######################################################################################################
 
selected_features <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ]

test_train2 <- test_train[, c(c(1, 2, 3), selected_features$feature_id + 3) ]

#########################################################################################
#Deliverable 3: Uses descriptive activity names to name the activities in the data set.
#########################################################################################
test_train3 = merge(test_train2, activity_labels)

###################################################################################
#Deliverable 4: Appropriately labels the data set with descriptive activity names.
###################################################################################

selected_features$feature_label = gsub("\\(\\)", "", selected_features$feature_label)
selected_features$feature_label = gsub("-", ".", selected_features$feature_label)

#Loop through features data and assign columns
for (i in 1:length(selected_features$feature_label)) {
        colnames(test_train3)[i + 3] <- selected_features$feature_label[i]
}
candidate_data = test_train3

###################################################################################################################################
#Deliverable 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
###################################################################################################################################
columns <- c("ID","activity_label")
candidate_data2 <- candidate_data[,!(names(candidate_data) %in% columns)]

final_data <-aggregate(candidate_data2, by=list(subject = candidate_data2$subject_id, activity = candidate_data2$activity_id), FUN=mean, na.rm=TRUE)
columns <- c("subject","activity")
final_data <- final_data[,!(names(final_data) %in% columns)]
final_data = merge(final_data, activity_labels)

#Finally, write all cleaned up data to a csv file
write.csv(file="tidydata_final.csv", x=final_data)
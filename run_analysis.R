# Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table('UCI HAR Dataset/features.txt')
keptfeatures<-features[grepl("mean()",features$V2,fixed = TRUE)|grepl("std()",features$V2,fixed = TRUE),]

train_X<-read.table("UCI HAR Dataset/train/X_train.txt")
train_Y<-read.table("UCI HAR Dataset/train/Y_train.txt")
train_subject<-read.table("UCI HAR Dataset/train/subject_train.txt")

test_X<-read.table("UCI HAR Dataset/test/X_test.txt")
test_Y<-read.table("UCI HAR Dataset/test/Y_test.txt")
test_subject<-read.table("UCI HAR Dataset/test/subject_test.txt")

# Merges the training and the test sets to create one data set.

# Uses descriptive activity names to name the activities in the data set

# Appropriately labels the data set with descriptive variable names. 
headers<-names(combined_set)
headers<-gsub("Acc","Accelerometer",headers)
headers<-gsub("Gyro","Gyroscope",headers)
headers<-gsub("Mag","Magnitude",headers)
headers<-gsub("-",".",headers)
headers<-gsub("\\(|\\)","",headers)
headers<-gsub("BodyBody","Body",headers)
headers<-gsub("tBody","TimeBody",headers)
headers<-gsub("fBody","FrequencyBody",headers)
headers<-gsub("tGravity","TimeGravity",headers)
headers<-gsub("fGravity","FrequencyGravity",headers)
headers<-gsub("mean","Mean",headers)
headers<-gsub("std","Std",headers)
names(combined_set)<-headers

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
combined_set_mean <- aggregate(.~Subject+Activity,data = combined_set, FUN=mean)
write.table(combined_set_mean,"HARData_Proj.txt", row.names= FALSE)
# Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table('UCI HAR Dataset/features.txt')
kept_features<-features[grepl("mean()",features[, 2],fixed = TRUE)|grepl("std()",features[, 2],fixed = TRUE),]

train_X<-read.table("UCI HAR Dataset/train/X_train.txt")
train_Y<-read.table("UCI HAR Dataset/train/Y_train.txt")
train_subject<-read.table("UCI HAR Dataset/train/subject_train.txt")

test_X<-read.table("UCI HAR Dataset/test/X_test.txt")
test_Y<-read.table("UCI HAR Dataset/test/Y_test.txt")
test_subject<-read.table("UCI HAR Dataset/test/subject_test.txt")

# Uses descriptive activity names to name the activities in the data set
activity_labels<-read.table("UCI HAR Dataset/activity_labels.txt")

getactivitylabel<-function(activity_data, activity_labels){
  activity_data<-cbind(activity_data, c(1:nrow(activity_data)))
  activity_label_list<-merge(activity_data, activity_labels, by = "V1", All.x=TRUE, sort=FALSE)
  names(activity_label_list)<-c("ActivityId","Index","Activity")
  activity_label_list<- activity_label_list[order(activity_label_list$Index),]
  return(activity_label_list)
}

train_Y_labeled<-getactivitylabel(train_Y,activity_labels)
test_Y_labeled<-getactivitylabel(test_Y,activity_labels)

# Merges the training and the test sets to create one data set.
mergedata<-function(dataset, kept_features, subject, activity, type){
  output<-matrix()
  #only retrieve mean and std columns
  dataset<-dataset[,kept_features[,2]]
  output<-cbind(dataset,activity[,"Activity"])
  output<-cbind(output, subject)
  output<-cbind(output, type)
  names(output)<-c(as.character(kept_features[, 2]),"Activity","Subject","Type")
  return(output)
}
train_data<-mergedata(train_X,kept_features,train_subject,train_Y_labeled,"train")
test_data<-mergedata(test_X,kept_features,test_subject,test_Y_labeled,"test")
combined_set<-rbind(train_data,test_data)

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
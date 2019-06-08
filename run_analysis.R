library(readtext)
library(data.table)
library(dplyr)
library(reshape2)

#File Dir
file_x_train<-"C:/Users/chenw/OneDrive/Documents/Data Science_Coursera/Module 3. - Getting and Cleaning Data/UCI HAR Dataset/train/X_train.txt"
file_x_test<-"C:/Users/chenw/OneDrive/Documents/Data Science_Coursera/Module 3. - Getting and Cleaning Data/UCI HAR Dataset/test/X_test.txt"
file_y_train<-"C:/Users/chenw/OneDrive/Documents/Data Science_Coursera/Module 3. - Getting and Cleaning Data/UCI HAR Dataset/train/y_train.txt"
file_y_test<-"C:/Users/chenw/OneDrive/Documents/Data Science_Coursera/Module 3. - Getting and Cleaning Data/UCI HAR Dataset/test/y_test.txt"
var<-"C:/Users/chenw/OneDrive/Documents/Data Science_Coursera/Module 3. - Getting and Cleaning Data/UCI HAR Dataset/features.txt"
subject_train<-"C:/Users/chenw/OneDrive/Documents/Data Science_Coursera/Module 3. - Getting and Cleaning Data/UCI HAR Dataset/train/subject_train.txt"
subject_test<-"C:/Users/chenw/OneDrive/Documents/Data Science_Coursera/Module 3. - Getting and Cleaning Data/UCI HAR Dataset/test/subject_test.txt"

#Variable Name
var_name<-read.table(var)
var_name<-var_name[,2]

#Import training and testing data
data_x_train<-data.table(read.table(file_x_train, col.names = var_name))
data_x_test<-data.table(read.table(file_x_test, col.names = var_name))
data_y_train<-read.table(file_y_train)
data_y_test<-read.table(file_y_test)
data_subject_train<-read.table(subject_train)
data_subject_test<-read.table(subject_test)

#Adding label to data set
data_x_train<-mutate(data_x_train,label=data_y_train[,1],subject=data_subject_train[,1])
data_x_test<-mutate(data_x_test,label=data_y_test[,1],subject=data_subject_test[,1])

#No.1 Merge data
data_all<-rbind(data_x_train,data_x_test)

#No.2 Extracts only the measurements on the mean and standard deviation for each measurement
data_all_extr<-select(data_all,grep("mean()", names(data_all)),grep("std()", names(data_all)),label,subject)

#No.3 Uses descriptive activity names to name the activities in the data set
#No.4 Appropriately labels the data set with descriptive activity names. 
data_all_extr$label <- factor(data_all_extr$label, levels = c(1,2,3,4,5,6), labels = c("WALKING", "WALKING_UPSTARIS", "WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

#No.5 Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy<-group_by(data_all_extr,subject,label)%>%summarise_all(.funs = mean)

#No.5 Alternative way
melted <- melt(data_all_extr, id=c("subject","label"))
tidy <- dcast(melted, subject+label ~ variable, mean)

#Ending - write the tidy data set to a file
write.csv(tidy, "tidy.csv", row.names=FALSE)

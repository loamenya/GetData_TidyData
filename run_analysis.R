require(dplyr)
require(reshape2)

data_folder <- "UCI HAR Dataset";
inertial_folder <- "Inertial Signals";
test_folder <- paste(data_folder, "test",sep="/");
train_folder <- paste(data_folder, "train",sep="/");
test_inertial_folder <- paste(test_folder, inertial_folder,sep="/");
train_inertial_folder <- paste(train_folder, inertial_folder,sep="/");

data_zip <- paste(data_folder,".zip",sep = "");
#download location of dataset
download_url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists(data_zip)){
  download.file(download_url, destfile =data_zip ,mode="wb")
  unzip(data_zip)
} 

rename_to_descriptive_feature <- function (feature_name){
  feature_name <- gsub("tBody","Time.Body",feature_name,ignore.case=FALSE);
  feature_name <-  gsub("fBody","FFT.Body",feature_name,ignore.case=FALSE);
  feature_name <-  gsub("tGravity","Time.Gravity.",feature_name,ignore.case=FALSE) ;
  feature_name <-  gsub("-mean[(][)]",".Mean",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-std[(][)]",".Standard.Deviation",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-mad[(][)]",".Median.Absolute.Deviation",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-max[(][)]",".Maximum",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-min[(][)]",".Minimum",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-sma[(][)]",".Signal.Magnitude.Area",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-energy[(][)]",".Energy.Measure",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-irq[(][)]",".Interquartile.Range",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-entropy[(][)]",".Entropy.Signal",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-arCoeff[(][)]",".Autoregression.Coeficient.BurgOrder4",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-correlation[(][)]",".Correlation.Coefficient",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-maxInds[(][)]",".IndexOf.Maximum.Frequency",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-meanFreq[(][)]",".Weighted.Frequency.Average",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-skewness[(][)]",".Frequency.Domain.Signal.Skewness",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-kurtosis[(][)]",".Frequency.Domain.Signal.Kurtosis",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-bandsEnergy[(][)]","FFT.64Bin.EnergyInterval",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-angle[(][)]",".Angle.Between.Vectors",feature_name,ignore.case=FALSE)
  feature_name <-  gsub("-",".",feature_name)
  feature_name;
}

message("load features.txt");
features <- read.table(paste(data_folder,"features.txt",sep = "/"),col.names = c("id","name"),stringsAsFactors = FALSE);

message("load activity_labels.txt");
activity_labels <- read.table(paste(data_folder,"activity_labels.txt",sep = "/"),col.names = c("id","Activity"));

message("load test set");
test_dataset<- cbind(read.table(paste(test_folder, "subject_test.txt", sep="/"), col.names="Subject") ,
                      read.table(paste(test_folder, "y_test.txt", sep="/"), col.names="Activity.ID"),
                      read.table(paste(test_folder, "x_test.txt", sep="/"))) %>% mutate(row = row_number(),set="test");
#test_inertial_signals <- rbind(read.table(paste(test_inertial_folder, "total_acc_x_test.txt", sep="/")) %>% mutate(signal="total_acc_x",row=row_number()),
#                                read.table(paste(test_inertial_folder, "total_acc_y_test.txt", sep="/")) %>% mutate(signal="total_acc_y",row=row_number()),
#                                read.table(paste(test_inertial_folder, "total_acc_z_test.txt", sep="/")) %>% mutate(signal="total_acc_z",row=row_number()),
#                                read.table(paste(test_inertial_folder, "body_acc_x_test.txt", sep="/")) %>% mutate(signal="body_acc_x",row=row_number()),
#                                read.table(paste(test_inertial_folder, "body_acc_y_test.txt", sep="/")) %>% mutate(signal="body_acc_y",row=row_number()),
#                                read.table(paste(test_inertial_folder, "body_acc_z_test.txt", sep="/")) %>% mutate(signal="body_acc_z",row=row_number()),
#                                read.table(paste(test_inertial_folder, "body_gyro_x_test.txt", sep="/")) %>% mutate(signal="body_gyro_x",row=row_number()),
#                                read.table(paste(test_inertial_folder, "body_gyro_y_test.txt", sep="/")) %>% mutate(signal="body_gyro_y",row=row_number()),
#                                read.table(paste(test_inertial_folder, "body_gyro_z_test.txt", sep="/")) %>% mutate(signal="body_gyro_z",row=row_number())) %>% mutate(set="test");


message("load training set");
train_dataset<- cbind(read.table(paste(train_folder, "subject_train.txt", sep="/"), col.names="Subject") ,
                      read.table(paste(train_folder, "y_train.txt", sep="/"), col.names="Activity.ID"),
                      read.table(paste(train_folder, "x_train.txt", sep="/"))) %>% mutate(row=row_number(),set="train");
#train_inertial_signals <- rbind(read.table(paste(train_inertial_folder, "total_acc_x_train.txt", sep="/")) %>% mutate(signal="total_acc_x",row=row_number()),
#                               read.table(paste(train_inertial_folder, "total_acc_y_train.txt", sep="/")) %>% mutate(signal="total_acc_y",row=row_number()),
#                               read.table(paste(train_inertial_folder, "total_acc_z_train.txt", sep="/")) %>% mutate(signal="total_acc_z",row=row_number()),
#                               read.table(paste(train_inertial_folder, "body_acc_x_train.txt", sep="/")) %>% mutate(signal="body_acc_x",row=row_number()),
#                               read.table(paste(train_inertial_folder, "body_acc_y_train.txt", sep="/")) %>% mutate(signal="body_acc_y",row=row_number()),
#                               read.table(paste(train_inertial_folder, "body_acc_z_train.txt", sep="/")) %>% mutate(signal="body_acc_z",row=row_number()),
#                               read.table(paste(train_inertial_folder, "body_gyro_x_train.txt", sep="/")) %>% mutate(signal="body_gyro_x",row=row_number()),
#                               read.table(paste(train_inertial_folder, "body_gyro_y_train.txt", sep="/")) %>% mutate(signal="body_gyro_y",row=row_number()),
#                               read.table(paste(train_inertial_folder, "body_gyro_z_train.txt", sep="/")) %>% mutate(signal="body_gyro_z",row=row_number())) %>% mutate(set="train");

message("merging train and train datasets");
#merged_dataset <- rbind( merge(test_dataset,test_inertial_signals, by=c("set","row")),
#                         merge(train_dataset,train_inertial_signals, by=c("set","row")))

#merge dataset
merged_dataset <- rbind(test_dataset,train_dataset)
merged_dataset$Activity.ID = activity_labels$Activity[merged_dataset$Activity.ID];

#extract mean and std dev measurements
tidy <- merged_dataset[,c(1,2,grep("(mean|std)\\(",features$name)+2) ]

#apply descriptive labels to the data
names(tidy) <- c("Subject","Activity",rename_to_descriptive_feature(features$name[grep("(mean|std)\\(",features$name)]));
melted_dataset <- melt(tidy, id.vars=c("Subject", "Activity"))
tidy_mean <- dcast(melted_dataset,Subject+Activity~variable,mean)
message("writting tidy data to file tidy.txt");
write.table(tidy_mean,file="tidy.txt",row.name=FALSE)


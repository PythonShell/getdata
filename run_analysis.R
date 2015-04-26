# Reading data from X_train.txt, y_train.txt and subject_train.txt
# in the subfolder dataset/train
train_data <- read.table("./dataset/train/X_train.txt")
train_label <- read.table("./dataset/train/y_train.txt")
train_subject <- read.table("./dataset/train/subject_train.txt")

# Reading data from X_test.txt, y_test.txt and subject_test.txt
# in the subfolder dataset/train
test_data <- read.table("./dataset/test/X_test.txt")
test_label <- read.table("./dataset/test/y_test.txt")
test_subject <- read.table("./dataset/test/subject_test.txt")

# Bind two data frame into one data frame called binded_data
binded_data <- rbind(train_data, test_data)
binded_label <- rbind(train_label, test_label)
binded_subject <- rbind(train_subject, test_subject)

# Read features from features.txt and edit the names
features <- read.table("./dataset/features.txt")
index <- grep("mean\\(\\)|std\\(\\)", features[, 2])
binded_data <- binded_data[, index]
names(binded_data) <- gsub("\\(\\)", "", features[index, 2])
names(binded_data) <- gsub("-", "_", names(binded_data))

# Read and make used of activity_labels.txt
activity <- read.table("./dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activity_label <- activity[binded_label[, 1], 2]
binded_label[, 1] <- activity_label
names(binded_label) <- "activity"

# Write cleaned data
names(binded_subject) <- "subject"
cleaned_data <- cbind(binded_subject, binded_label, binded_data)
write.table(cleaned_data, "merged_data.txt")

# clean result's name
subject_len <- length(table(binded_subject))
activity_len <- dim(activity)[1]
column_len <- dim(cleaned_data)[2]
result <- matrix(NA, nrow=subject_len*activity_len, ncol=column_len) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleaned_data)

row_num <- 1
for(i in 1:subject_len) {
  for(j in 1:activity_len) {
    result[row_num, 1] <- sort(unique(binded_subject)[, 1])[i]
    result[row_num, 2] <- activity[j, 2]
    logic_1 <- i == cleaned_data$subject
    logic_2 <- activity[j, 2] == cleaned_data$activity
    result[row_num, 3:column_len] <- colMeans(cleaned_data[logic_1&logic_2, 3:column_len])
    row_num <- row_num + 1
  }
}

write.table(result, "output_data.txt", row.name=FALSE)

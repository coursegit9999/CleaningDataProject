
run_analysis <- function() {
    ## load plyr library for assistance with joins and ddply functionality
    library("plyr")

    ## load activity label data and assign column names and classes  
    act_labels <- read.table("activity_labels.txt", colClasses = c("numeric","character"), col.names = c("V1","activity"))
        
    ## read in feature.txt and create a vector with just feature names
    features <- read.table("features.txt", colClasses = "character")
    fnames <- features[,2]
    
    ## read in x_train feature data, assign feature column names
    x_train <- read.table("train/X_train.txt")
    colnames(x_train) <- fnames
    
    ## read in y_train activity data, join activities with activity labels, drop numeric activity numbers
    y_train <- read.table("train/y_train.txt")
    y_train_join <- join(y_train, act_labels, by = c("V1"), type="left", match = "first")
    y_train_join <- y_train_join[,-1, drop=FALSE]
    
    ## read in subject_train data with descriptive column name
    subject_train <- read.table("train/subject_train.txt", col.names = c("subject"))
    
    ## combine three data frames into one dataset for train data, remove old objects to clear memory
    train <- cbind(subject_train, y_train_join, x_train)
    rm(subject_train)
    rm(y_train_join)
    rm(x_train)
    
    ## read in x_test feature data, assign feature column names
    x_test <- read.table("test/x_test.txt")
    colnames(x_test) <- fnames
    
    ## read in y_test activity data, join activities with activity labels, drop numeric activity numbers
    y_test <- read.table("test/y_test.txt")
    y_test_join <- join(y_test, act_labels, by = c("V1"), type="left", match = "first")
    y_test_join <- y_test_join[,-1, drop=FALSE]
    
    ## read in subject_test data with descriptive column name
    subject_test <- read.table("test/subject_test.txt", col.names = c("subject"))
    
    ## combine three data frames by columns into one dataset for test data, remove old objects to clear memory
    test <- cbind(subject_test, y_test_join, x_test)
    rm(subject_test)
    rm(y_test_join)
    rm(x_test)

    ## combine the train and test data frames by rows, remove old objects to clear memory
    comb <- rbind(train,test)
    rm(train)
    rm(test)
    
    ## create a list of required column names for just mean, standard deviation, subject, and activity in the correct order
    ft <- features[ c(grep("mean()",features$V2, fixed=TRUE), grep("std()",features$V2, fixed=TRUE)),2,drop=TRUE]
    flist <- features[features$V2 %in% ft,2,drop=TRUE]
    clist <- c("subject","activity",flist)
    
    ## extract out only the required mesurements for mean and standard deviation for each subject and activity
    ## remove old object from memory
    extract <- comb[,clist]
    rm(clist)
    
    ## use ddply to display the average for mean and standard deviation columns grouped by subject and activity
    ## remove old object from memory
    tidy <- ddply( extract, .(subject,activity),numcolwise(mean))
    rm(extract)
    
    ## write out tidy data to a file and clear tidy object from memory
    write.table(tidy,"tidy_data.txt", row.names = FALSE)
    rm(tidy)
}
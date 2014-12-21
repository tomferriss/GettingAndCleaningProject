
#########
##  Part 1
##########

##load in the test data
X_test <-read.table("X_test.txt",header=FALSE)
y_test <-read.table("y_test.txt",header=FALSE)
subject_test <- read.table("subject_test.txt",header=FALSE)


##load in the training data
X_train <-read.table("X_train.txt",header=FALSE)
y_train <-read.table("y_train.txt",header=FALSE)
subject_train <- read.table("subject_train.txt",header=FALSE)

##stack the test and training data. Later on we will 
##bind y, X, and subject

y <- rbind(y_test,y_train)
X <- rbind(X_test,X_train)
subject <- rbind(subject_test,subject_train)

###########
#
#  Part 2, Part 4
#
##############

#the variable names for the X matrix are in features.txt
features <- read.table("features.txt",header=FALSE)

# the feature names are in the second column of this data set
features <- as.character(features[[2]])

#set the names of X
names(X) <- features

#also name the y vector. The y vector tracks the activities -- see the activities.txt file
activity_labels <- read.table("activity_labels.txt",header=FALSE)
names(y) <- "activities"

##now we will only include vectors in the X matrix that concern a mean or sd measurement

#create a logical for if "mean" or "std" or appear:
meanTrue <- grepl("mean()",tolower(names(X)))
stdTrue <- grepl("std()",tolower(names(X)))

#create a logical for if either appears
isTrue <- meanTrue + stdTrue  
keeps <- isTrue > 0

#keep only the columns where either mean or sd appear
X <- X[,keeps]

#The following also need to be dropped these too

#create a logical specifying which columns to drop
meanFreqTrue <- grepl("meanfreq",tolower(names(X)))
gravityMeanTrue <- grepl("gravitymean",tolower(names(X)))
isDrops <- meanFreqTrue + gravityMeanTrue
drops <- isDrops >0

#drop the unnecessary columns
X <- X[,!drops]

#finally, still need to drop the last one:
X <- X[,1:66]

############
#
# Part 3, End of part 1
#
############

# name the subject vector
names(subject) <- "subject"

# bind all the data together into one data set.
data <- cbind(subject,y,X)
names(data)

## re-encode the data in the activities vector with the name 
## of each activity, rather than a number code

# create a temporary vector that you can mutate
temp <- data[["activities"]]

#change the labels from a factor to characters
activity_labels <- as.character(activity_labels[[2]])

#replace each activity number with its descriptions
for (i in 1:length(temp)){
  temp[i] <- activity_labels[data[["activities"]][i]] 
}

#replace the activities column with the new column that has better names
data[["activities"]] <- temp
########
#
#  Part 5
#
#########


#From the data set in step 4, creates a second, independent tidy data set with the average of each
#￼￼￼￼￼￼￼￼variable for each activity and each subject.

require(reshape2)

#create the first part of the data set that you will glue the next columns onto
col3mean <- tapply(data[[3]],list(data$subject,data$activities),mean)
newdata <- melt(col3mean,id.var = "activities")
names(newdata)[3] <- names(data)[3]

#construck tidy data set "newdata" 
for (i in 4:ncol(data)){
  colMean <- tapply(data[[i]],list(data$subject,data$activities),mean) #calculate the mean for each subject and activity of the next variable
  iter <- melt(colMean,id.var = "activities") #make new data tidy
  newdata <- cbind(newdata,iter[,3])   #bind the next column to the data set
}

#put names on tidy data set
names(newdata) <- names(data)

write.table(newdata,file="newdata.txt",row.name=FALSE)


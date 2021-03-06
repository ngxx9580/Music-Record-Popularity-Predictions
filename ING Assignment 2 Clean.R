# Original Logistic Regression Model
#---------------------------------
MusicRecord<-read.csv(file.choose()) #load data

# How many songs does the dataset include for which the artist name is "Michael Jackson"?
table(MusicRecord$artistname == "Michael Jackson")

# Alternatively, use the pipe %>% function in "dplyr" package. It means you apply the formula after the %>% to the frame before it. 
library(dplyr)
MusicRecord %>% filter(artistname == "Michael Jackson") %>% summarize(count = n())

# first use the filter function to split the data into a training set "SongsTrain" 
# consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", 
# consisting of the 2010 song releases.
SongsTrain = MusicRecord %>% filter(year <= 2009)
SongsTest = MusicRecord %>% filter(year == 2010)

# we want to exclude some of the variables in our dataset from being used as independent variables 
# ("year", "songtitle", "artistname", "songID", and "artistID"). To do this, we can use the following trick. 
# First define a vector of variable names called nonvars - these are the variables that we won't use in our model.
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

# To remove these variables from your training and testing sets:
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

# build a logistic regression model to predict Top10 using the training data. 
# We can now use "." in place of enumerating all the remaining independent variables in the following way:
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

summary(SongsLog1)

# True or False?
# 1. The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10
# 2. In general, if the confidence is low for the time signature, tempo, and key, then the song is more likely to be complex. 
#What does our model suggest in terms of complexity?


# You can make predictions on the test set by using the command:
testPredict = predict(SongsLog1, newdata=SongsTest, type="response")

# Then, you can create a confusion matrix with a threshold of 0.15 by using the table command:
#Setting the threshold depends on how the probability is distributed.  If you choose 0.05, they will all be classified as 0.05.
confusion.matrix<-table(SongsTest$Top10, testPredict >= 0.15)

# The accuracy of the model is? 
Count.correct<-confusion.matrix[1,1]+confusion.matrix[2,2]
Count.wrong<-confusion.matrix[1,2]+confusion.matrix[2,1]

Accuracy.rate<-Count.correct/(Count.correct+Count.wrong)
# What is the prediction accuracy of the model?

# To generate the ROC curve: do the below steps
# The ROC Curve tells us.  The area under the curve 0.843, which means the prediction is good.
# Refer to slide 20 in lecture for legend. 
# As part of the assignment, we want to make the accuracy better by only including the most significant models (push to the left). 
install.packages("pROC")
library(pROC)
test_prob = predict(SongsLog1, newdata = SongsTest, type = "response")
test_roc = roc(SongsTest$Top10 ~ test_prob, plot = TRUE, print.auc = TRUE)



#Improving the Prediction Performance 
#-----------------------------------------
#Select variables by identifying which variables in SongsLog1 are statistically significant
summary(SongsLog1)
nonvars2 = c("timesignature", "tempo", "key", "timbre_1_max", "timbre_2_max", "timbre_3_min", "timbre_5_max", "timbre_8_min", 
            "timbre_8_max", "timbre_9_min", "timbre_9_max", "key_confidence", "timbre_2_min", "timbre_6_max", "timbre_7_min", 
            "timbre_7_max", "timbre_10_min")


#Create new training and testing sets that excludes the insignificant variables
SongsTrain2 = SongsTrain[ , !(names(SongsTrain) %in% nonvars2) ]
SongsTest2 = SongsTest[ , !(names(SongsTest) %in% nonvars2) ]

#Build a new logarithmic regression model 
SongsLog2 = glm(Top10 ~ ., data=SongsTrain2, family=binomial)
summary(SongsLog2)
#------------------
#Prediction Command
testPredict2 = predict(SongsLog2, newdata=SongsTest2, type="response")

#Create new confusion matrix 
confusion.matrix2<-table(SongsTest2$Top10, testPredict2 >= 0.15)

#Check accuracy of the new model
Count.correct2<-confusion.matrix2[1,1]+confusion.matrix2[2,2]
Count.wrong2<-confusion.matrix2[1,2]+confusion.matrix2[2,1]

Accuracy.rate2<-Count.correct2/(Count.correct2+Count.wrong2)

#Generate the new ROC curve
test_prob2 = predict(SongsLog2, newdata = SongsTest2, type = "response")
test_roc2 = roc(SongsTest2$Top10 ~ test_prob2, plot = TRUE, print.auc = TRUE)

hist(testPredict2)

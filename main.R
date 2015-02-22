setwd("/home/Calin/Desktop/coursera/PracticalMachineLearning/")
# Getting and Cleaning Data + Exploratory Analysis
# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","train.csv", method="curl")
# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","test.csv", method="curl")
library(rpart)
# Read the data
train <- read.csv("train.csv", na.strings=c("NA",""), header=TRUE)
test <- read.csv("test.csv", na.strings=c("NA",""), header=TRUE)

#Checking the NA distribution
summary(colSums(is.na(train)))
summary(colSums(is.na(test)))
# Most variables are NA on all lines so we remove them
# Removing the NA lines

train<-train[,colSums(is.na(train)) == 0]
test<-test[,colSums(is.na(test)) == 0]

# Removing the "header" rows
train<-train[,-c(1:7)]
test<-test[,-c(1:7)]

# Check the class of the data 
summary(train)
summary(test)
# Last collumn is for classe and problem ID I remove the problem ID column
test<-test[,1:(ncol(test)-1)]

# Split the data in a train subset and a validation subset
inTrain <- createDataPartition(y=train$classe, p=0.75, list=FALSE)
Train <- train[inTrain, ] 
validation <- train[-inTrain, ]

#Using Decision Tree
mod1 <- rpart(classe ~ ., data=Train, method="class")
pred1 <- predict(mod1, validation, type = "class")
confusionMatrix(pred1, validation$classe)

#Using Random Forest from caret
mod2<-randomForest(classe ~. , data=Train, method="class")
pred2 <- predict(mod2, validation, type = "class")
confusionMatrix(pred2, validation$classe)

submission <- predict(mod2, test, type="class")


# Write files for submission
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(predictfinal)
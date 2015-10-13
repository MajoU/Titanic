library(data.table)
library(rattle)
library(rpart)
library(RColorBrewer)
library(caret)
library(rpart)
library(rpart.plot)
library(gbm)
library(e1071)
tr <-  fread("./train.csv")
tes <- fread("./test.csv")
# Survived
tr[, Survived := as.factor(Survived)]
# Age
#tr[is.na(Age), Age := mean(tr$Age, na.rm = T)]
#tes[is.na(Age), Age := mean(tr$Age, na.rm = T)]
tr[is.na(Age), Age := -1]
tes[is.na(Age), Age := -1]
# Embarked
tr[, Embarked := as.factor(Embarked)]
tr[Embarked == "", Embarked := "S"]
tes[, Embarked := as.factor(Embarked)]
# Sex
tr[, Sex := as.factor(ifelse(Sex %in% c("female"), 1, 0))]
tes[, Sex := as.factor(ifelse(Sex %in% c("female"), 1, 0))]
# Fare
tes$Fare <- ifelse(is.na(tes$Fare), median(tes$Fare, na.rm = TRUE), tes$Fare)
# Pclass
# model
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=tr, importance=TRUE, ntree=2000)
varImpPlot(fit)

model <- train(Survived ~ Sex + Pclass + SibSp + Embarked +
                    Parch + Fare + Age, data = tr,
                    method="rf")
prediction <- predict(model, newdata=tes)
submiss <-  tes[,list(PassengerId, Survived=prediction)]
write.table(submiss, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")

#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
#             Embarked, data=tr, method="class")
#fancyRpartPlot(fit)
#Prediction <- predict(fit, tes, type = "class")
#submiss <-  tes[,list(PassengerId, Survived=Prediction)]
#write.table(submiss, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
#

#library(randomForest)
#train <- read.csv("./train.csv", stringsAsFactors=FALSE)
#test  <- read.csv("./test.csv",  stringsAsFactors=FALSE)
#extractFeatures <- function(data) {
#  features <- c("Pclass",
#                "Age",
#                "Sex",
#                "Parch",
#                "SibSp",
#                "Fare",
#                "Embarked")
#  fea <- data[,features]
#  fea$Age[is.na(fea$Age)] <- -1
#  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
#  fea$Embarked[fea$Embarked==""] = "S"
#  fea$Sex      <- as.factor(fea$Sex)
#  fea$Embarked <- as.factor(fea$Embarked)
#  return(fea)
#}











###################################
########## FUNCTION FOR EXTRACTION
###################################

knn <- train(Survived ~ Sex + Pclass + 
               SibSp + Parch + Fare + 
               Age , data = tr,
                method = "knn", preProcess = c("center", "scale"),
                  tuneLength = 10, trControl = trainControl(method = "repeatedcv"))
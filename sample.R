library(data.table)
#library(rattle)
#library(rpart)
#library(RColorBrewer)
#library(rpart.plot)
library(party)
library(caret)
library(randomForest)
library(gbm)
library(e1071)
library(vcd)
library(Hmisc)
tes <- fread("./test.csv")
tr <-  fread("./train.csv")
#tr$Fare <- cut2(tr$Fare, c(10,20,30,80))
#mosaicplot(tr$Fare ~ tr$Survived, color = T, ylab = "Survived", xlab="Feature")
tr[Embarked == "", Embarked := "S"]
#table(tr$Survived, tr$Pclass, tr$Sex)
#table(tr[Age <= 4, list(Survived)])
#tr$Fare <- cut2(tr$Fare, c(10,20,30)) 
#aggregate(Survived ~ Fare + Pclass , data=tr, FUN=function(x) {sum(x)/length(x)})
#aggregate(Survived ~ Fare + Parch, data=tr, FUN=function(x) {sum(x)/length(x)})
#table(tr$Fare , tr$SibSp)
#table(tr$Fare , tr$Survived)
#table(tes$Survived, tes$Sex)
# Survived
tr[, Survived := as.factor(Survived)]
# Age - NA values as median by Pclass == 3
#tr[is.na(Age), Age := -1]
#tes[is.na(Age), Age := -1]
tr[is.na(Age), Age := tr[Pclass == 3 & !is.na(Age), median(Age)]]
tes[is.na(Age), Age := tr[Pclass == 3 & !is.na(Age), median(Age)]]
# Embarked
tr[, Embarked := as.factor(Embarked)]
tes[, Embarked := as.factor(Embarked)]
# Fare
tes[is.na(Fare), Fare := median(tes$Fare, na.rm=T)]
# Sex !! transform string to number factor by ifelse function
tr[, Sex := as.factor(ifelse(Sex %in% 'female', 1, 0))]
tes[, Sex := as.factor(ifelse(Sex %in% 'female', 1, 0))]
# Names !!!
tr[grep("Mr\\.", Name),]
nick <- c("Dr", "Master", "Mrs", "Miss", "Mr")
lapply(nick, function(x) tr[grep(paste0(x, "\\.") , Name), Name := x])
# model
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
model <- train(Survived ~ Sex + Pclass + Embarked + SibSp +
                    Parch + Fare + Age, data = tr,
                    method="glm", trControl = ctrl)
#model <- cforest(Survived ~ Sex + Pclass +  
#                    Parch + Fare + Age, data =
#                    tr,controls=cforest_unbiased(ntree=2000, mtry=3))

prediction <- predict(model, newdata=tes)
submiss <-  tes[,list(PassengerId, Survived=prediction)]
write.table(submiss, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")

# C-forest
model <- cforest(Survived ~ Pclass + Sex + Age + Embarked + SibSp + Parch + Fare,
               data = tr, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(model, tes, OOB=TRUE, type = "response")

# Random-Forest
model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=tr, importance=TRUE, ntree=200)
varImpPlot(model)

prediction <- predict(model, newdata=tes)

#
model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
             Embarked, data=tr, method="class")
fancyRpartPlot(fit)
Prediction <- predict(fit, tes, type = "class")

###################################
########## FUNCTION FOR EXTRACTION
###################################

knn <- train(Survived ~ Sex + Pclass + 
               SibSp + Parch + Fare + 
               Age , data = tr,
                method = "knn", preProcess = c("center", "scale"),
                  tuneLength = 10, trControl = trainControl(method = "repeatedcv"))
pred <- predict(knn, newdata=tes)
submiss <-  tes[,list(PassengerId, Survived=pred)]
write.table(submiss, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")

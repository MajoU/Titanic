library(data.table)
library(rattle)
library(rpart)
library(RColorBrewer)
library(rpart.plot)
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
# model
model <- train(Survived ~ Sex + Pclass + Embarked + SibSp +
                    Parch + Fare + Age, data = tr,
                    method="glm")
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

############################################
########## COMBINATION and work with STRINGS
############################################


tes <- fread("./test.csv")
tr <-  fread("./train.csv")
tes$Survived <- NA
com <- rbind(tr, tes)
com$Title = sapply(com$Name, function(x) {strsplit(x, "[,.]")[[1]][2]})
com$Title = sub(' ', '', com$Title)
nick <- c("Dr|Master|Mrs|Miss|Mr")
com[!grep(nick, Title), Title := "Others"]
com$Title <- as.factor(com$Title)
com$Family <- com$SibSp + com$Parch + 1
com[Embarked == "", Embarked := "S"]
com[, Embarked := as.factor(Embarked)]
com[, Survived := as.factor(Survived)]
com[is.na(Age), Age := tr[Pclass == 3 & !is.na(Age), median(Age)]]
com[is.na(Fare), Fare := median(tes$Fare, na.rm=T)]
com$Fare <- cut2(com$Fare, c(10,20,30,80))
#com[, Sex := as.factor(ifelse(Sex %in% 'female', 1, 0))]
train <- com[1:891,]
test <- com[892:1309,]
model <- glm(Survived ~ SibSp + Fare + Embarked + Family + Title + Pclass + Sex + Age, data=train, family=binomial("logit"))
anova(model, test="Chisq")

#model <- rpart(Survived ~ Pclass + Title + Family + Sex + Age + SibSp +
#               Parch + Fare + Embarked, data=train, method="class")
#prediction <- predict(model, test, type = "class")
#fancyRpartPlot(model)
# primarny model
model <- train(Survived ~ SibSp + Embarked + Sex + Pclass +
               Family + Fare + Age + Title, data=train, method="glm")

# testovaci model
model <- glm(Survived ~ SibSp + Fare + Embarked + Family + Title + Pclass + Sex + Age, data=train, family=binomial("logit"))
anova(model, test="Chisq")

# overovaci model
model <- train(Survived ~ SibSp + Embarked + Sex + Pclass +
               Family + Fare + Age + Title, data=train, method="rf")
submiss <-  test[,list(PassengerId, Survived=prediction)]
write.table(submiss, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
########################
###### TEXT Manipulation
########################
# selected names transform to com$Name
lapply(nick, function(x) com[grep(paste0(x, "\\.") , Name), Name := x])
# find names with whole word and "." at the end of word
com[grep("\\b\\.", Name), Name]
# find everthing except name in vector 'nick'
nick <- c("Dr","Master","Mrs","Miss","Mr")
lapply(nick, function(x) com[grep(paste0("[^", x,"]"), Name),])



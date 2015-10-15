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
#aggregate(Survived ~ Fare + Pclass , data=tr, FUN=function(x) {sum(x)/length(x)})
tr[Embarked == "", Embarked := "S"]
# Survived
tr[, Survived := as.factor(Survived)]
# Age - NA values as median by Pclass == 3
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

model <- cforest(Survived ~ Sex + Pclass +  Parch + Fare + Age, data =
                 train,controls=cforest_unbiased(ntree=1000, mtry=5))
pred <- predict(model, test, OOB=T,type='response')

prediction <- predict(model, newdata=tes)
submiss <-  tes[,list(PassengerId, Survived=prediction)]
write.table(submiss, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")


prediction <- predict(model, newdata=tes)

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
com$Sex <- as.factor(com$Sex)
com$Pclass <- as.factor(com$Pclass)
# Parch
com$Family <- com$SibSp + com$Parch + 1
#com$Family <- as.factor(com$Family)
com[Embarked == "", Embarked := "S"]
# lepsie vysledky ked su dolne riadky zakomentovane
#com[Embarked == "C", Embarked := as.factor(1)]
#com[Embarked == "Q", Embarked := as.factor(2)]
#com[Embarked == "S", Embarked := as.factor(3)]
com[, Embarked := as.factor(Embarked)]
com[, Survived := as.factor(Survived)]
com[is.na(Age), Age := tr[Pclass == 3 & !is.na(Age), median(Age)]]
#com[is.na(Age), Age := -1]
com[is.na(Fare), Fare := median(tes$Fare, na.rm=T)]
# com[, Sex := as.factor(ifelse(Sex %in% 'female', 1, 0))]
train <- com[1:891,]
test <- com[892:1309,]
#I(Embarked=="S")
model <- gbm(Survived ~ Embarked + Family + Fare + Sex + Title + Pclass +
             Age, data=train, distribution="bernoulli", n.trees=1000)

model <- gml(Survived ~ Embarked + Family + Fare + Sex + Title + Pclass + Age, data=train, family=binomial("logit"))
anova(model, test="Chisq")

#model <- rpart(Survived ~ Pclass + Title + Family + Sex + Age + SibSp +
#               Parch + Fare + Embarked, data=train, method="class")
#prediction <- predict(model, test, type = "class")
#fancyRpartPlot(model)

### 1. ###
### Nizsie score ako random forest
# primarny model
model <- train(Survived ~ SibSp + Embarked + Sex +
               Pclass + Family + Fare + Age + Title,
               data=train, method="glmnet")

### 2. ###
# testovaci model
model <- glm(Survived ~ SibSp + Fare + Embarked + Family + Title + Pclass
             + Sex + Age, data=train, family=binomial("logit"))
anova(model, test="Chisq")


### 3. ###
# overovaci model !!! Nechať ako je! Age nie je -1 ! Pclass aj Sex je factor
set.seed(123)
model <- train(Survived ~ Embarked + Sex + Pclass + SibSp +
               Family + Fare + Age + Title, data=train, method="rf")


### 4. ###
### Najvyssie score !!
# Random-Forest - tiez vyskusať !!! + varImportance pozriet 
set.seed(123)
model <- randomForest(Survived ~ Pclass + Sex + Title + Family + Age +
                      SibSp + Fare + Embarked, data=train,
                      importance=TRUE, ntree=50)
varImpPlot(model)


### 5. ###
# Grad boost verzion 3 - slaby v kaggle !!!!
gbmGrid <-  expand.grid(interaction.depth = 5,
                        n.trees = 300,
                        shrinkage = 0.05,
                        n.minobsinnode = 5)
model <- train(Survived ~ Embarked + Sex + Pclass + SibSp + 
               Family + Fare + Age + Title, data=train, method="gbm",
               distribution="bernoulli", tuneGrid=gbmGrid)

### 6. ###
# C-forest !! vyskusat !! - 0.85 !!
set.seed(123)
model <- cforest(Survived ~ Pclass + Title + Family + Sex + Age + Embarked
                 + Fare + SibSp, data = train,
                 controls=cforest_unbiased(ntree=8000, mtry=6))
table(predict(model), train$Survived)
prediction <- predict(model, test, OOB=TRUE, type = "response")


prediction <- predict(model, test)
submiss <-  test[,list(PassengerId, Survived=prediction)]
write.table(submiss, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")

model <- rpart(Survived ~ Pclass + Title + Family + Sex + Age + Embarked
                 + Fare + SibSp, data = train, method = "class")
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

########################
###### DUMMY Examples
########################

# dummify the data
tes <- fread("./test.csv")
tr <-  fread("./train.csv")
tes$Survived <- NA
com <- rbind(tr, tes)
com$Title = sapply(com$Name, function(x) {strsplit(x, "[,.]")[[1]][2]})
com$Title = sub(' ', '', com$Title)
nick <- c("Dr|Master|Mrs|Miss|Mr")
com[!grep(nick, Title), Title := "Others"]
com$Title <- as.factor(com$Title)
com$Sex <- as.factor(com$Sex)
com$Pclass <- as.factor(com$Pclass)
# Parch
com$Family <- com$SibSp + com$Parch + 1
#com$Family <- as.factor(com$Family)
com[Embarked == "", Embarked := "S"]
# lepsie vysledky ked su dolne riadky zakomentovane
#com[Embarked == "C", Embarked := as.factor(1)]
#com[Embarked == "Q", Embarked := as.factor(2)]
#com[Embarked == "S", Embarked := as.factor(3)]
com[, Embarked := as.factor(Embarked)]
com[, Survived := as.factor(Survived)]
com[is.na(Age), Age := tr[Pclass == 3 & !is.na(Age), median(Age)]]
#com[is.na(Age), Age := -1]
com[is.na(Fare), Fare := median(tes$Fare, na.rm=T)]
comb <- com[,list(Pclass, Embarked, Sex, Title)]
dmy <- dummyVars(" ~ .", data = comb, fullRank=T)
dum <- data.table(predict(dmy, newdata = comb))
dum$Age <- com$Age
dum$SibSp <- com$SibSp
dum$Parch <- com$Parch
dum$Fare <- com$Fare
dum$Family <- com$Family
dum$Survived <- com$Survived
dum <- dum[,lapply(.SD,as.factor),]
train <- dum[1:891,]
test <- dum[892:1309,]
print(dum)

model <- train(Survived ~ Fare + Age + SibSp + Family + Pclass.3 +
               Sex.male + Title.Mr + Embarked.S + Title.Miss + Title.Mrs,
               data=train, method="glm")


model <- randomForest(Survived ~ Fare + Age + SibSp + Family + Pclass.3 +
               Sex.male + Title.Mr + Embarked.S + Title.Miss + Title.Mrs,
                data=train, importance=TRUE, ntree=1000) 
varImpPlot(model)

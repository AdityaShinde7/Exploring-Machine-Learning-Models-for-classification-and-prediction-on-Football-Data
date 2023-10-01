install.packages(c("ggplot2", "ggthemes", "scales", "dplyr", "mice", "randomForest"))
install.packages('psych', "Boruta")
install.packages("Boruta")
install.packages("ISLR")
install.packages('corrgram')
install.packages("klar")
install.packages("ROCR",dep=T)
setwd("C:/Users/adity/Downloads/DMMLPROJ")
shotsData <-read.csv("shots.csv", header=T, na.strings=c(""), stringsAsFactors = T)
playerdata <-read.csv("players.csv", header=T, na.strings=c(""), stringsAsFactors = T)
appearanceData <-read.csv("appearances.csv", header=T, na.strings=c(""), stringsAsFactors = T)
gamesData <-read.csv("games.csv", header=T, na.strings=c(""), stringsAsFactors = T)
teamstats <-read.csv("teamstats.csv", header=T, na.strings=c(""), stringsAsFactors = T)
str(teamstats)


gamesData1 <- gamesData[, c(1,2,5,6,9,10,11,12,13,14,15,16)]
str(gamesData1)

teamstats1 <- merge(teamstats,gamesData1, all.x=TRUE)
head(teamstats1, 5)

str(teamstats1)


teamstats1$opponent <- ifelse(teamstats1$location == 'h',teamstats1$awayTeamID,teamstats1$homeTeamID)
teamstats2leagueFilter <- teamstats1[teamstats1$leagueID == 1 | teamstats1$leagueID == 2 ,]

str(teamstats2leagueFilter)

table(teamstats2leagueFilter$teamID)

min(table(teamstats2leagueFilter$teamID))

library(plyr)
teamfreq  <-count(teamstats2leagueFilter , "teamID")
teamfreq
freqRelevant <- teamfreq[teamfreq$freq == 38,]
irrelevant<- freqRelevant$teamID 
teamstats2leagueFilter$relevant <- 1
irrelevant

for (i in irrelevant) {
  teamstats2leagueFilter$relevant <- ifelse(teamstats2leagueFilter$teamID == i ,0,teamstats2leagueFilter$relevant)
  
  
}

str(teamstats2leagueFilter)
teamfin1<- teamstats2leagueFilter[teamstats2leagueFilter$relevant == 1 ,]

str(teamfin1)
#####################################################
#For opponents

table(teamfin1$opponent)

min(table(teamfi1n$opponent))
oppfreq  <-count(teamfin1 , "opponent")
freqRelevantopp <- oppfreq[oppfreq$freq <= 38,]
freqRelevantopp$opponent
irrelevanopp <- freqRelevantopp$opponent 
teamfin1$relevant <- 1

for (i in irrelevanopp) {
  teamfin1$relevant <- ifelse(teamfin1$opponent == i ,0,teamfin1$relevant)
  
  
}

str(teamfin1)
teamfin<- teamfin1[teamfin1$relevant == 1 ,]

str(teamfin)
#####################################################

#Result Classification Data
finalData <-teamfin[,-c(1,4,6,17,18,19,23,24,25,26,27,29)]
#finalData$teamID = as.integer(finalData$teamID)
finalData$teamID = as.factor(finalData$teamID)
finalData$season = as.factor(finalData$season)
finalData$yellowCards = as.numeric(as.character(finalData$yellowCards))
#finalData$opponent = as.integer(finalData$opponent)
finalData$opponent = as.factor(as.character( finalData$opponent))
#write.csv(finalData, "findunq.csv")

str(finalData) 
levels(droplevels(finalData$teamID))
finalData$teamID <- factor(finalData$teamID)
finalData$opponent<- factor(finalData$opponent)
levels(droplevels(finalData$opponent))

str(finalData) 
sapply(finalData,function(x)sum(is.na(x)))

finalData<-finalData [!is.na(finalData$yellowCards), ]
sum(is.na(finalData$yellowCards))
str(finalData)
summary(finalData)

#############BORUTA###########################
library(Boruta)
set.seed(111)
boruta.result <- Boruta(result~., data = finalData, doTrace = 2)
print(boruta.result)

plot(boruta.result, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.result$ImpHistory),function(i)
  boruta.result$ImpHistory[is.finite(boruta.result$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.result$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.result$ImpHistory), cex.axis = 0.7)

##############################
library(tree)
library(ISLR)
x <- finalData[,-13]
n <-nrow(x)
set.seed(101)
train.index <- sample(1:n, .70*n)
train = finalData[train.index,] # Create the training data 
test = finalData[-train.index,]

# Create the test data
x.train <- x[ train.index,]
x.test  <- x[-train.index,]

attach(finalData)
summary(result)
y.train<- result[train.index]
y.test<- result[-train.index]
##################Random forest#####################################

library(MASS)
require(randomForest)
rf <-randomForest(result~.,data=train) 
print(rf)
str(train)
oob.err = double(16)
test.err = double(16)
set.seed(777)
for(mtry in 1:16){
  print(mtry)
  fit = randomForest(result~., data = train, mtry = mtry,type = 'class', ntree = 350)
  print(fit$err.rate[350])
  oob.err[mtry] = fit$err.rate[350]
  pred = predict(fit, test,type = 'class')
  table_mat <- table(y.test, pred)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  print(accuracy_Test)
  test.err[mtry] = accuracy_Test
  
}
# View accuracy score



matplot(1:mtry, test.err, pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
set.seed(777)
fitopt = randomForest(result~., data = train, mtry = 2,type = 'class', ntree = 350)
oob.erropt = fitopt$err.rate[350]
predopt = predict(fitopt, test,type = 'class')
table_matopt <- table(y.test, predopt)
accuracy_Testopt <- sum(diag(table_matopt)) / sum(table_matopt)

library(caret)
confusionMatrix(predopt,y.test)



#########    XGB      ########################################
library(tidyverse)
library(caret)
library(xgboost)

# Fit the model on the training set
set.seed(123)
model <- train(
  result ~., data = train, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
model$bestTune

# Make predictions on the test data
set.seed(123)
predicted.classes <- model %>% predict(test)
head(predicted.classes)
confusionMatrix(predicted.classes,y.test)

# Compute model prediction accuracy rate

varImp(model)



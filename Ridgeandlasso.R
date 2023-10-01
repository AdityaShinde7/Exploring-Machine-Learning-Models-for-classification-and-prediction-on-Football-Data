install.packages(c("ggplot2", "ggthemes", "scales", "dplyr", "mice", "randomForest"))
install.packages('psych', "Boruta")
install.packages('MLmetrics')
install.packages("Boruta")
setwd("C:/Users/adity/Downloads/DMMLPROJ")
shotsData <-read.csv("shots.csv", header=T, na.strings=c(""), stringsAsFactors = T)
playerdata <-read.csv("players.csv", header=T, na.strings=c(""), stringsAsFactors = T)
appearanceData <-read.csv("appearances.csv", header=T, na.strings=c(""), stringsAsFactors = T)
gamesData <-read.csv("games.csv", header=T, na.strings=c(""), stringsAsFactors = T)
teamstats <-read.csv("teamstats.csv", header=T, na.strings=c(""), stringsAsFactors = T)
str(teamstats)


gamesData1 <- gamesData[, c(1,2,5,6,12,13,17,18,19,26,27,28)]
str(gamesData1)

teamstats1 <- merge(teamstats,gamesData1, all.x=TRUE)
head(teamstats1, 5)

str(teamstats1)
teamstats2leagueFilter1 <- teamstats1[teamstats1$leagueID == 3 | teamstats1$leagueID == 5 | teamstats1$leagueID == 4,]

str(teamstats2leagueFilter1)

table(teamstats2leagueFilter1$teamID)

min(table(teamstats2leagueFilter1$teamID))

library(plyr)
teamfreq1  <- count(teamstats2leagueFilter1 , "teamID")
freqRelevant1 <- teamfreq1[teamfreq1$freq == 38,]
freqRelevant1$teamID
irrelevant1<- freqRelevant1$teamID 
teamstats2leagueFilter1$relevant <- 1


for (i in irrelevant1) {
  teamstats2leagueFilter1$relevant <- ifelse(teamstats2leagueFilter1$teamID == i ,0,teamstats2leagueFilter1$relevant)
  
  
}

str(teamstats2leagueFilter1)
teamfin1<- teamstats2leagueFilter1[teamstats2leagueFilter1$relevant == 1 ,]

str(teamfin1)

#Goal prediction Data
finalDataGoals <-teamfin1[,-c(1,4,16,17,28)]
str(finalDataGoals)
finalDataGoals$teamID = as.factor(finalDataGoals$teamID)
finalDataGoals$leagueID = as.factor(finalDataGoals$leagueID)
finalDataGoals$season = as.factor(finalDataGoals$season)
finalDataGoals$homeTeamID = as.factor(finalDataGoals$homeTeamID)
finalDataGoals$awayTeamID = as.factor(finalDataGoals$awayTeamID)

finalDataGoals$yellowCards = as.numeric(paste(finalDataGoals$yellowCards))
finalDataGoals$BWH = as.numeric(paste(finalDataGoals$BWH))
finalDataGoals$BWD = as.numeric(paste(finalDataGoals$BWD))
finalDataGoals$BWA = as.numeric(paste(finalDataGoals$WHH))
finalDataGoals$WHH = as.numeric(paste(finalDataGoals$BWH))
finalDataGoals$WHD = as.numeric(paste(finalDataGoals$WHD))
finalDataGoals$WHA = as.numeric(paste(finalDataGoals$WHA))
str(finalDataGoals)
#write.csv(finalDataGoals, "findGoal.csv")
sapply(finalDataGoals,function(x)sum(is.na(x)))

finalDataGoals<-finalDataGoals [!is.na(finalDataGoals$WHD), ]
sum(is.na(finalDataGoals$WHD))
str(finalDataGoals)
hist(sqrt(finalDataGoals$goals))
par(mfrow=c(1,1))
boxplot(finalDataGoals)
str(finalDataGoals)
summary(finalDataGoals)
#############BORUTA###########################
library(Boruta)
set.seed(111)
boruta.result <- Boruta(goals~., data = finalDataGoals, doTrace = 2)
print(boruta.result)

plot(boruta.result, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.result$ImpHistory),function(i)
  boruta.result$ImpHistory[is.finite(boruta.result$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.result$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.result$ImpHistory), cex.axis = 0.7)

##############################
library(caret)
library(class)
library(glmnet)
set.seed(777)
x=model.matrix(goals~.,data=finalDataGoals)
y=finalDataGoals$goals
n <-nrow(x)
train.index <- sample(1:n, .70*n)
train = finalDataGoals[train.index,] # Create the training data 
test = finalDataGoals[-train.index,] # Create the test data
x.train <- x[ train.index,]
x.test  <- x[-train.index,]

y.train=y[train.index]
y.test= y[-train.index]


#
set.seed(777)
lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x.train, y.train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)
plot(ridge_reg)
summary(ridge_reg)
set.seed(777)
cv_ridge <- cv.glmnet(x.train, y.train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda
optimalreg <- glmnet(x.train, y.train, alpha = 0, lambda = 0.001)
m.ridge.cv <- cv.glmnet(x.train, y.train, alpha = 0, lambda = lambdas)
plot(m.ridge.cv)

# Compute R^2 from true and predicted values
library(MLmetrics)
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  MAPE_value = MAPE(true,predicted)
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square,
    MAPE = MAPE_value
  )
  
}
set.seed(777)
# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x.train)
eval_results(y.train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x.test)
eval_results(y.test, predictions_test, test)




###continue Lasso
set.seed(777)
lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
set.seed(777)
lasso_reg <- cv.glmnet(x.train, y.train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
plot(lasso_reg)
# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best

lasso_model <- glmnet(x.train, y.train, alpha = 1, lambda = lambda_best, standardize = TRUE)
set.seed(777)
predictions_train <- predict(lasso_model, s = lambda_best, newx = x.train)
eval_results(y.train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x.test)
eval_results(y.test, predictions_test, test)





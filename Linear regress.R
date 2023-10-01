install.packages(c("ggplot2", "ggthemes", "scales", "dplyr", "mice", "randomForest"))
install.packages('psych', "Boruta")
install.packages("Boruta")
install.packages("ISLR")
setwd("C:/Users/adity/Downloads/DMMLPROJ")
set.seed(777)

gamesData <-read.csv("games.csv", header=T, na.strings=c(""), stringsAsFactors = T)
teamstats <-read.csv("teamstats.csv", header=T, na.strings=c(""), stringsAsFactors = T)
str(teamstats)


gamesData1 <- gamesData[, c(1,2,5,6,9,10,11,12,13,14,15,16)]
str(gamesData1)

teamstats1 <- merge(teamstats,gamesData1, all.x=TRUE)
head(teamstats1, 5)

str(teamstats1)

teamstats2leagueFilter1 <- teamstats1[teamstats1$leagueID == 1 | teamstats1$leagueID == 2 | teamstats1$leagueID == 4 ,]

str(teamstats2leagueFilter1)

table(teamstats2leagueFilter1$teamID)

min(table(teamstats2leagueFilter1$teamID))

summary(teamstats2leagueFilter1)

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
finalDataGoals <-teamfin1[,-c(1,4,16,17,18,19,20,21,22,28)]
finalDataGoals$goals = as.numeric(finalDataGoals$goals)
finalDataGoals$teamID = as.integer(finalDataGoals$teamID)
finalDataGoals$teamID = as.factor(finalDataGoals$teamID)
finalDataGoals$season = as.factor(finalDataGoals$season)
finalDataGoals$yellowCards = as.integer(finalDataGoals$yellowCards)
finalDataGoals$opponent = as.integer(finalDataGoals$opponent)

finalDataGoals$B365H = as.numeric(paste(finalDataGoals$B365H))
finalDataGoals$B365D = as.numeric(paste(finalDataGoals$B365D))
finalDataGoals$B365A = as.numeric(paste(finalDataGoals$B365A))
str(finalDataGoals)
write.csv(finalDataGoals, "findGoal.csv")
sapply(finalDataGoals,function(x)sum(is.na(x)))
finalDataGoals<-finalDataGoals [!is.na(finalDataGoals$B365H), ]
sum(is.na(finalDataGoals$B365H))
library(corrgram)
corrgram(finalDataGoals,order= TRUE,lower.panel = panel.shade,upper.panel = panel.pie, text.panel = panel.txt)
pairs(finalDataGoals, panel =panel.smooth)
hist(sqrt(finalDataGoals$goals))
par(mfrow=c(1,1))
boxplot(finalDataGoals$goals)
summary(finalDataGoals)
str(finalDataGoals)
finalDataGoals <-finalDataGoals[,-c(16)]
#------------------------------------
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
#plotting graphs for EDA
set.seed(777)
str(finalDataGoals)
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

set.seed(777)
lm.fit=lm(goals~.,data=train )
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

set.seed(777)
library(dplyr)
cooksD <- cooks.distance(lm.fit)
influential <- cooksD[(cooksD > (1 * mean(cooksD, na.rm = TRUE)))]
influential
names_of_influential <- names(influential)
outliers <- train[names_of_influential,]
finalDataoutliersRemoved <- train %>% anti_join(outliers)
finalDataoutliersRemoved

set.seed(777)
lm.fit1=lm(goals~.-deep-ppda,data=finalDataoutliersRemoved )
summary(lm.fit1)
par(mfrow=c(2,2))
plot(lm.fit1)

str(train)


library(performance)
library(see)
library(patchwork)
check_model(lm.fit1)

library(car)

durbinWatsonTest(lm.fit1)
vif(lm.fit1)

library(MLmetrics)
eval_results <- function(true, predicted,df) {
  Rsquare = R2_Score(predicted,true)
  RMSE = RMSE(predicted,true)
  MAPE_value = MAPE(true,predicted)
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = Rsquare,
    MAPE = MAPE_value
  )
  
}
# Prediction and evaluation on train data
predictions_train <- predict.lm(lm.fit, train)
eval_results(y.train, predictions_train,train)



predictions_test <- predict.lm(lm.fit, test)
eval_results(y.test, predictions_test,test)

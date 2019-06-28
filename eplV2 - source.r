#setwd("C:\\Users\\ali_c\\Documents\\R\\Practice")
# setwd("C:/Users/SinghV54/Desktop/GPRS Research/Codementor/Self Learning/Premier League/Ali_Shinyapp/")

# Loading Library & Reading file ---------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(randomForest)
library(dplyr)
library(purrr)
library(skellam)
library(lubridate)
library(Hmisc)
library(e1071)
library(rpart)
library(huxtable)
library(caret)
library(kernlab)
#Reading the File
df <- read.csv("d.csv")

# Model Building ----------------------------------------------------------

#First using Random Forest
#Third Model - Regression
  
main <- function(home_team, away_team)
{
  df <- read.csv("d.csv")
  epl <- df
test <- data.frame(home=1, team=home_team, opponent=away_team) 
  levels(test$team) <- union(levels(test$team), levels(epl$HomeTeam))
  levels(test$opponent) <- union(levels(test$opponent), levels(epl$AwayTeam))
  names(epl)[c(5,6)] <- c("homeGoals","awayGoals")
  
  rf_model <- 
    rbind(
      data.frame(goals=epl$homeGoals,
                 team=epl$HomeTeam,
                 opponent=epl$AwayTeam,
                 home=1),
      data.frame(goals=epl$awayGoals,
                 team=epl$AwayTeam,
                 opponent=epl$HomeTeam,
                 home=0)) %>%
    train(goals ~ home + team +opponent, data=., method="rf", ntree=5, trControl=trainControl(method="repeatedcv", number=10), repeats=3, search="random")
  #randomForest(goals ~ home + team +opponent, data=., ntree=500, mtry=2)
  
  #Second Model - CART - Classification & Decision Tress
  rpart_model <- 
    rbind(
      data.frame(goals=epl$homeGoals,
                 team=epl$HomeTeam,
                 opponent=epl$AwayTeam,
                 home=1),
      data.frame(goals=epl$awayGoals,
                 team=epl$AwayTeam,
                 opponent=epl$HomeTeam,
                 home=0)) %>%
    train(goals ~ home + team +opponent, data=., method="rpart", tuneLength=10,trControl=trainControl(method="repeatedcv",number=10, repeats=3), parms=list(split= 'information'))
  #rpart(goals ~ home + team +opponent, data=.)
  
  #Third Model - Regression
  
  glm_model <- 
    rbind(
      data.frame(goals=epl$homeGoals,
                 team=epl$HomeTeam,
                 opponent=epl$AwayTeam,
                 home=1,type="prob"),
      data.frame(goals=epl$awayGoals,
                 team=epl$AwayTeam,
                 opponent=epl$AwayTeam,
                 home=0,type="prob")) %>%
    glm(goals ~ home + team +opponent, family=poisson(link=log),data=.)
  
  
  svm_model <- 
    rbind(
      data.frame(goals=epl$homeGoals,
                 team=epl$HomeTeam,
                 opponent=epl$AwayTeam,
                 home=1,type="prob"),
      data.frame(goals=epl$awayGoals,
                 team=epl$AwayTeam,
                 opponent=epl$AwayTeam,
                 home=0,type="prob")) %>%
    train(goals ~ home + team +opponent, data=.,method="svmLinear",trControl = trainControl(method = "repeatedcv", number = 3, repeats = 1), preProcess = c("center", "scale"))
  
  lm_model <- 
    rbind(
      data.frame(goals=epl$homeGoals,
                 team=epl$HomeTeam,
                 opponent=epl$AwayTeam,
                 home=1,type="prob"),
      data.frame(goals=epl$awayGoals,
                 team=epl$AwayTeam,
                 opponent=epl$AwayTeam,
                 home=0,type="prob")) %>%
    lm(goals ~ home + team +opponent,data=.)
  
  #Using Same simulation function to calculate probability of each teams win
  simulate_match <- function(foot_model, test, max_goals){
    home_goals_avg <- predict(foot_model, data.frame(home=1, team=test$team, opponent=test$opponent))
    away_goals_avg <- predict(foot_model, data.frame(home=0, team=test$opponent, opponent=test$team))
    dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg) 
  }
  
  simulate_match_svm <- function(foot_model, test, max_goals){
    home_goals_avg <- predict(foot_model, data.frame(home=1, team=test$team, opponent=test$opponent, "raw"))
    away_goals_avg <- predict(foot_model, data.frame(home=0, team=test$opponent, opponent=test$team,"raw"))
    dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg) 
  }
  #Using Same simulation function to calculate probability of each teams win for GLM
  simulate_match_glm <- function(foot_model, test, max_goals){
    home_goals_avg <- predict(foot_model,data.frame(home=1, team=test$team, opponent=test$opponent), type="response")
    away_goals_avg <- predict(foot_model, data.frame(home=0, team=test$opponent, opponent=test$team), type="response")
    dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg) 
  }
  


#Creating Test data

#Getting output for each model
 # chel_sun_rf <- simulate_match(rf_model, test, 20)
  chel_sun_rp <- simulate_match(rpart_model, test, 20)
  chel_sun_glm <- simulate_match_glm(glm_model, test, 20)
  chel_sun_svm <- simulate_match_svm(svm_model, test, 20)
  chel_sun_lm <- simulate_match(lm_model, test, 20)
  
  
  # rf <- data.frame("Win"=paste0(round(sum(chel_sun_rf[lower.tri(chel_sun_rf)]),3)*100,"%"),"Draw"=paste0(round(sum(diag(chel_sun_rf)),3)*100,"%"),"Loss" = paste0(round(sum(chel_sun_rf[upper.tri(chel_sun_rf)]),3)*100,"%"))
  rp <- data.frame("Win"=paste0(round(sum(chel_sun_rp[lower.tri(chel_sun_rp)]),3)*100,"%"),"Draw"=paste0(round(sum(diag(chel_sun_rp)),3)*100,"%"),"Loss" = paste0(round(sum(chel_sun_rp[upper.tri(chel_sun_rp)]),3)*100,"%"))
  glm <- data.frame("Win"=paste0(round(sum(chel_sun_glm[lower.tri(chel_sun_glm)]),3)*100,"%"),"Draw"=paste0(round(sum(diag(chel_sun_glm)),3)*100,"%"),"Loss" = paste0(round(sum(chel_sun_glm[upper.tri(chel_sun_glm)]),3)*100,"%"))
  
  svm <- data.frame("Win"=paste0(round(sum(chel_sun_svm[lower.tri(chel_sun_svm)]),3)*100,"%"),"Draw"=paste0(round(sum(diag(chel_sun_svm)),3)*100,"%"),"Loss" = paste0(round(sum(chel_sun_svm[upper.tri(chel_sun_svm)]),3)*100,"%"))
  
  lm <- data.frame("Win"=paste0(round(sum(chel_sun_lm[lower.tri(chel_sun_lm)]),3)*100,"%"),"Draw"=paste0(round(sum(diag(chel_sun_lm)),3)*100,"%"),"Loss" = paste0(round(sum(chel_sun_lm[upper.tri(chel_sun_lm)]),3)*100,"%"))
  
  
  #Creating a dataframe where each row represents each model
  final <- rbind(rp,glm, svm,lm)
  rownames(final)<- c("Decision Trees","Poisson Regression","SVM", "Linear Regression")
  colnames(final) <- c(paste0(home_team," Win Vs ",away_team),"Draw",paste0(away_team," Win Vs ",home_team))
  
#Printing final output table
final_df <- 
  hux(glm) %>% 
  add_colnames() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_outer_borders(TRUE)  %>%
  add_rownames() %>%
  set_outer_borders(0.4) %>%
  set_caption('Prediction of Results') %>%
  theme_article() 

#Printing Final output
#readline(print(final_df,colnames = FALSE, rownames=TRUE))
return(final)


}

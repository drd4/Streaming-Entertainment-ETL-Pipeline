# @Daniel Dasgupta
# BLA Assessment
# 2/21/2021



###############################################################################
###############################################################################
###############################################################################

#Question 1:  All code is original 

###########################  # Prepare Data  # ##################################
      
data = read.csv(file = "PredictiveModelingAssessmentData.csv")
testdata = read.csv(file="TestData.csv")


#Create train and test samples using split
library(caret)
set.seed(1706)
split = createDataPartition(y = data$y, p = 0.7, list = F)
train = data[split,]
test = data[-split,]
head(train)

###########################  # Linear Regression  # ###########################
mod <- lm(y~x1 + x2, data = train)
summary(mod)
round(table(data$x2),2)
pred = predict(mod, newdata = test)

rmse1 = sqrt(mean((pred-train$y)^2)); rmse1
#2.39

###########################  # Bootstrap (BAG MODEL) # ########################
library(randomForest)
set.seed(1706)
bag = randomForest(y~.,data=train,mtry = ncol(train)-1,ntree=1000)  #mtry = number of predictors  #fit 1000 trees
pred = predict(bag,newdata=test)
rmse_bag = sqrt(mean((pred-test$y)^2)); rmse_bag



###########################  # Random Forest # ###############################

library(randomForest)
set.seed(1706)
forest = randomForest(y~.,data=train,ntree = 1000)
pred = predict(forest,newdata=test)
rmse_forest = sqrt(mean((pred-test$y)^2)); rmse_forest
#1.071



###########################  # Tree with Tuning # ##############################

library(rpart)
library(rpart.plot)
library(caret)
set.seed(1706)
trControl = trainControl(method='cv',number = 5)
tuneGrid = expand.grid(.cp = seq(from = 0.001,to = 0.1,by = 0.001))

cvModel = train(y~.,
                data=train,
                method="rpart",
                trControl = trControl,
                tuneGrid = tuneGrid)

cvModel$results
tree <- rpart(y~.,
              cp = cvModel$bestTune$cp,
              data=train)

rpart.plot(tree)
pred = predict(tree, newdata= test)
rmse_tree = sqrt(mean((pred-test$y)^2)); rmse_tree
#1.115

###########################  # Tuned Forest Ranger # ##########################
library(ranger)
trControl=trainControl(method="cv",number=5)
tuneGrid = expand.grid(mtry=1:2, 
                       splitrule = c('variance','extratrees','maxstat'), 
                       min.node.size = c(2,5,10,15,20,25))
set.seed(1706)
cvModel = train(y~.,
                data=train,
                method="ranger",
                num.trees=1000,
                trControl=trControl,
                tuneGrid=tuneGrid )
cv_forest_ranger = ranger(y~.,
                          data=train,
                          num.trees = 1000, 
                          mtry=cvModel$bestTune$mtry, 
                          min.node.size = cvModel$bestTune$min.node.size, 
                          splitrule = cvModel$bestTune$splitrule)
pred = predict(cv_forest_ranger, data =test, num.trees = 1000)
rmse_cv_forest_ranger = sqrt(mean((pred$predictions-test$y)^2)); rmse_cv_forest_ranger
#1.029

###########################  # Boosting Model # #############################

library(gbm)
set.seed(1706)
boost = gbm(y~.,
            data=train,
            distribution="gaussian",
            n.trees = 1000,
            interaction.depth = 2,
            shrinkage = 0.01)

pred = predict(boost,test,n.trees = 1000)
rmse_boost = sqrt(mean((pred-test$y)^2)); rmse_boost
# 1.05

###########################  # Boosting with XGBoost # #########################
library(vtreat)
library(xgboost)


trt = designTreatmentsZ(dframe = train,
                        varlist = names(train)[2:3])
newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']

train_input = prepare(treatmentplan = trt, 
                      dframe = train,
                      varRestriction = newvars)
test_input = prepare(treatmentplan = trt, 
                     dframe = test,
                     varRestriction = newvars)
head(train_input)

#Identify number of nrounds to prevent overfitting
set.seed(1706)
tune_nrounds = xgb.cv(data=as.matrix(train_input), 
                      label = train$y,
                      nrounds=250,
                      nfold = 5,
                      verbose = 0)


ggplot(data=tune_nrounds$evaluation_log, aes(x=iter, y=test_rmse_mean))+
  geom_point(size=0.4, color='sienna')+
  geom_line(size=0.1, alpha=0.1)+
  theme_bw()

# find optimal n rounds 
which.min(tune_nrounds$evaluation_log$test_rmse_mean)

xgboost2= xgboost(data=as.matrix(train_input), 
                  label = train$y,
                  nrounds=11,
                  verbose = 0)
pred = predict(xgboost2, 
               newdata=as.matrix(test_input))
rmse_xgboost = sqrt(mean((pred - test$y)^2)); rmse_xgboost
#1.042

###########################  # Boosting with Cross Validation # ################
library(gbm)
library(caret)
set.seed(1706)
trControl = trainControl(method="cv",number=5)
tuneGrid = expand.grid(n.trees = 1000, 
                       interaction.depth = c(1,2,3),
                       shrinkage = (1:100)*0.001,
                       n.minobsinnode=c(5,10,15))
garbage = capture.output(cvModel <- train(y~.,
                                          data=train,
                                          method="gbm",
                                          trControl=trControl, 
                                          tuneGrid=tuneGrid))
cvBoost = gbm(y~.,
              data=train,
              distribution="gaussian",
              n.trees=cvModel$bestTune$n.trees,
              interaction.depth=cvModel$bestTune$interaction.depth,
              shrinkage=cvModel$bestTune$shrinkage,
              n.minobsinnode = cvModel$bestTune$n.minobsinnode)
pred = predict(cvBoost,test,n.trees=1000) ## predicting the new dataset
rmse_cv_boost = sqrt(mean((pred-test$y)^2)); rmse_cv_boost
#1.028958

predsub = predict(cvBoost, newdata= testdata, n.trees = 1000)
submissionFile = data.frame(testdata, y=predsub)
write.csv(submissionFile, 'TestPredictions.csv', row.names=FALSE)





###############################################################################
###############################################################################
## Question 3:

##REFERENCES -  I was not too familiar with NCAA Basketball Rankings, but after some research
# I found some interesting methods on Reddit and Github.  However, the functions to create RPI and SRS come from https://dpmartin42.github.io/posts/r/college-basketball-rankings
# SQL Analysis is original code  



library(RSQLite)
library(DBI)
library(readr)
library(tidyverse)
library(purrr)
library(forcats)
library(limSolve)

################## DATA PREP ###############################
# Connect to DB

conn <- dbConnect(RSQLite::SQLite(), "acc1819.db")
dbListTables(conn)
box_scores <- dbReadTable(conn, "box_scores")
games <- dbReadTable(conn, "games")

### First lets write two queries to get the Away and Home Stats 
away <- "select
g.gameid,
g.hometeam,
g.awayteam,
case when b.Home = 0 then b.score else null end as 'Away_Score',
case when b.Home = 0 then b.AST else null end as 'Away_AST',
case when b.Home = 0 then b.TOV else null end as 'Away_TOV',
case when b.home = 0 then b.STL else null end as 'Away_STL',
case when b.home = 0 then b.BLK else null end as 'Away_BLK',
case when b.home = 0 then b.Rebounds else null end as 'Away_Rebounds',
case when b.home = 0 then b.ORB else null end as 'Away_ORB',
case when b.home = 0 then b.DRB else null end as 'Away_DRB',
case when b.home = 0 then b.FGA else null end as 'Away_FGA',
case when b.home = 0 then b.FGM else null end as 'Away_FGM',
case when b.home = 0 then b.FTA else null end as 'Away_FTA',
case when b.home = 0 then b.FTM else null end as 'Away_FTM',
case when b.home = 0 then b.Fouls else null end as 'Away_Fouls'
from games g
left join box_scores b on b.gameid = g.gameid
group by g.gameid"
away <- dbGetQuery(conn, away)


home <- "select 
g.gameid,
g.hometeam,
g.awayteam,
case when b.Home = 1 then b.score else null end as 'Home_Score',
case when b.Home = 1 then b.AST else null end as 'Home_AST',
case when b.Home = 1 then b.TOV else null end as 'Home_TOV',
case when b.home = 1 then b.STL else null end as 'Home_STL',
case when b.home = 1 then b.BLK else null end as 'Home_BLK',
case when b.home = 1 then b.Rebounds else null end as 'Home_Rebounds',
case when b.home = 1 then b.ORB else null end as 'Home_ORB',
case when b.home = 1 then b.DRB else null end as 'Home_DRB',
case when b.home = 1 then b.FGA else null end as 'Home_FGA',
case when b.home = 1 then b.FGM else null end as 'Home_FGM',
case when b.home = 1 then b.FTA else null end as 'Home_FTA',
case when b.home = 1 then b.FTM else null end as 'Home_FTM',
case when b.home = 1 then b.Fouls else null end as 'Home_Fouls'



from games g
left join box_scores b on b.gameid = g.gameid
where b.home = 1
"
home <- dbGetQuery(conn, home)

#merge queries
scores_cleaned <- merge(home, away)

#mutate to create new columns 
mut = scores_cleaned %>%
  mutate(WTeamID = case_when(Home_Score > Away_Score ~ HomeTeam, TRUE ~ AwayTeam)) %>%
  mutate(LTeamID = case_when (Home_Score <  Away_Score ~ HomeTeam, TRUE ~ AwayTeam)) %>%
  mutate(WScore = case_when(Home_Score > Away_Score ~ Home_Score, TRUE ~ Away_Score)) %>%
  mutate(LScore = case_when(Home_Score < Away_Score ~ Home_Score, TRUE ~ Away_Score)) %>%
  mutate(WLoc = case_when(Home_Score > Away_Score ~ 'H', TRUE ~ 'A'))

library(tidyverse)
#select only columns used in function
ex_data = mut %>% select(WTeamID, LTeamID, WScore, LScore, WLoc)

################## DATA ANALYSIS  ###############################

##################### RPI #################################
#Create functions to calculate RPI 

#Winning Percentage

calc_wp <- function(game_data, team_id, exclusion_id = NULL){
  
  games_played <- game_data[game_data$WTeamID == team_id | game_data$LTeamID == team_id, ]
  
  if(!is.null(exclusion_id)){
    
    games_played <- 
      games_played[games_played$WTeamID != exclusion_id & games_played$LTeamID != exclusion_id, ]
    
    wp <- sum(games_played$WTeamID == team_id)/length(games_played$WTeamID)
    
  } else{
    
    wwins <- 1.4 * sum(games_played$WTeamID == team_id & games_played$WLoc == "A") +
      0.6 * sum(games_played$WTeamID == team_id & games_played$WLoc == "H") +
      sum(games_played$WTeamID == team_id & games_played$WLoc == "N")
    
    wlosses <- 1.4 * sum(games_played$LTeamID == team_id & games_played$WLoc == "A") +
      0.6 * sum(games_played$LTeamID == team_id & games_played$WLoc == "H") +
      sum(games_played$LTeamID == team_id & games_played$WLoc == "N")
    
    wp <- wwins/(wwins + wlosses)
    
  }
  
  return(wp)
  
}

calc_wp(ex_data, team_id = "Duke Blue Devils")


#Opponents Winning Percentage
calc_owp <- function(game_data, team_id){
  
  opp_games <- game_data[game_data$WTeamID == team_id | game_data$LTeamID == team_id, ]
  opps <- if_else(opp_games$WTeamID == team_id, opp_games$LTeamID, opp_games$WTeamID)
  
  owp <- opps %>%
    map_dbl(~ calc_wp(game_data, team_id = .x, exclusion_id = team_id))
  
  return(mean(owp))
  
}

calc_owp(ex_data, team_id = "Duke Blue Devils")

#Opponents oppenets Winning %

calc_oowp <- function(game_data, team_id){
  
  opp_games <- game_data[game_data$WTeamID == team_id | game_data$LTeamID == team_id, ]
  opps <- if_else(opp_games$WTeamID == team_id, opp_games$LTeamID, opp_games$WTeamID)
  
  oowp <- opps %>%
    map_dbl(~ calc_owp(game_data, team_id = .x))
  
  return(mean(oowp))
  
}

calc_oowp(ex_data, team_id = "Duke Blue Devils")

#Finally, RPI using the weights

calc_rpi <- function(game_data, team_id){
  
  rpi <- 0.25 * calc_wp(game_data, team_id) +
    0.5 * calc_owp(game_data, team_id) +
    0.25 * calc_oowp(game_data, team_id)
  
  return(round(rpi, 4))
  
}

calc_rpi(ex_data, team_id = "Duke Blue Devils")

ex_teams <- unique(c(ex_data$WTeamID, ex_data$LTeamID))

RPI_df = data_frame(Team = ex_teams,
           RPI = map_dbl(ex_teams, ~ calc_rpi(ex_data, team_id = .x)))





############################### SRS #########################################
all_teams <- unique(c(ex_data$WTeamID, ex_data$LTeamID))

# Function to create column vector of wins/losses for each team in every game

transform_wl <- function(game_data, team_id){
  
  col_w <- if_else(game_data$WTeamID == team_id, 1, 0) %>%
    na_if(0)
  
  col_l <- if_else(game_data$LTeamID == team_id, -1, 0) %>%
    na_if(0)
  
  col_all <- coalesce(col_w, col_l) %>%
    tbl_df()
  
  return(col_all)
  
}

# Replace NAs with 0 and cbind home/away column

srs_ex <- map(all_teams, ~ transform_wl(ex_data, team_id = .x)) %>%
  bind_cols() %>%
  setNames(all_teams) %>%
  replace(is.na(.), 0) %>%
  mutate(loc = fct_recode(ex_data$WLoc, "1" = "H", "-1" = "A", "0" = "N")) %>%
  mutate(loc = as.numeric(as.character(loc))) %>%
  select(loc, everything()) %>%
  as.matrix()

srs_ex

scorediff_ex <- ex_data %>%
  mutate(scorediff = WScore - LScore) %>%
  select(scorediff) %>% 
  as.matrix()

results_ex <- lsei(srs_ex, scorediff_ex)

srs = data_frame(Team = colnames(srs_ex),
           SRS = results_ex[[1]])


## Based on SRS - Home teams have ~1.2 point advantage

#Lets compare RPI to SRS
SRS_df = subset(srs, Team != "loc")

compare_df = cbind(SRS_df, RPI_df)

#While SRS and RPI are similar, there are some differences in rankings 
#SRS takes into account point differential and strength of schedule - 
#SRS rating is based on points above/below average (0 average)
#Therefore, I use SRS to rank ACC teams
# additional thoughts- considered doing regression and using coefficents as rankings - ex: lm(pointdiff ~ WTeam + LTeam + Location)

write.csv(compare_df, "ACCRankings1819.csv")



head(ex_data)


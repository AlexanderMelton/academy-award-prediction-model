################################################################################
###               04_model_validation - Oscar Prediction Model              ###
################################################################################

modelValidator <- function(year){
  tmp <- predictionModel(master[which(master$year != year),], 
                  master[which(master$year == year),])
  tmp$year <- year
  tmp
}

validation <- ldply(1990:2016, modelValidator)
validation <- validation[order(validation$year, -validation$prob_winning),]
winners <- master[which(master$winner == 1), "film"]
validation$winner <- ifelse(validation$film %in% winners, 1, 0)

winners_pool <- validation[which(validation$winner == 1),]
sum(winners_pool$proj_winner != winners_pool$winner)


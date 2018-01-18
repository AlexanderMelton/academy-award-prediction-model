################################################################################
###                 Model Validation - Oscar Prediction Model                ###
################################################################################

tree_model <- randomForest(winner ~ Nominations + noms_sqrd + length + budget + dg_nom + dg_win, 
                           data = master)

basic <- lm(winner ~ Nominations + noms_sqrd + length + budget + dg_nom + dg_win, 
            data = master)

plot(tree_model)
text(tree_model)

summary(tree_model)

predict(boot_pred, master[which(master$film == "Chicago"),])

bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 

boot_pred <- boot(data = master, statistic = bs, R = 1001,
                  formula =winner ~ Nominations + noms_sqrd + length + budget + dg_nom + dg_win)
summary(boot_pred)

#####

rfValidation <- function(year){
  tmp_train <- master[which(master$year != year),]
  tmp_test <- master[which(master$year == year),]
  
  model <- randomForest(winner ~ Nominations + noms_sqrd + length + budget + dg_nom + dg_win, 
                        data = master)
  
  tmp_test$prob_winning <- predict(model, tmp_test)
  return(tmp_test)
}
options(scipen=999)

model_output <- ldply(1990:2015, rfValidation)


year_2015 <- model_output[which(model_output$year == 2015),]

vector <- rep("Spotlight", 39)

populate <- function(film, prob){
  if(prob < 0.01){
    value = 1
  }else{
    value = ceiling(prob*100)
  }
  rep(film, value)
}

pred_2015 <- unlist(mapply(populate, year_2015$film, year_2015$prob_winning))

tally <- sample(pred_2015, 1001, replace = TRUE)
table(tally)
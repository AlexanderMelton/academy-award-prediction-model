################################################################################
###              03_model_creation - Oscar Prediction Model                  ###
################################################################################
predictionModel <- function(df_train, df_test){
  set.seed(05251929) #Date of First Academy Awards
  tree_model <- randomForest(winner ~ nominations + length + adj_budget +
                               dg_nom + dg_win + rt_score + screenplay_nom 
                               + visual_effects_nom + foreign,  data = df_train,
                             ntree = 50000)
  df_test$prob <- predict(tree_model, df_test)
  df_test$sims <- ifelse(df_test$pro < 0.01, 1, ceiling(df_test$pro*100))
  pool <- rep(df_test$film, df_test$sims)
  tally <- sample(pool, 1001, replace = TRUE)
  tmp <- as.data.frame(table(tally)) %>%
    rename(film = tally) %>%
    mutate(prob_winning = Freq/1001) %>%
    select(film, prob_winning)
  tmp$proj_winner <- ifelse(tmp$prob_winning == max(tmp$prob_winning), 1,0)
  tmp
}

predictionModel(master[which(master$year != 2013),], master[which(master$year == 2013),])

model <- lm(winner ~ nominations + length + adj_budget +
              dg_nom + dg_win + rt_score + screenplay_nom 
            + visual_effects_nom + foreign,  data = master[which(master$year != 2013),])
test <- 
predict(model, master[which(master$year == 2013),])


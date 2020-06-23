library(tidyverse)


#linear model with variables discovered
tennis_data_cleaned < 
init_w_pointswon <- lm(w_pointswon ~ w_n_winners + w_n_ue + Tour + as.factor(w_setswon) + l_n_ue + w_gameswon, tennis_data_linear)
summary(init_w_pointswon)
library(ggfortify)
autoplot(init_w_pointswon, 
         which = 1:6,
         alpha = .2,
         ncols = 3) +
  theme_bw()

#Train-Test split
set.seed(4345)
n_players <- nrow(tennis_data_linear)
train_i <- sample(n_players, n_players * 0.7, replace = FALSE)
test_i <- (1:n_players)[-train_i]
tennis_train <- tennis_data_linear[train_i,]
tennis_test <- tennis_data_linear[test_i,]

#3 models and their mse

candidate_model_1 <- lm(w_pointswon ~ w_n_winners + w_n_ue  + l_n_ue + w_gameswon, tennis_train)
model_1_preds <- predict(candidate_model_1, newdata = tennis_test)
model_1_mse <- mean((model_1_preds - tennis_test$w_pointswon)^2)
model_1_mse

candidate_model_2 <- lm(w_pointswon ~ w_n_winners + w_n_ue  + l_n_ue + w_gameswon + Tour, tennis_train)
model_2_preds <- predict(candidate_model_2, newdata = tennis_test)
model_2_mse <- mean((model_2_preds - tennis_test$w_pointswon)^2)
model_2_mse

candidate_model_3 <- lm(w_pointswon ~ w_n_winners + w_n_ue  + l_n_ue + w_gameswon + as.factor(w_setswon)+ Tour, tennis_train)
model_3_preds <- predict(candidate_model_3, newdata = tennis_test)
model_3_mse <- mean((model_3_preds - tennis_test$w_pointswon)^2)
model_3_mse

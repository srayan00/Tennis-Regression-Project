library(tidyverse)


#linear model with variables discovered
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


#K fold Cross Validation
set.seed(2020)
tennis_data_linear <- tennis_data_linear %>%
  mutate(test_fold = sample(rep(1:5, length.out = n())))
get_cv_preds <- function(model_formula, data = tennis_data_linear) {
  # generate holdout predictions for every row based season
  map_dfr(unique(data$test_fold), 
          function(holdout) {
            # Separate test and training data:
            test_data <- data %>%
              filter(test_fold == holdout)
            train_data <- data %>%
              filter(test_fold != holdout)
            # Train model:
            reg_model <- lm(as.formula(model_formula), data = train_data)
            # Return tibble of holdout results:
            tibble(test_preds = predict(reg_model, newdata = test_data),
                   test_actual = test_data$w_pointswon,
                   test_fold = holdout) 
          })
}


all_cv_preds <- get_cv_preds("w_pointswon ~ w_n_winners + Tour + tournament + w_n_ue + l_n_ue + w_gameswon")
no_tournament_cv_preds <- get_cv_preds("w_pointswon ~ w_n_winners + Tour + w_n_ue + l_n_ue + w_gameswon")
interaction_cv_preds <- get_cv_preds("w_pointswon ~ w_n_winners + Tour*tournament + w_n_ue + l_n_ue + w_gameswon")


bind_rows(mutate(all_cv_preds, type = "All"),
          mutate(no_tournament_cv_preds, type = "Excludes Tournament"),
          mutate(interaction_cv_preds, type = "Interaction between Tour & Tournament")) %>%
  group_by(type) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2))) %>%
  mutate(type = fct_reorder(type, rmse)) %>%
  ggplot(aes(x = type, y = rmse)) +
  geom_point() + coord_flip() + theme_bw() + xlab("Model Type") 


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

#random 3 models and their mse

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

#All 3 models and its coeffecient plots 

all_model <- lm(w_pointswon ~ w_n_winners + Tour + tournament + w_n_ue + l_n_ue + w_gameswon,
                data = tennis_data_linear)
ggcoef(all_model, 
       exclude_intercept = TRUE,
       vline = TRUE,
       vline_color = "red") + 
  theme_bw() +
  labs(title = "All Variables model") +
  xlab("Coeffecients") +
  ylab("Variables")
no_tournament_model <- lm(w_pointswon ~ w_n_winners + Tour + w_n_ue + l_n_ue + w_gameswon,
                          data = tennis_data_linear)
summary(interaction_model)
ggcoef(no_tournament_model, 
       exclude_intercept = TRUE,
       vline = TRUE,
       vline_color = "red") + 
  theme_bw() +
  labs(title = " All Variables with Tournament model") +
  xlab("Coeffecients") +
  ylab("Variables")
interaction_model <- lm(w_pointswon ~ w_n_winners + Tour*tournament + w_n_ue + l_n_ue + w_gameswon,
                        data = tennis_data_linear)
ggcoef(interaction_model, 
       exclude_intercept = TRUE,
       vline = TRUE,
       vline_color = "red") + 
  theme_bw() +
  labs(title = "All Variables with an interaction model") +
  xlab("Coeffecients") +
  ylab("Variables")


#Best model and its diagnostic plots
library(ggfortify)
autoplot(all_model, 
         which = 1:6,
         alpha = .2,
         ncols = 3) +
  theme_bw()

autoplot(all_model, 
         which = c(1, 2),
         alpha = .2,
         ncols = 2) +
  theme_bw()

autoplot(all_model, 
         which = c(3, 4),
         alpha = .2,
         ncols = 2) +
  theme_bw()
autoplot(all_model, 
         which = c(5, 6),
         alpha = .2,
         ncols = 2) +
  theme_bw()

#Trying elastic net regression
tennis_data_linear <- tennis_data_linear %>% na.omit()
model_x <- model.matrix(w_pointswon ~ ., tennis_data_linear)[, -c(1, 2)]
model_y <- tennis_data_linear$w_pointswon
set.seed(2020)
fold_id <- sample(rep(1:10, length.out = nrow(model_x)))
cv_en_25 <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = .25)
cv_en_50 <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = .5)
cv_ridge <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 0)
cv_lasso <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 1)
which.min(c(min(cv_en_25$cvm), min(cv_en_50$cvm), min(cv_ridge$cvm), min(cv_lasso$cvm)))
coef(cv_en_50)


#A more effecient way of finding optimal alpha
alphalist <- seq(0,1,by=0.1)
elasticnet <- lapply(alphalist, function(a){
  cv.glmnet(model_x, model_y, alpha=a, foldid = fold_id)
})
for (i in 1:11) {print(min(elasticnet[[i]]$cvm))}
elastic <- elasticnet[[5]]
plot(elastic)
coef(elastic)



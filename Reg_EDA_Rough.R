#Loading and Cleaning Data Set
tennis_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/regression_projects/tennis_2013_2017_GS.csv")
tennis_data_cleaned <- tennis_data %>% 
  select(-c("w_n_netpt_w", "w_n_netpt", "slam", "player1", "player2", "a1", "a2", "X1", "match_id.y")) %>% 
  rename(Net_points = l_n_netpt,
         Net_points_won = l_n_netpt_w) %>% 
  mutate(w_bp_success = w_n_bp_w/w_n_bp,
         l_bp_success = l_n_bp_w/l_n_bp,
         w_sv_success = w_n_sv_w/w_n_sv,
         l_sv_success = l_n_sv_w/l_n_sv,
         upset_bool = ifelse(winner_rank >loser_rank, TRUE, FALSE)) %>% 
  filter(Retirement == FALSE) %>% 
  filter(w_setswon %in% c(2,3))

table("retirement" =tennis_data$Retirement, "upset_bbool" =tennis_data_cleaned$upset_bool)
table(tennis_data_cleaned$upset_bool)

# service success and service speed scatterplot

tennis_data_cleaned %>% 
  filter(w_ave_serve_speed != 0) %>% 
  ggplot(aes(x = w_sv_success, y = w_ave_serve_speed)) +
  geom_point(alpha = 0.2) +
  geom_smooth(color = "darkred", method = "lm") +
  theme_bw() +
  xlab("Winner's service success rate") +
  ylab("Winner's average serve speed") +
  labs(
    title = "Scatterplot"
  )

cor((tennis_data_cleaned$w_sv_success)^2, tennis_data_cleaned$w_ave_serve_speed, method = "pearson")

#Looking at how upset influences other variables 
tennis_data_cleaned %>% 
  group_by(upset_bool) %>% 
  summarise(mean_service = mean(w_sv_success)) %>% 
  ggplot(aes(x = upset_bool, y = mean_service)) +
  geom_col()

#looking for continuous variables with a normal distribution
tennis_data_cleaned %>% 
  #filter((w_pointswon > 50) & (w_pointswon < 200)) %>% 
  mutate( w_pointsgames = w_pointswon/ w_gameswon) %>% 
  ggplot() +
  geom_histogram(aes(x = w_pointsgames)) +
  theme_bw()

#EDA

#Strong correlation (0.567) between unforced errors and number of winners for a winning player
tennis_data_cleaned %>% 
  ggplot(aes(x = w_n_ue, y = w_n_winners)) +
  geom_point(alpha = 0.2, aes(color = Tour)) +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_bw()
cor(tennis_data_cleaned$w_n_ue, tennis_data_cleaned$w_n_winners, method = "pearson")  



#messing around. Is this obvious, That winners and service wins are linearly related. This is the case for winning and
#losing player
tennis_data_cleaned %>% 
  filter(w_n_winners < 80) %>% 
  ggplot(aes(x = w_n_sv_w, y = w_n_winners)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_bw()
cor(tennis_data_cleaned$w_n_winners, tennis_data_cleaned$w_n_sv_w, method = "pearson")


#messing around. Ace frequency and serve success??
tennis_data_cleaned %>% 
  ggplot(aes(x = l_n_aces/l_n_sv, y = l_sv_success)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_bw()
cor(tennis_data_cleaned$w_n_winners, tennis_data_cleaned$w_n_aces, method = "pearson")

#strong correlation between losing players unforced errors and winning player's points earned
tennis_data_cleaned %>% 
  ggplot(aes(x = w_pointswon, y = l_n_ue)) +
  geom_point(alpha = 0.2, aes(color = Tour)) +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_bw()
cor(tennis_data_cleaned$l_n_ue, tennis_data_cleaned$w_pointswon, method = "pearson")

#box plots for tour and w_pointswons
tennis_data_cleaned %>%
  ggplot(aes(x = Tour, y = w_pointswon)) +
  geom_violin() +
  geom_boxplot(width = .2) + 
  ylab("Winning player's Total points won") +
  scale_x_discrete(labels = c("Association for Tennis Players (Men's)", "Womens Tennis Association")) +
  theme_bw()

#box plots for sets wons and w_pointswon
tennis_data_cleaned %>%
  ggplot(aes(x = as.factor(w_setswon), y = w_pointswon)) +
  geom_violin() +
  geom_boxplot(width = .2) + 
  ylab("Winning player's Total points won") +
  theme_bw()

#determining the linear relationship between winner's games won and winner's points won
tennis_data_cleaned %>% 
  ggplot(aes(x = w_gameswon, y = w_pointswon)) +
  geom_point(alpha = 0.2, aes(color = Tour)) +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_bw()

#determining the linear relationship between winning player's number of winners and winner's points won
tennis_data_cleaned %>% 
  ggplot(aes(x = w_n_winners, y = w_pointswon)) +
  geom_point(alpha = 0.2, aes(color = Tour)) +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_bw()
cor(tennis_data_cleaned$w_n_winners, tennis_data_cleaned$w_pointswon, method = "pearson")

#Rough correlation matrix with some continuous variables

library(ggcorrplot)
tennis_model_data <- tennis_data_cleaned %>%
  dplyr::select(w_n_winners,
                l_n_ue,
                w_n_ue,
                w_bp_success,
                w_pointswon
                 
  )
nfl_cor_matrix <- cor(tennis_model_data)
ggcorrplot(nfl_cor_matrix)
round_cor_matrix <- 
  round(cor(tennis_model_data), 2)
ggcorrplot(round_cor_matrix, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

table("tour" = tennis_data_linear$Tour,
      "sets won" = as.factor(tennis_data_linear$w_setswon))

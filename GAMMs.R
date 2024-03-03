#Honing Behave output by separating seasons as separate models and running as GAMS. ----

bh3.2_early <- subset(behave_output3.2, season == "early")

bh3.2_early <- bh3.2_early %>% drop_na(PIG)

linear_model <- gam(PIG ~ mean_LAI, data = bh3.2_early)
summary(linear_model)

#and plot the linear model
data_plot <- ggplot(data = bh3.2_early, aes(y = PIG, x = mean_LAI)) +
  geom_point() +
  geom_line(aes(y = fitted(linear_model)),
            colour = "red", size = 1.2) +
  theme_bw()
data_plot

#Now we will try a GAM (same formula, but add s for smooth function)
gam_model <- gam(PIG ~ s(mean_LAI), data = bh3.2_early)
summary(gam_model)

data_plot <- data_plot +
  geom_line(aes(y = fitted(gam_model)),
            colour = "blue", size = 1.2)
data_plot

plot(gam_model)

linear_model <- gam(PIG ~ mean_LAI, data = bh3.2_early)
smooth_model <- gam(PIG ~ s(mean_LAI), data = bh3.2_early)
AIC(linear_model, smooth_model)

#LATE
bh3.2_late <- subset(behave_output3.2, season == "late")

bh3.2_late <- bh3.2_late %>% drop_na(PIG)

linear_model <- gam(PIG ~ mean_LAI, data = bh3.2_late)
summary(linear_model)

#and plot the linear model
data_plot <- ggplot(data = bh3.2_late, aes(y = PIG, x = mean_LAI)) +
  geom_point() +
  geom_line(aes(y = fitted(linear_model)),
            colour = "red", size = 1.2) +
  theme_bw()
data_plot

#Now we will try a GAM (same formula, but add s for smooth function)
gam_model <- gam(PIG ~ s(mean_LAI), data = bh3.2_late)
summary(gam_model)

data_plot <- data_plot +
  geom_line(aes(y = fitted(gam_model)),
            colour = "blue", size = 1.2)
data_plot

plot(gam_model)

linear_model <- gam(PIG ~ mean_LAI, data = bh3.2_late)
smooth_model <- gam(PIG ~ s(mean_LAI), data = bh3.2_late)
AIC(linear_model, smooth_model)

#Seasons modeled together included as a factor and an interaction term ----
#change seasons to factor
behave_output3.2$season <- factor(behave_output3.2$season)

#drop NAs
behave_output3.2 <- behave_output3.2 %>% drop_na(PIG)

#GAM with interaction
factor_interact <- gam(PIG ~ season + s(mean_LAI, by = season),
                         data = behave_output3.2, method = "REML")

summary(factor_interact)
k.check(factor_interact)
gam.check(factor_interact)


#looks great under the normal distribution

#PLOT
new_data <- expand.grid(mean_LAI = seq(min(behave_output3.2$mean_LAI), max(behave_output3.2$mean_LAI), length = 100),
                        season = unique(behave_output3.2$season))

# Make predictions using the GAM model
new_data$predicted <- predict(factor_interact, newdata = new_data, type = "response")

# Create a plot using ggplot2
ggplot(new_data, aes(x = mean_LAI, y = predicted, color = season)) +
  geom_line() +
  geom_point(data = behave_output3.2, aes(x = mean_LAI, y = PIG, color = season), alpha = 0.5) +
  labs(title = "Relationship between canopy cover and PIG by Season",
       x = "mean_LAI",
       y = "PIG") +
  theme_minimal()



#NOW TO ADD RANDOM EFFECT FOR PLOT (FROM GAM TO GAMM) ----
#first, make plot_id a factor
behave_output3.2$plot_id <- factor(behave_output3.2$plot_id)

# Assuming 'response' is your continuous response variable,
# 'predictor' is your continuous predictor variable,
# 'season' is your factor variable with two levels (early and late),
# and 'plot_id' is your factor variable with 10 levels.

# Creating a GAM model with a random intercept for 'plot_id'
gamm_model<- gam(PIG ~ season + s(mean_LAI, by = season) + s(plot_id, bs = "re"),
                       data = behave_output3.2, method = "REML")

summary(gamm_model)


#PLOT
new_data <- expand.grid(mean_LAI = seq(min(behave_output3.2$mean_LAI), max(behave_output3.2$mean_LAI), length = 100),
                        season = unique(behave_output3.2$season))

# Make predictions using the GAM model
new_data$predicted <- predict(factor_interact, newdata = new_data, type = "response")

# Create a plot using ggplot2
ggplot(new_data, aes(x = mean_LAI, y = predicted, color = season)) +
  geom_line() +
  geom_point(data = behave_output3.2, aes(x = mean_LAI, y = PIG, color = season), alpha = 0.5) +
  labs(title = "Seasonal Difference in PIG and LAI (GAMM)",
       x = "LAI",
       y = "PIG") +
  theme_minimal()

##Random effect didn't change much so I think we can just stick with the GAM! Unless for like research design reasons we need to include the effect?


#Were gonna check out random slope + intercept ----

gamm_slope_int <- gam(PIG ~ season + s(mean_LAI, by = season) + s(plot_id, mean_LAI, bs = "re"),
                 data = behave_output3.2, method = "REML")

summary(gamm_slope_int)

#okay, it didn't change anything because it seems plot_id really doesn't matter to PIG


# But For future reference here is how to compare random intercepts for when plot id is a random effect.

plot_smooth(gamm_model_tw, view = "mean_LAI", rm.ranef = FALSE, cond = list(plot_id = "ddf2"),
            main = "... + s(plot_id)", col = "orange", ylim = c(-10,5))
plot_smooth(gamm_model_tw, view = "mean_LAI", rm.ranef = FALSE, cond = list(plot_id = "ddf3"),
            add = TRUE, col = "red")
plot_smooth(gamm_model_tw, view = "mean_LAI", rm.ranef = FALSE, cond = list(plot_id = "ddf4"),
            add = TRUE, col = "purple")
plot_smooth(gamm_model_tw, view = "mean_LAI", rm.ranef = FALSE, cond = list(plot_id = "ddf5"),
            add = TRUE, col = "turquoise")
plot_smooth(gamm_model_tw, view = "mean_LAI", rm.ranef = FALSE, cond = list(plot_id = "ddf6"),
            add = TRUE, col = "green")
plot_smooth(gamm_model_tw, view = "mean_LAI", rm.ranef = FALSE, cond = list(plot_id = "evg1"),
            add = TRUE, col = "black")
plot_smooth(gamm_model_tw, view = "mean_LAI", rm.ranef = FALSE, cond = list(plot_id = "evg2"),
            add = TRUE, col = "magenta")
plot_smooth(gamm_model_tw, view = "mean_LAI", rm.ranef = FALSE, cond = list(plot_id = "evg3"),
            add = TRUE, col = "blue")
plot_smooth(gamm_model_tw, view = "mean_LAI", rm.ranef = FALSE, cond = list(plot_id = "evg4"),
            add = TRUE, col = "forestgreen")
plot_smooth(gamm_model_tw, view = "mean_LAI", rm.ranef = FALSE, cond = list(plot_id = "evg5"),
            add = TRUE, col = "yellow")

## This is the same as above but added plot_id to test_data for the graph which looks exactly the same idk,----
##i think its just because the plot_id was not very relevant in this instance. 
test_model <- gam(PIG ~ season + s(mean_LAI, by = season) + s(plot_id, bs = "re"),
                  data = behave_output3.2, method = "REML")

summary(test_model)

# PLOT using the GAMM model
test_data <- expand.grid(mean_LAI = seq(min(behave_output3.2$mean_LAI), max(behave_output3.2$mean_LAI), length = 100),
                        season = unique(behave_output3.2$season),
                        plot_id = unique(behave_output3.2$plot_id))

# Make predictions using the GAMM model
test_data$predicted <- predict(test_model, newdata = test_data, type = "response")

# Create a plot using ggplot2
ggplot(test_data, aes(x = mean_LAI, y = predicted, color = season)) +
  geom_line() +
  geom_point(data = behave_output3.2, aes(x = mean_LAI, y = PIG, color = season), alpha = 0.5) +
  labs(title = "Seasonal Difference in PIG and LAI (GAMM)",
       x = "LAI",
       y = "PIG") +
  theme_minimal()

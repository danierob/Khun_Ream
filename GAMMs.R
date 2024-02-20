#Honing Behave output by separating seasons as separate models and running as GAMS.

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

#PLOT
new_data <- expand.grid(mean_LAI = seq(min(behave_output3.2$mean_LAI), max(behave_output3.2$mean_LAI), length = 100),
                        season = unique(behave_output3.2$season))

# Make predictions using the GAM model
new_data$predicted <- predict(factor_interact, newdata = new_data, type = "response")

# Create a plot using ggplot2
ggplot(new_data, aes(x = mean_LAI, y = predicted, color = season)) +
  geom_line() +
  geom_point(data = behave_output3.2, aes(x = mean_LAI, y = PIG, color = season), alpha = 0.5) +
  labs(title = "Relationship between mean_LAI and PIG by Season",
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
  labs(title = "Seasonal Difference in PIG and LAI (GAM)",
       x = "LAI",
       y = "PIG") +
  theme_minimal()

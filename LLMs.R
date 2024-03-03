# Look at the distribution of continuous variables:
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))

#Grass mass Histogram
#remove the massive bamboo outlier in DDF5 Q4
#remove huge bamboo outlier 
z_scores <- scale(LAI_earlyxbiomass_seasons$grass_mass)
outliers <- abs(z_scores) > 3  # Adjust the threshold as needed

# Create a subset without outliers
data <- LAI_earlyxbiomass_seasons[!outliers, ]

#check out the histogram
hist(data$grass_mass, xlab = "Grass Load (g/m2)", main = "")
#grass has a really right skewed distribution from all of the 0 or low mass areas


#standaradize LAI
data$scale_LAI <- scale(data$mean_LAI, center = TRUE, scale = TRUE)

ggplot(data = data, aes(y = litter_mass, x = scale_LAI, color = season)) +
  geom_point() + geom_smooth()

ggplot(data = data, aes(y = litter_mass, x = mean_LAI, color = season)) +
  geom_point() + geom_smooth()


#lmm model for gras_mass
model <- lmer(grass_mass ~ mean_LAI*season + (1 | plot_id), data = data, REML = TRUE)

summary(model)

#check assumptions
par(mar = c(4, 4, 0.5, 0.5))
plot(resid(model) ~ fitted(model), xlab = "Predicted values", ylab = "Normalized residuals")
abline(h = 0, lty = 2)



## LAIxFM
model1 <- lmer(grass_mass ~ mean_LAI*season + (1|plot_id), data = data, REML = TRUE)
summary(model1)

qqnorm(resid(model1))
qqline(resid(model1))  

model2 <- lmer(litter_mass ~ mean_LAI*season + (1|plot_id), data = data, REML = TRUE)
summary(model2)

qqnorm(resid(model2))
qqline(resid(model2))  

sim_residuals <- DHARMa::simulateResiduals(model2)

DHARMa::plotSimulatedResiduals(model2)
shapiro.test(sim_residuals$scaledResiduals)

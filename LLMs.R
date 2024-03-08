#setup ----
#see 1.1.1 to bring in LAI data, creation of LAI_1 and biomass data. This will all look very similar to 1.1.1, the only exception being that biomass will include early and late season observations. Where as in 1.1.1 we only looked at early season obs. 

#loading in LAI data
LAI <- read_csv("C:/Users/d/Desktop/UBC 2023/Robinson-Project-MSc/KH_data/LAI/LAI_V1.csv", 
                col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                                 LAI = col_number()))

#make season a factor
LAI$season <- factor(LAI$season)

#make plot_id a factor
LAI$plot_id <- factor(LAI$plot_id)

biomass <- read_csv("C:/Users/d/Desktop/UBC 2023/Robinson-Project-MSc/KH_data/Biomass/biomass_V4.csv", 
                    col_types = cols(grass_height = col_number(), 
                                     litter_depth = col_number(), grass_per = col_number(), 
                                     wood_per = col_number(), bare_per = col_number(), 
                                     litter_per = col_number(), grass_mass = col_number(), 
                                     other_mass = col_number(), wood_mass = col_number(), 
                                     litter_mass = col_number(), seedlings = col_integer()))

#make season a factor
biomass$season <- factor(biomass$season)

#make plot_id a factor
biomass$plot_id <- factor(biomass$plot_id)


#we again must average the 3 biomass obs taken at each logger for one observation per logger per seasonal period for a total of 100 (50 x 2) minus 8 equals 92 records.
biomass_seasons <- biomass %>%
  group_by(plot_id, quad_id, season) %>% 
  summarise(grass_mass = mean(grass_mass), litter_mass = mean(litter_mass), 
            wood_mass = mean(wood_mass), other_mass = mean(other_mass), 
            grass_height = mean(grass_height), litter_depth = mean(litter_depth), 
            grass_per = mean(grass_per), litter_per = mean(litter_per))
#isolating and averaging LAI ob 1&2  data across all observations for one value per quadrant (n=50)

LAI_early <- LAI %>%
  group_by(plot_id, quad_id) %>%
  filter(obs_period == "1" | obs_period == "2") %>% 
  summarise(mean_LAI = mean(LAI))

#merging LAI_early and biomass_seasons into LAIxbiomass_seasons
LAI_earlyxbiomass_seasons <- right_join(LAI_early, biomass_seasons, by = c("plot_id", "quad_id"))

#drop the quadrants that did not have biomass records
LAI_earlyxbiomass_seasons <- LAI_earlyxbiomass_seasons %>% drop_na(grass_mass)



# Look at the distribution of continuous variables: ----


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

hist(data$mean_LAI, xlab = "Grass Load (g/m2)", main = "")
#GRASS ----

#Creating an lmm model for grass load
model <- lmer(grass_mass ~ mean_LAI*season + (1 | plot_id), data = data)
#plot the model to check out the residuals fit
plot(model)
#looking pretty crazy, there are loads of 0s that line up but beyond that there
#is clear heteroskedasticity

#log the grass load to improve heteroskedasticity (still have 0s lined up, that's fine)
data$grass_ln <- log(data$grass_mass + 1)  #have to add a 1 to the ln to avoid NANs
model_ln <- lmer(grass_ln ~ mean_LAI*season + (1 | plot_id), data = data)

plot(model_ln)

###Testing Assumptions
sim_residuals <- DHARMa::simulateResiduals(fittedModel = model_ln)
plotSimulatedResiduals(sim_residuals) # For visual validation of (i)
shapiro_test <- shapiro.test(sim_residuals$scaledResiduals) # For statistical validation of (i)
plotResiduals(sim_residuals) # For visual validation of (iii)
random_effects <- lme4::ranef(model_ln) # For visual validation of (iv)
plot(random_effects)

#plot grass_ln out
ggplot(data = data, aes(x = mean_LAI, y = grass_mass)) +
  geom_point(alpha=0.4) + geom_smooth() +
  labs(x="LAI", y="Litter Load (g/m2)", color="Season", fill="Season")

#trying out piecewise model with the segmented function
library(segmented)
?segmented
?rep

model_ln <- lmer(grass_ln ~ mean_LAI*season + (1 | plot_id), data = data)

seg_grass <- segmented(model_ln, ~mean_LAI, x.diff = ~season, z.psi= ~1, 
                 random = list(plot_id = pdDiag(~ 1 + mean_LAI + U + G0)))

#trying just a plain old lm (no random effects), because I just want to identify breakpoints using segmented()
model_ln <- lm(grass_mass ~ mean_LAI*season, data = data)



###Litter ----
#Checking out mixed effects models with interaction between mean_LAI and season
#to allow for variable slopes between levels (early and late) of season 

#random intercepts model
m1 <- lmer(litter_mass ~ mean_LAI*season + (1|plot_id), data = data, REML = TRUE)
summary(m1)

#ran int and slope model
m2 <-  lmer(litter_mass ~ mean_LAI*season + (1 + season|plot_id), data = data, REML = TRUE)
summary(m2)

#comparing random slopes and random intercepts models with anova
anova(m1,m2)


###Testing Assumptions
sim_residuals <- DHARMa::simulateResiduals(fittedModel = model2)
plotSimulatedResiduals(sim_residuals) # For visual validation of (i)
shapiro_test <- shapiro.test(sim_residuals$scaledResiduals) # For statistical validation of (i)
plotResiduals(sim_residuals) # For visual validation of (iii)
random_effects <- lme4::ranef(model2) # For visual validation of (iv)
plot(random_effects)


### Interpreting coefficients wiht CIs using emmeans
summary(m2)

emtrends(m2, ~ season, var="mean_LAI")

#SO, late season does not include 0 in the 95% CI but early season does, so
#litter load is significant across the LAI gradient in the late but not the early season

#Now lets graphically represent this
mylist <- list(mean_LAI=seq(0,7,by=1))

#emmip
lit_dat <- emmip(m1,season~mean_LAI, at=mylist , CIs=TRUE, plotit = FALSE)

#make ggplot with emmip data
p <- ggplot(data = lit_dat, aes(x = mean_LAI, y = yvar, color = season)) +
  geom_line() + geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=season), alpha=0.4) +
  labs(x="LAI", y="Litter Load (g/m2)", color="Season", fill="Season")

#add points from the original data
p + geom_point(data = data, aes(x = mean_LAI, y = litter_mass, color = season), alpha = .5) 


## pairwise comparisons across season with emmeans
em_m2 <- emmeans(m2, ~mean_LAI*season, at = mylist)

contrast(em_m2, "pairwise", by="mean_LAI")

### Pairwise comparison - Significant differences in early and late at all LAI levels

data %>% 
  cor.test(mean_LAI ~ as.numeric(season))

segmented_model <- segmented(m2, seg.Z = ~x)


# Create a data frame named Cefamandole with one column 'z'
Cefamandole <- data.frame(z = numeric(84))

# Assign a new variable 'z' with a random vector
Cefamandole$z <- rep(runif(6), rep(14, 6))

# Display the resulting data frame
print(Cefamandole)

# Plot the values of the variable 'z'
plot(Cefamandole$z, pch = 16, col = "blue", main = "Scatter Plot of Variable 'z'", xlab = "Index", ylab = "Values")



library(lme4)
str(sleepstudy)

#Basis functions
bp = 4
b1 <- function(x, bp) ifelse(x < bp, bp - x, 0)
b2 <- function(x, bp) ifelse(x < bp, 0, x - bp)

#Mixed effects model with break point = 4
(mod <- lmer(Reaction ~ b1(Days, bp) + b2(Days, bp) + (b1(Days, bp) + b2(Days, bp) | Subject), data = sleepstudy))

           
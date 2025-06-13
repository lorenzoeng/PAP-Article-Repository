
# Code for the analysis of the article
#################################################################################################################
### Precision Sports Science: The case of Post-Activation Potentiation and the baseline dependency phenomenon ###
#################################################################################################################
#### This R script includes: ####
# Part 1: An analysis of baseline torque variables and PAP considering each intervention as independent
# - Step 1: Building null models
# - Step 2: Visualizing null models
# - Step 3: Visualizing baseline-PAP relationships and their interactions with Sets and Reps
# - Step 4: Comparing baseline torque models with Null models
# - Step 5: Case bootstrapping model estimates and confidence intervals
# - Step 6: Visualizing random effects by model
# Part 2: An analysis of baseline torque and PAP relationship differences between interventions (combined intervention analysis)
# - Step 1: Model specification (combined intervention Null and Baseline model specifications)
# - Step 2: Null Vs Baseline model comparisons (combined dataset and models for both interventions)
# - Step 3: Case bootstrapping of combined intervention model estimates
# - Step 4: Visualizing random effects by model
# Part 3: Model diagnostic plots
# Part 4: 3D plots of baseline TT/MVIC models


#### Load Necessary files ####
# load necessary packages
library(tidyr); library(dplyr); library(readxl); library(ggplot2); library(stringr); library(tidyverse); library(patchwork); library(MuMIn); library(broom.mixed); library(lmerTest); library(performance); library(gridExtra); library(lmeresampler); library(grid); library(writexl); library(plotly); library(htmlwidgets)
# load the data
data <- read_excel("ADD THE PATH TO THE DATASET CONTAINING THE RAW DATA")
#### Pre-process data before fitting the models ####
# Split the data into two datasets based on the intervention variable
post_set_data_3s <- data %>% filter(intervention == "3s")
post_set_data_6s <- data %>% filter(intervention == "6s")
# Center data by subtracting the mean of the sample
post_set_data_3s <- post_set_data_3s %>%
  mutate(
    baseline_twitch_torque_c = scale(baseline_twitch_torque, center = TRUE, scale = FALSE),
    baseline_tt_mvic_ratio_c = scale(baseline_tt_mvic_ratio, center = TRUE, scale = FALSE),
    baseline_mvic_c = scale(baseline_mvic, center = TRUE, scale = FALSE),
    Rep_c = scale(Rep, center = TRUE, scale = FALSE),
    Set_c = scale(Set, center = TRUE, scale = FALSE))
post_set_data_6s <- post_set_data_6s %>%
  mutate(
    baseline_twitch_torque_c = scale(baseline_twitch_torque, center = TRUE, scale = FALSE),
    baseline_tt_mvic_ratio_c = scale(baseline_tt_mvic_ratio, center = TRUE, scale = FALSE),
    baseline_mvic_c = scale(baseline_mvic, center = TRUE, scale = FALSE),
    Rep_c = scale(Rep, center = TRUE, scale = FALSE),
    Set_c = scale(Set, center = TRUE, scale = FALSE))
#### ...Pre-process combined intervention dataset ####
# Mean Center predictor vars
data <- data %>%
  group_by(intervention) %>%
  mutate(
    baseline_twitch_torque_c = scale(baseline_twitch_torque, center = TRUE, scale = FALSE),
    baseline_tt_mvic_ratio_c = scale(baseline_tt_mvic_ratio, center = TRUE, scale = FALSE),
    baseline_mvic_c = scale(baseline_mvic, center = TRUE, scale = FALSE),
    Rep_c = scale(Rep, center = TRUE, scale = FALSE),
    Set_c = scale(Set, center = TRUE, scale = FALSE)
  ) %>%
  ungroup()



#### Part 1: Step 1. Building null models ####
#### ...Visualsing individual participant trajectories ####
# Visualization of Responses over the interventions
plot_individual <- 
  ggplot(data, aes(x = Rep, y = twitch_torque_response, group = Part_id, color = factor(Part_id))) +
  geom_line(size = 0.5) +
  geom_point(size = 2) +
  labs(
    title = "Twitch Torque Response Across Repetitions by Intervention and Set",
    x = "Repetition (Rep)",
    y = "Twitch Torque Response",
    color = "Participant ID") +
  theme_minimal() + 
  facet_grid(intervention ~ Set) +  
  theme(
    legend.position = "right",                     # Position of the legend
    strip.text = element_text(size = 20),          # Size of facet labels
    strip.text.x = element_text(size = 20),        # Size of x facet labels (Set)
    strip.text.y = element_text(size = 20),        # Size of y facet labels (Intervention)
    plot.title = element_text(size = 22, face = "bold"),          # Plot title size
    axis.title.x = element_text(size = 20),                       # X axis title size
    axis.title.y = element_text(size = 20),                       # Y axis title size
    legend.title = element_text(size = 20),                        # Legend title size
    axis.text.y = element_text(size = 16),                # Y-axis tick label size
    axis.text.x = element_text(size = 16))

plot_individual
##############  ...Null model 3s intervention ##############
Null_3s_1 <- lmer(twitch_torque_response ~  
                    # Fixed effects
                    Rep_c + Set_c
                  # Random effects
                  + (1 + Rep_c + Set_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
Null_3s_2 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c
                  # Random effects
                  + (1 + Rep_c + Set_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
# Singular warning for Null_3s_2 model.
# The inclusion of an interaction between repetitions and sets produces a singular warning.
summary(Null_3s_2)
# Investigation of random effects:
# Random slope for Rep is highly correlated with both the participant intercept (0.65) and the set ran.slope (0.72)
# It also does not explain a lot of variability..
# Try symplyfying the random effects by removing the correlations of the intercept and set slope with repetition slope.
Null_3s_2 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c 
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
# fit Null_3s_1 model again with the same random effect structure as Null_3s_2 to isolate the fixed effects contributions
Null_3s_1 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c + Set_c 
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
# Removing the random effect correlation fixed the singular warning
anova(Null_3s_1, Null_3s_2) 
AICc(Null_3s_1, Null_3s_2)
# The interaction between Rep and Set improves the null model
# Null_3s_2 is used as the null from now on
Null_3s_3 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
anova(Null_3s_2, Null_3s_3)
AICc(Null_3s_2, Null_3s_3)
# The inclusion of a quadratic effect for Rep improves the model substantially!
# Null_3s_3 is used as the null from now on
# Add a random quadratic slope for the quadratic effect of repetition..
Null_3s_4 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c + I(Rep_c^2) | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
# It is likely the random slope of Repetition and the quadratic random slope of repetition are highly correlated.
anova(Null_3s_3, Null_3s_4)
AICc(Null_3s_3, Null_3s_4)
# The model with the random quadratic slope is preferred!
Null_3s_5 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) + I(Rep_c^3)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c + I(Rep_c^2) | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
anova(Null_3s_4, Null_3s_5)
AICc(Null_3s_4, Null_3s_5)
# A cubic effect for repetition improves the model
# continue with Null_3s_5 as the null model..
Null_3s_6 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) + I(Rep_c^3) + I(Set_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c + I(Rep_c^2) | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
anova(Null_3s_5, Null_3s_6)
AICc(Null_3s_5, Null_3s_6)
# A quadratic effect for set improves the model
# continue with Null_3s_6 as the null model
Null_3s_7 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) + I(Set_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c + I(Rep_c^2) | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
anova(Null_3s_6, Null_3s_7)
AICc(Null_3s_6, Null_3s_7)
# The model including set as a moderator of the quadratic effect of repetition improves the model
# Use the Null_3s_7 from now on as the null model..
Null_3s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) * Set_c + I(Set_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c + I(Rep_c^2) | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
anova(Null_3s_7, Null_3s_8)
AICc(Null_3s_7, Null_3s_8)
# Including a moderating effect of the cubic effect of repetition does not improve the model.
# Use Null_3s_7 from now on as the null model..
# Add a cubic random slope for repetition..
Null_3s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) + I(Set_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c + I(Rep_c^2) + I(Rep_c^3) | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
summary(Null_3s_8)
# singular fit, simplify the random effect structure
# remove the correlation between the rep random effects..
Null_3s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) + I(Set_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c + I(Rep_c^2) | Part_id) + (0 + I(Rep^3) | Part_id)
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
# Singular issue persists... remove the cubic random slope for repetition
# Add random quadratic repetition slopes for each set for each participant
Null_3s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) + I(Set_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c + I(Rep_c^2) | Part_id)
                  + (1 + Rep_c + I(Rep_c^2) | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
# singular fit
summary(Null_3s_8)
# Repetition slope and quadratic repetition slope under Participants are perfectly correlated..
# remove the quadratic repetition slope under participants and compare models..
Null_3s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) + I(Set_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                  + (1 + Rep_c + I(Rep_c^2) | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
anova(Null_3s_7, Null_3s_8)
AICc(Null_3s_7, Null_3s_8)
# Modelling the random quadratic slopes within sets for each participant (Null_3s_8), improves the model,
# compared to modelling the random quadratic slopes for each participant averaged across sets.
# continue with Null_3s_8
# Add a random quadratic slope for sets
Null_3s_9 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) + I(Set_c^2)
                  # Random effects
                  + (1 + Set_c + I(Set_c^2) | Part_id) + (0 + Rep_c | Part_id)
                  + (1 + Rep_c + I(Rep_c^2) | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
# Convergence issue
summary(Null_3s_9)
# The random quadratic slope for set is highly correlated with random participant intercept (-0.96)
# Further it does not explain any significant amount of variance..
# The random quadratic slope is not needed in the model
# Keep Null_3s_8
anova(Null_3s_1, Null_3s_2, Null_3s_3, Null_3s_4, Null_3s_5, Null_3s_6, Null_3s_7, Null_3s_8)
AICc(Null_3s_1, Null_3s_2, Null_3s_3, Null_3s_4, Null_3s_5, Null_3s_6, Null_3s_7, Null_3s_8)
# The final Null_3s model is defined as the "Null_3s_8"
Null_3s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) + I(Set_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                  + (1 + Rep_c + I(Rep_c^2) | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)


#################  ...Null model 6s intervention ##############
Null_6s_1 <- lmer(twitch_torque_response ~  
                    # Fixed effects
                    Rep_c + Set_c
                  # Random effects
                  + (1 + Rep_c + Set_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
Null_6s_2 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c
                  # Random effects
                  + (1 + Rep_c + Set_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
# Singular warning for Null_6s_2 model.
# The inclusion of an interaction between repetitions and sets produces a singular warning.
summary(Null_6s_2)
# Investigation of random effects:
# Random slope for Set is highly correlated with the participant random slope for repetitions (0.99) 
# random slope for Set explains more variability than the random slope for repetition so it's kept as a correlated ran.effect.
# Try simplifying the random effects by specifying random slope for repetition as uncorrelated in the randome eff. structure.
Null_6s_2 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c 
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
# fit Null_6s_1 model again with the same random effect structure as Null_6s_2 to isolate the fixed effects contributions
Null_6s_1 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c + Set_c 
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
# Removing the random effect correlation fixed the singular warning
anova(Null_6s_1, Null_6s_2) 
AICc(Null_6s_1, Null_6s_2)
# The interaction between Rep and Set improves the null model
# Null_6s_2 is used as the null from now on
Null_6s_3 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
anova(Null_6s_2, Null_6s_3)
AICc(Null_6s_2, Null_6s_3)
# The inclusion of a quadratic effect for Rep improves the model substantially!
# Null_6s_3 is used as the null from now on
# Add a random quadratic slope for the quadratic effect of repetition..
Null_6s_4 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c + I(Rep_c^2) | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
# It is likely the random slope of Repetition and the quadratic random slope of repetition are highly correlated.
anova(Null_6s_3, Null_6s_4)
AICc(Null_6s_3, Null_6s_4)
# The model with the random quadratic slope for repetition is preferred!
Null_6s_5 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) + I(Rep_c^3)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c + I(Rep_c^2) | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
# Convergence issue
summary(Null_6s_5)
# The random quadratic slope does not explain much of the variance..
# Remove the quadratic slope for participants, fit the model
# and reassess the model compared to Null_6s_4
Null_6s_5 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) + I(Rep_c^3)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
# refit Null_6s_4 with the same random ef. structure to isolate fixed effects
Null_6s_4 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
anova(Null_6s_4, Null_6s_5)
AICc(Null_6s_4, Null_6s_5)
# A cubic effect for repetition improves the model compared to no cubic term for repetition 
# continue with Null_6s_5 as the null model..
Null_6s_6 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) + I(Rep_c^3) + I(Set_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
anova(Null_6s_5, Null_6s_6)
AICc(Null_6s_5, Null_6s_6)
# A quadratic effect for set does not improve the model
# continue with Null_6s_5 as the null model
Null_6s_7 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) * Set_c
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id) 
                  + (1 + Rep_c | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
anova(Null_6s_5, Null_6s_7)
AICc(Null_6s_5, Null_6s_7)
# Including a moderating effect of the quadratic and cubic effect of repetitions improves the model.
# Use Null_6s_7 from now on as the null model..
# Add a random quadratic slope for repetitions within sets within participants
Null_6s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) * Set_c
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id) 
                  + (1 + Rep_c + I(Rep_c^2) | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
anova(Null_6s_7, Null_6s_8)
AICc(Null_6s_7, Null_6s_8)
# A random quadratic slope for each set of participants significantly improves the model
# use Null_6s_8 as null model from now on
# Compare with the null model specification of intervention 3s..
Null_6s_9_3s <- lmer(twitch_torque_response ~ 
                       # Fixed effects
                       Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) + I(Set_c^2)
                     # Random effects
                     + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                     + (1 + Rep_c + I(Rep_c^2) | Set_c:Part_id)
                     , REML = FALSE, data = post_set_data_6s)
anova(Null_6s_8, Null_6s_9_3s)
AICc(Null_6s_8, Null_6s_9_3s)
# Null_6s_8 specification is better
# Keep Null_6s_8
anova(Null_6s_1, Null_6s_2, Null_6s_3, Null_6s_4, Null_6s_5, Null_6s_6, Null_6s_7, Null_6s_8, Null_6s_9_3s)
AICc(Null_6s_1, Null_6s_2, Null_6s_3, Null_6s_4, Null_6s_5, Null_6s_6, Null_6s_7, Null_6s_8, Null_6s_9_3s)
# The final Null_6s model is defined as the "Null_6s_8"
Null_6s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c + I(Rep_c^2) * Set_c + I(Rep_c^3) * Set_c
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id) 
                  + (1 + Rep_c + I(Rep_c^2) | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)



#### Part 1: Step 2. Visualizing null models ####
# Compute predictions to the data
data_preds <- data %>%
  mutate(
    fixed_pred = NA,
    full_pred = NA
  )
data_preds <- data_preds %>% rename(Intervention = intervention)
# Get 3s null model predictions
data_preds <- data_preds %>%
  mutate(
    fixed_pred = ifelse(Intervention == "3s",
                        predict(Null_3s_8, newdata = ., re.form = NA),
                        fixed_pred),
    full_pred = ifelse(Intervention == "3s",
                       predict(Null_3s_8, newdata = ., re.form = NULL),
                       full_pred)
  )

# Get 6s null model predictions
data_preds <- data_preds %>%
  mutate(
    fixed_pred = ifelse(Intervention == "6s",
                        predict(Null_6s_8, newdata = ., re.form = NA),
                        fixed_pred),
    full_pred = ifelse(Intervention == "6s",
                       predict(Null_6s_8, newdata = ., re.form = NULL),
                       full_pred)
  )

# Create plot
plot_combined <- ggplot(data_preds, aes(x = Rep, group = Part_id)) +
  geom_line(aes(y = twitch_torque_response, color = factor(Part_id)),
            size = 0.6, alpha = 0.6) +
  geom_point(aes(y = twitch_torque_response, color = factor(Part_id)),
             size = 1.2, alpha = 0.9) +
  # Random-effect predictions
  geom_line(aes(y = full_pred, color = factor(Part_id)),
            size = 0.9, alpha = 1, linetype = "dashed") +
  # Fixed-effect predictions
  geom_line(aes(y = fixed_pred),
            color = "black", size = 1.2, linetype = "dashed") +
  facet_grid(Intervention ~ Set, labeller = label_both,
             scales = "free_x", space = "free_x") +
  scale_x_continuous(breaks = 1:6) +
  labs(
    title = "Participant Twitch Torque Trajectories:\nObserved Data, Random Effects, and Fixed Effects",
    x = "Repetitions",
    y = "Twitch Torque Response (%)",
    color = "Part. ID"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold", hjust = 0),  # 👈 Left-align title
    plot.title.position = "plot",  # 👈 Align relative to plot area
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(size = 13),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.spacing.x = unit(0.7, "lines"),
    panel.spacing.y = unit(1.5, "lines")
  )
# Show null model plot
plot_combined

#### Part 1: Step 3. Visualizing baseline-PAP relationships and their interactions with Sets and Reps ####
#### ...Visualize baseline MVIC and intervention parameters ####
# Create baseline MVIC and intervention 3s plot
plot_mvic_3 <- ggplot(post_set_data_3s, aes(x = baseline_mvic, y = twitch_torque_response)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add linear fit
  facet_grid(Set ~ Rep, labeller = label_both) +           # Facet by Set and Rep
  theme_minimal() +
  theme(
    strip.text = element_text(size = 22),          # Size of facet labels
    strip.text.x = element_text(size = 22),        # Size of x facet labels (Set)
    strip.text.y = element_text(size = 22),        # Size of y facet labels (Intervention)
    plot.title = element_text(size = 22, face = "bold"),          # Plot title size
    axis.title.x = element_text(size = 22),                       # X axis title size
    axis.title.y = element_text(size = 22),                       # Y axis title size
    legend.title = element_text(size = 22),                        # Legend title size
    axis.text.y = element_text(size = 18),                # Y-axis tick label size
    axis.text.x = element_text(size = 18)  ) +
  labs(
    title = "Relationship between Baseline MVIC and TT Response (%) (3s)",
    x = "Baseline MVIC",
    y = "TT Response (%)") 
# Create Baseline MVIC and intervention 6s plot
plot_mvic_6 <- ggplot(post_set_data_6s, aes(x = baseline_mvic, y = twitch_torque_response)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add linear fit
  facet_grid(Set ~ Rep, labeller = label_both) +           # Facet by Set and Rep
  theme_minimal() +
  theme(
    strip.text = element_text(size = 22),          # Size of facet labels
    strip.text.x = element_text(size = 22),        # Size of x facet labels (Set)
    strip.text.y = element_text(size = 22),        # Size of y facet labels (Intervention)
    plot.title = element_text(size = 22, face = "bold"),          # Plot title size
    axis.title.x = element_text(size = 22),                       # X axis title size
    axis.title.y = element_text(size = 22),                       # Y axis title size
    legend.title = element_text(size = 22),                        # Legend title size
    axis.text.y = element_text(size = 18),                # Y-axis tick label size
    axis.text.x = element_text(size = 18)  ) +
  labs(
    title = "Relationship between Baseline MVIC and TT Response (%) (6s)",
    x = "Baseline MVIC",
    y = "TT Response (%)")
# Create baseline MVIC and combined intervention plot
plot_mvic <- plot_mvic_3 + plot_mvic_6 + 
  plot_annotation(title = "Visualization of MVIC and TT Response relationships",
                  theme = theme(plot.title = element_text(size = 24, face = "bold")))
# Visualize plot
plot_mvic


#### ...Visualize baseline TT and intervention parameters ####
# create Baseline TT and intervention 3s plot
plot_tt_3 <- ggplot(post_set_data_3s, aes(x = baseline_twitch_torque, y = twitch_torque_response)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add linear fit
  facet_grid(Set ~ Rep, labeller = label_both) +           # Facet by Set and Rep
  theme_minimal() +
  theme(
    strip.text = element_text(size = 22),          # Size of facet labels
    strip.text.x = element_text(size = 22),        # Size of x facet labels (Set)
    strip.text.y = element_text(size = 22),        # Size of y facet labels (Intervention)
    plot.title = element_text(size = 22, face = "bold"),          # Plot title size
    axis.title.x = element_text(size = 22),                       # X axis title size
    axis.title.y = element_text(size = 22),                       # Y axis title size
    legend.title = element_text(size = 22),                        # Legend title size
    axis.text.y = element_text(size = 22),                # Y-axis tick label size
    axis.text.x = element_text(size = 22)  ) +
  labs(
    title = "Relationship between Baseline TT and TT Response (%) (3s)",
    x = "Baseline TT",
    y = "TT Response (%)")
# create Baseline TT and intervention 6s plot
plot_tt_6 <- ggplot(post_set_data_6s, aes(x = baseline_twitch_torque, y = twitch_torque_response)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add linear fit
  facet_grid(Set ~ Rep, labeller = label_both) +           # Facet by Set and Rep
  theme_minimal() +
  theme(
    strip.text = element_text(size = 22),          # Size of facet labels
    strip.text.x = element_text(size = 22),        # Size of x facet labels (Set)
    strip.text.y = element_text(size = 22),        # Size of y facet labels (Intervention)
    plot.title = element_text(size = 22, face = "bold"),          # Plot title size
    axis.title.x = element_text(size = 22),                       # X axis title size
    axis.title.y = element_text(size = 22),                       # Y axis title size
    legend.title = element_text(size = 22),                        # Legend title size
    axis.text.y = element_text(size = 22),                # Y-axis tick label size
    axis.text.x = element_text(size = 22)  ) +
  labs(
    title = "Relationship between Baseline TT and TT Response (%) (6s)",
    x = "Baseline TT",
    y = "TT Response (%)"
  )
# create Baseline TT combined intervention plot
plot_tt <- plot_tt_3 + plot_tt_6 + 
  plot_annotation(title = "Visualization of TT and TT Response relationships",
                  theme = theme(plot.title = element_text(size = 24, face = "bold")))
# Visualize plot
plot_tt


#### ...Visualize baseline TT/MVIC and intervention parameters ####
# create Baseline TT/MVIC and intervention 3s plot
plot_tt_mvic_3 <- ggplot(post_set_data_3s, aes(x = baseline_tt_mvic_ratio, y = twitch_torque_response)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add linear fit
  facet_grid(Set ~ Rep, labeller = label_both) +           # Facet by Set and Rep
  theme_minimal() +
  theme(
    strip.text = element_text(size = 22),          # Size of facet labels
    strip.text.x = element_text(size = 22),        # Size of x facet labels (Set)
    strip.text.y = element_text(size = 22),        # Size of y facet labels (Intervention)
    plot.title = element_text(size = 22, face = "bold"),          # Plot title size
    axis.title.x = element_text(size = 22),                       # X axis title size
    axis.title.y = element_text(size = 22),                       # Y axis title size
    legend.title = element_text(size = 22),                        # Legend title size
    axis.text.y = element_text(size = 22),                # Y-axis tick label size
    axis.text.x = element_text(size = 22)  ) +
  labs(
    title = "Relationship between Baseline TT/MVIC and TT Response (%) (3s)",
    x = "Baseline TT/MVIC ratio",
    y = "TT Response (%)")
# Create Baseline TT/MVIC and intervention 6s plot
plot_tt_mvic_6 <- ggplot(post_set_data_6s, aes(x = baseline_tt_mvic_ratio, y = twitch_torque_response)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add linear fit
  facet_grid(Set ~ Rep, labeller = label_both) +           # Facet by Set and Rep
  theme_minimal() +
  theme(
    strip.text = element_text(size = 22),          # Size of facet labels
    strip.text.x = element_text(size = 22),        # Size of x facet labels (Set)
    strip.text.y = element_text(size = 22),        # Size of y facet labels (Intervention)
    plot.title = element_text(size = 22, face = "bold"),          # Plot title size
    axis.title.x = element_text(size = 22),                       # X axis title size
    axis.title.y = element_text(size = 22),                       # Y axis title size
    legend.title = element_text(size = 22),                        # Legend title size
    axis.text.y = element_text(size = 22),                # Y-axis tick label size
    axis.text.x = element_text(size = 22)  ) +
  labs(
    title = "Relationship between Baseline TT/MVIC and TT Response (%) (6s)",
    x = "Baseline TT/MVIC Ratio",
    y = "TT Response (%)")
# Create Baseline TT/MVIC combined intervention plot
plot_tt_mvic <- plot_tt_mvic_3 + plot_tt_mvic_6 + 
  plot_annotation(title = "Visualization of TT/MVIC and TT Response relationships",
                  theme = theme(plot.title = element_text(size = 24, face = "bold")))
# Visualize plot
plot_tt_mvic


#### Part 1: Step 4. Comparing baseline torque models with Null models ####
# Note: Models here were fitted with simplified random effects (I(Rep_c^2) as uncorrelated), because the inclusion of baseline variables
# led to convergence issues or unstable random effect correlations
#### ...Fitting the models ####
# Null 3s model
Null_3s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c 
                  + I(Rep_c^2) * Set_c 
                  + I(Rep_c^3) 
                  + I(Set_c^2)
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                  + (1 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_3s)
# Baseline 3s models
base_mvic_3s <- lmer(twitch_torque_response ~ 
                       # Fixed effects
                       baseline_mvic_c +
                       Rep_c * Set_c 
                     + I(Rep_c^2) * Set_c 
                     + I(Rep_c^3) 
                     + I(Set_c^2)
                     # Random effects
                     + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                     + (1 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                     , REML = FALSE, data = post_set_data_3s)
base_tt_3s <- lmer(twitch_torque_response ~ 
                     # Fixed effects
                     baseline_twitch_torque_c * Rep_c * Set_c
                   + baseline_twitch_torque_c * I(Rep_c^2) * Set_c 
                   + I(Rep_c^3) 
                   + I(Set_c^2)
                   # Random effects
                   + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                   + (1 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                   , REML = FALSE, data = post_set_data_3s)
base_tt_mvic_3s <- lmer(twitch_torque_response ~ 
                          # Fixed effects
                          baseline_tt_mvic_ratio_c * Rep_c * Set_c
                        + baseline_tt_mvic_ratio_c * I(Rep_c^2) * Set_c 
                        + I(Rep_c^3) 
                        + I(Set_c^2)
                        # Random effects
                        + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                        + (1 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                        , REML = FALSE, data = post_set_data_3s)
# Null 6s model
Null_6s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c 
                  + I(Rep_c^2) * Set_c 
                  + I(Rep_c^3) * Set_c
                  # Random effects
                  + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                  + (1 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                  , REML = FALSE, data = post_set_data_6s)
# Baseline 6s models
base_mvic_6s <- lmer(twitch_torque_response ~ 
                       # Fixed effects
                       baseline_mvic_c
                     + Rep_c * Set_c 
                     + I(Rep_c^2) * Set_c 
                     + I(Rep_c^3) * Set_c
                     # Random effects
                     + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                     + (1 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                     , REML = FALSE, data = post_set_data_6s)
base_tt_6s <- lmer(twitch_torque_response ~ 
                     # Fixed effects
                     baseline_twitch_torque_c * Rep_c * Set_c 
                   + baseline_twitch_torque_c * I(Rep_c^2) * Set_c 
                   + I(Rep_c^3) * Set_c
                   # Random effects
                   + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                   + (1 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                   , REML = FALSE, data = post_set_data_6s)
base_tt_mvic_6s <- lmer(twitch_torque_response ~ 
                          # Fixed effects
                          baseline_tt_mvic_ratio_c * Rep_c * Set_c 
                        + baseline_tt_mvic_ratio_c * I(Rep_c^2) * Set_c 
                        + I(Rep_c^3) * Set_c
                        # Random effects
                        + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                        + (1 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                        , REML = FALSE, data = post_set_data_6s)

#### ...Run function to extract model comparison statistics and model estimates ####
# Specify the function
summarize_all_models <- function(
    all_model_list, all_model_names,
    all_null_model_list, all_null_model_names,
    rename_vars = NULL  # argument for renaming
) {
  # fixed effects
  extract_fixed_effects <- function(model, model_name) {
    sum_model <- summary(model)
    coefs <- as.data.frame(fixef(model)) %>%
      tibble::rownames_to_column("variable") %>%
      rename(estimate_num = `fixef(model)`) %>%
      mutate(model = model_name)
    if (!is.null(sum_model$coefficients)) {
      se <- sum_model$coefficients[, "Std. Error"]
      pvals <- sum_model$coefficients[, "Pr(>|t|)"]
      coefs <- coefs %>%
        mutate(
          se = se[variable],
          p_value_num = pvals[variable],
          p_value = ifelse(p_value_num < 0.001, "<0.001", sprintf("%.3f", p_value_num))
        )
    } else {
      coefs <- coefs %>%
        mutate(se = NA_real_, p_value_num = NA_real_, p_value = NA_character_)
    }
    coefs <- coefs %>%
      mutate(
        stars = case_when(
          is.na(p_value_num) ~ "",
          p_value_num < 0.001 ~ "***",
          p_value_num < 0.01 ~ "**",
          p_value_num < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
    coefs <- coefs %>%
      mutate(
        estimate_rounded = round(estimate_num, 3),
        estimate = paste0(
          estimate_rounded, stars, "\n",
          "se: ", round(se, 3), "\n",
          "pval: ", p_value
        )
      ) %>%
      mutate(variable = case_when(
        str_detect(variable, "^baseline_mvic") ~ str_replace(variable, "^baseline_mvic_c", "baseline"),
        str_detect(variable, "^baseline_twitch_torque") ~ str_replace(variable, "^baseline_twitch_torque_c", "baseline"),
        str_detect(variable, "^baseline_tt_mvic_ratio") ~ str_replace(variable, "^baseline_tt_mvic_ratio_c", "baseline"),
        TRUE ~ variable
      )) %>%
      select(variable, estimate, model)
    
    return(coefs)
  }
  
  # Model comparisons
  extract_model_metrics <- function(model, model_name, null_model, null_name) {
    aicc_value <- round(AICc(model), 3)
    r2 <- as.data.frame(r.squaredGLMM(model))
    colnames(r2) <- c("marginal_r2", "conditional_r2")
    marginal_r2 <- round(r2$marginal_r2[1], 3)
    conditional_r2 <- round(r2$conditional_r2[1], 3)
    if (model_name != null_name) {
      lrt <- anova(null_model, model)
      p_value <- lrt$`Pr(>Chisq)`[2]
      df_diff <- lrt$Df[2]
      if (p_value < 0.001) {
        lrt_string <- paste0("<0.001*** (", df_diff, ")")
      } else if (p_value < 0.01) {
        lrt_string <- paste0(round(p_value, 3), "** (", df_diff, ")")
      } else if (p_value < 0.05) {
        lrt_string <- paste0(round(p_value, 3), "* (", df_diff, ")")
      } else {
        lrt_string <- paste0(round(p_value, 3), " (", df_diff, ")")
      }
    } else {
      lrt_string <- NA_character_
    }
    
    # Random effects
    # Refit models with REML
    refit_with_reml <- function(model) {
      update(model, REML = TRUE)
    }
    model_reml <- refit_with_reml(model)
    extract_correlations <- function(model_reml) {
      vc <- VarCorr(model_reml)
      cor_list <- list()
      for (grp in names(vc)) {
        mat <- as.matrix(vc[[grp]])
        if (ncol(mat) > 1) {
          cor_mat <- cov2cor(mat)
          cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA
          cor_df <- as.data.frame(as.table(cor_mat))
          cor_df <- cor_df[!is.na(cor_df$Freq), ]
          cor_df <- cor_df %>%
            mutate(
              cor_label = paste0("Corr_", grp, "_", Var1, "_", Var2),
              correlation = round(Freq, 3)
            ) %>%
            select(cor_label, correlation)
          cor_list[[grp]] <- cor_df
        }
      }
      if (length(cor_list) > 0) {
        return(dplyr::bind_rows(cor_list))
      } else {
        return(tibble(cor_label = character(), correlation = numeric()))
      }
    }
    varcomp_raw <- as.data.frame(VarCorr(model_reml)) %>%
      filter(is.na(var2)) %>%
      mutate(
        effect_name = ifelse(is.na(var1), "(Intercept)", var1),
        level = as.character(grp)
      )
    total_var <- sum(varcomp_raw$vcov)
    varcomp <- varcomp_raw %>%
      mutate(VPC = round(vcov / total_var, 3)) %>%
      select(level, effect_name, VPC)
    corrs_raw <- extract_correlations(model_reml)
    
    # Create dataframe
    metrics_df <- tibble(
      variable = c(
        "AICc", "LRT vs Null", "marginal_r2", "conditional_r2",
        paste0("VPC_", varcomp$level, "_", varcomp$effect_name),
        if (nrow(corrs_raw) > 0) corrs_raw$cor_label,
        "Total Variance"
      ),
      estimate = as.character(c(
        aicc_value,
        lrt_string,
        marginal_r2,
        conditional_r2,
        varcomp$VPC,
        if (nrow(corrs_raw) > 0) corrs_raw$correlation,
        round(total_var, 3)
      )),
      model = model_name
    )
    return(metrics_df)
  }
  fixed_list <- purrr::map2(all_model_list, all_model_names, extract_fixed_effects)
  null_models_aligned <- purrr::map2(all_model_names, all_null_model_names, function(m_name, null_name) {
    if (str_detect(m_name, "Null 3s") | str_detect(m_name, "MVIC 3s") | str_detect(m_name, "TT 3s") | str_detect(m_name, "TT/MVIC 3s")) {
      return(Null_3s_8)
    } else if (str_detect(m_name, "Null 6s") | str_detect(m_name, "MVIC 6s") | str_detect(m_name, "TT 6s") | str_detect(m_name, "TT/MVIC 6s")) {
      return(Null_6s_8)
    } else {
      return(NULL)
    }
  })
  metrics_list <- purrr::pmap(list(all_model_list, all_model_names, null_models_aligned, all_null_model_names), extract_model_metrics)
  all_fixed <- bind_rows(fixed_list)
  all_metrics <- bind_rows(metrics_list)
  full_df <- bind_rows(all_fixed, all_metrics) %>%
    group_by(model, variable) %>%
    summarise(estimate = dplyr::first(estimate), .groups = "drop") %>%
    pivot_wider(names_from = model, values_from = estimate)
  intercept_row <- "(Intercept)"
  baseline_rows <- full_df$variable[str_starts(full_df$variable, "baseline")]
  metric_rows <- c("AICc", "LRT vs Null", "marginal_r2", "conditional_r2")
  varcomp_rows <- full_df$variable[str_starts(full_df$variable, "VPC_") | full_df$variable == "Total Variance"]
  corr_rows <- full_df$variable[str_starts(full_df$variable, "Corr_")]
  other_fixed <- setdiff(
    full_df$variable,
    c(intercept_row, baseline_rows, metric_rows, varcomp_rows, corr_rows)
  )
  final_order <- c(intercept_row, baseline_rows, other_fixed, metric_rows, varcomp_rows, corr_rows)
  full_df <- full_df %>%
    arrange(factor(variable, levels = final_order))
  desired_column_order <- c("variable", "Null 3s", "MVIC 3s", "TT 3s", "TT/MVIC 3s",
                            "Null 6s", "MVIC 6s", "TT 6s", "TT/MVIC 6s")
  full_df <- full_df %>%
    select(any_of(desired_column_order))
  if (!is.null(rename_vars) && length(rename_vars) > 0) {
    full_df <- full_df %>%
      mutate(variable = ifelse(variable %in% names(rename_vars),
                               rename_vars[variable],
                               variable))
  }
  full_df <- full_df %>%
    dplyr::mutate(dplyr::across(everything(), ~ ifelse(is.na(.), "-", .)))
  return(full_df)
}
# Specify the models
model_list_3s <- list(Null_3s_8, base_mvic_3s, base_tt_3s, base_tt_mvic_3s)
model_names_3s <- c("Null 3s", "MVIC 3s", "TT 3s", "TT/MVIC 3s")
null_model_list_3s <- list(Null_3s_8, Null_3s_8, Null_3s_8, Null_3s_8)
null_model_names_3s <- c("Null 3s", "Null 3s", "Null 3s", "Null 3s")
model_list_6s <- list(Null_6s_8, base_mvic_6s, base_tt_6s, base_tt_mvic_6s)
model_names_6s <- c("Null 6s", "MVIC 6s", "TT 6s", "TT/MVIC 6s")
null_model_list_6s <- list(Null_6s_8, Null_6s_8, Null_6s_8, Null_6s_8)
null_model_names_6s <- c("Null 6s", "Null 6s", "Null 6s", "Null 6s")
# Specify 3s and 6s models
all_model_list <- c(model_list_3s, model_list_6s)
all_model_names <- c(model_names_3s, model_names_6s)
all_null_model_list <- c(null_model_list_3s, null_model_list_6s)
all_null_model_names <- c(null_model_names_3s, null_model_names_6s)
# Run the function to extract statistics
summary_table <- summarize_all_models(
  all_model_list, all_model_names,
  all_null_model_list, all_null_model_names,
  rename_vars = c("marginal_r2" = "Marginal R2",
                  "conditional_r2" = "Conditional R2",
                  "VPC_Part_id.1_(Intercept)" = "VPC Part-lvl Intercept",
                  "VPC_Part_id.1_Set_c" = "VPC Part-lvl Slope Set",
                  "VPC_Part_id_Rep_c" = "VPC Part-lvl Slope Rep",
                  "VPC_Set_c.Part_id.1_(Intercept)" = "VPC Part-Set Lvl Intercept",
                  "VPC_Set_c.Part_id.1_Rep_c" = "VPC Part-Set Lvl Slope Rep",
                  "VPC_Set_c.Part_id_I(Rep_c^2)" = "VPC Part-Set Lvl Slope Quad.Rep",
                  "VPC_Residual_(Intercept)" = "VPC Residual",
                  "Corr_Part_id.1_(Intercept)_Set_c" = "Part-lvl Corr.Betw. Intercept&Set slope",
                  "Corr_Set_c.Part_id.1_(Intercept)_Rep_c" = "Part-Set lvl Corr.Betw. Intercept&Rep slope")
)
# View the results of the model parameter estimates and model comparison statistics - summary table
print(summary_table, n = 29)
#### ...Save model estimates and model comparison statistics in summary table ####
write_xlsx(summary_table, path = "ADD PATH TO THE PLACE YOU WANT TO SAVE THE SUMMARY TABLE")


#### Part 1: Step 5. Case bootstrapping model estimates and confidence intervals ####
#### ...Re-fitting the models for bootstraps ####
# Note: Models here were fitted with uncorrelated random effects between:
# - Random Part Intercept & Random Part. Slope for Sets
# - Random Intra-Part. Intercept for Sets and Random slope for reps
# This was done because initial bootstrap procedures resulted in high convergence issues (~20%), the simplification
# of the random effects highly reduced these issues, suggesting random effects were likely too complicated for the 
# case bootstrapped samples, which was expected as a result from the small sample size at the participant level..
# Re-fitting models with REML to get less biased bootstrap estimates of random effects
# Null 3s model
Null_3s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c 
                  + I(Rep_c^2) * Set_c 
                  + I(Rep_c^3) 
                  + I(Set_c^2)
                  # Random effects
                  + (1 | Part_id) + (0 + Rep_c | Part_id) + (0 + Set_c | Part_id)
                  + (1 | Set_c:Part_id) + (0 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                  , REML = TRUE, data = post_set_data_3s)
# Baseline 3s models
base_mvic_3s <- lmer(twitch_torque_response ~ 
                       # Fixed effects
                       baseline_mvic_c +
                       Rep_c * Set_c 
                     + I(Rep_c^2) * Set_c 
                     + I(Rep_c^3) 
                     + I(Set_c^2)
                     # Random effects
                     + (1 | Part_id) + (0 + Rep_c | Part_id) + (0 + Set_c | Part_id)
                     + (1 | Set_c:Part_id) + (0 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                     , REML = TRUE, data = post_set_data_3s)
base_tt_3s <- lmer(twitch_torque_response ~ 
                     # Fixed effects
                     baseline_twitch_torque_c * Rep_c * Set_c
                   + baseline_twitch_torque_c * I(Rep_c^2) * Set_c 
                   + I(Rep_c^3) 
                   + I(Set_c^2)
                   # Random effects
                   + (1 | Part_id) + (0 + Rep_c | Part_id) + (0 + Set_c | Part_id)
                   + (1 | Set_c:Part_id) + (0 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                   , REML = TRUE, data = post_set_data_3s)
base_tt_mvic_3s <- lmer(twitch_torque_response ~ 
                          # Fixed effects
                          baseline_tt_mvic_ratio_c * Rep_c * Set_c
                        + baseline_tt_mvic_ratio_c * I(Rep_c^2) * Set_c 
                        + I(Rep_c^3) 
                        + I(Set_c^2)
                        # Random effects
                        + (1 | Part_id) + (0 + Rep_c | Part_id) + (0 + Set_c | Part_id)
                        + (1 | Set_c:Part_id) + (0 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                        , REML = TRUE, data = post_set_data_3s)
# Null 6s model
Null_6s_8 <- lmer(twitch_torque_response ~ 
                    # Fixed effects
                    Rep_c * Set_c 
                  + I(Rep_c^2) * Set_c 
                  + I(Rep_c^3) * Set_c
                  # Random effects
                  + (1 | Part_id) + (0 + Rep_c | Part_id) + (0 + Set_c | Part_id)
                  + (1 | Set_c:Part_id) + (0 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                  , REML = TRUE, data = post_set_data_6s)
# Baseline 6s models
base_mvic_6s <- lmer(twitch_torque_response ~ 
                       # Fixed effects
                       baseline_mvic_c
                     + Rep_c * Set_c 
                     + I(Rep_c^2) * Set_c 
                     + I(Rep_c^3) * Set_c
                     # Random effects
                     + (1 | Part_id) + (0 + Rep_c | Part_id) + (0 + Set_c | Part_id)
                     + (1 | Set_c:Part_id) + (0 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                     , REML = TRUE, data = post_set_data_6s)
base_tt_6s <- lmer(twitch_torque_response ~ 
                     # Fixed effects
                     baseline_twitch_torque_c * Rep_c * Set_c 
                   + baseline_twitch_torque_c * I(Rep_c^2) * Set_c 
                   + I(Rep_c^3) * Set_c
                   # Random effects
                   + (1 | Part_id) + (0 + Rep_c | Part_id) + (0 + Set_c | Part_id)
                   + (1 | Set_c:Part_id) + (0 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                   , REML = TRUE, data = post_set_data_6s)
base_tt_mvic_6s <- lmer(twitch_torque_response ~ 
                          # Fixed effects
                          baseline_tt_mvic_ratio_c * Rep_c * Set_c 
                        + baseline_tt_mvic_ratio_c * I(Rep_c^2) * Set_c 
                        + I(Rep_c^3) * Set_c
                        # Random effects
                        + (1 | Part_id) + (0 + Rep_c | Part_id) + (0 + Set_c | Part_id)
                        + (1 | Set_c:Part_id) + (0 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                        , REML = TRUE, data = post_set_data_6s)
# Set seed for reproducibility of bootstraps
set.seed(123)
# Specify the models to do bootstraps on
models <- list(Null_3s_8, base_mvic_3s, base_tt_3s, base_tt_mvic_3s,
               Null_6s_8, base_mvic_6s, base_tt_6s, base_tt_mvic_6s)
# Specify a function to extract the parameter estimates of interest after every re-sample and refit
mySumm <- function(.) {
  s <- getME(., "sigma") 
  c(beta = getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta")))}
# Start a list to store bootstrap results in
bootstrap_results <- list()
# Run case bootstraps on the models specified using a loop
for (i in 1:length(models)) {
  # In the loop, run the bootstrap for each model, and store the results in the list
  # Set re-sampling scheme to re-sample only the participant level
  bootstrap_results[[i]] <- 
    case_bootstrap(models[[i]], .f = mySumm, resample = c(TRUE, FALSE, FALSE), .refit = TRUE, B = 10000)
  cat("Finished bootstrap for model", i, "\n")}
# Extract bootstrap results for each model
case_null_3s <- bootstrap_results[[1]]
case_mvic_3s <- bootstrap_results[[2]]
case_tt_3s <- bootstrap_results[[3]]
case_tt_mvic_3s <- bootstrap_results[[4]]
case_null_6s <- bootstrap_results[[5]]
case_mvic_6s <- bootstrap_results[[6]]
case_tt_6s <- bootstrap_results[[7]]
case_tt_mvic_6s <- bootstrap_results[[8]]
# Bootstrap model fitting diagnostics of warnings
analyze_model_issues <- function(data) {
  data_name <- deparse(substitute(data))
  singular_indices <- which(sapply(data$message, function(x) identical(x, "boundary (singular) fit: see help('isSingular')\n")))
  convergence_indices <- which(sapply(data$warning, function(x) is.character(x) && grepl("Model failed to converge", x[1])))
  cat("The", data_name, "produced", length(singular_indices), "models with singular fit messages and", length(convergence_indices), "models with convergence warnings.\n")
}
analyze_model_issues(case_null_3s)
analyze_model_issues(case_null_6s)
analyze_model_issues(case_mvic_3s)
analyze_model_issues(case_mvic_6s)
analyze_model_issues(case_tt_3s)
analyze_model_issues(case_tt_6s)
analyze_model_issues(case_tt_mvic_3s)
analyze_model_issues(case_tt_mvic_6s)
# Specify function to filter non-converged models
filter_nonconverged_replicates <- function(bootstrap_result, model_name = "") {
  nonconverged <- sapply(bootstrap_result$warning, function(x) 
    is.character(x) && grepl("Model failed to converge", x[1]))
  keep_indices <- which(!nonconverged)
  n_original <- nrow(bootstrap_result$replicates)
  n_kept <- length(keep_indices)
  n_removed <- n_original - n_kept
  cat(sprintf("Model %s: %d original, %d non-converged removed, %d kept.\n", 
              model_name, n_original, n_removed, n_kept))
  bootstrap_result$replicates <- bootstrap_result$replicates[keep_indices, , drop = FALSE]
  bootstrap_result$warning <- bootstrap_result$warning[keep_indices]
  bootstrap_result$message <- bootstrap_result$message[keep_indices]
  return(bootstrap_result)}
# Filter non-converged models
case_null_3s <- filter_nonconverged_replicates(case_null_3s, "Null 3s")
case_null_6s <- filter_nonconverged_replicates(case_null_6s, "Null 6s")
case_mvic_3s <- filter_nonconverged_replicates(case_mvic_3s, "MVIC 3s")
case_mvic_6s <- filter_nonconverged_replicates(case_mvic_6s, "MVIC 6s")
case_tt_3s <- filter_nonconverged_replicates(case_tt_3s, "TT 3s")
case_tt_6s <- filter_nonconverged_replicates(case_tt_6s, "TT 6s")
case_tt_mvic_3s <- filter_nonconverged_replicates(case_tt_mvic_3s, "TT MVIC 3s")
case_tt_mvic_6s <- filter_nonconverged_replicates(case_tt_mvic_6s, "TT MVIC 6s")
# bootstrapped estimates
stats_null_3s <- as.data.frame(case_null_3s$replicates)
stats_mvic_3s <- as.data.frame(case_mvic_3s$replicates)
stats_tt_3s <- as.data.frame(case_tt_3s$replicates)
stats_tt_mvic_3s <- as.data.frame(case_tt_mvic_3s$replicates)
stats_null_6s <- as.data.frame(case_null_6s$replicates)
stats_mvic_6s <- as.data.frame(case_mvic_6s$replicates)
stats_tt_6s <- as.data.frame(case_tt_6s$replicates)
stats_tt_mvic_6s <- as.data.frame(case_tt_mvic_6s$replicates)
# Specify function to Rename coefficients to their actual names..
rename_stats_columns <- function(stats_df, model) {
  beta_names <- names(fixef(model))
  colnames(stats_df)[seq_along(beta_names)] <- beta_names
  varcorr_df <- as.data.frame(VarCorr(model))
  rand_names <- ifelse(varcorr_df$grp == "Residual",
                       "Residual",
                       paste0(varcorr_df$grp, "_", varcorr_df$var1))
  sigma_cols <- grep("^sig0\\d{2,}", colnames(stats_df), value = TRUE)
  rand_effects <- rand_names[varcorr_df$grp != "Residual"]
  if (length(sigma_cols) != length(rand_effects)) {
    warning("Number of sigma columns and random effects do not match!")
  }
  for (i in seq_along(sigma_cols)) {
    colnames(stats_df)[colnames(stats_df) == sigma_cols[i]] <- rand_effects[i]
  }
  if ("sigma" %in% colnames(stats_df)) {
    colnames(stats_df)[colnames(stats_df) == "sigma"] <- "Residual"
  }
  return(stats_df)
}
# Rename columns
stats_null_3s_renamed_v <- rename_stats_columns(stats_null_3s, Null_3s_8)
stats_mvic_3s_renamed_v <- rename_stats_columns(stats_mvic_3s, base_mvic_3s)
stats_tt_3s_renamed_v <- rename_stats_columns(stats_tt_3s, base_tt_3s)
stats_tt_mvic_3s_renamed_v <- rename_stats_columns(stats_tt_mvic_3s, base_tt_mvic_3s)
stats_null_6s_renamed_v <- rename_stats_columns(stats_null_6s, Null_6s_8)
stats_mvic_6s_renamed_v <- rename_stats_columns(stats_mvic_6s, base_mvic_6s)
stats_tt_6s_renamed_v <- rename_stats_columns(stats_tt_6s, base_tt_6s)
stats_tt_mvic_6s_renamed_v <- rename_stats_columns(stats_tt_mvic_6s, base_tt_mvic_6s)
# Compute bootstrapped VPCs
compute_vpc <- function(df) {
  variance_cols <- grep("Part_id|Residual", names(df), value = TRUE)
  sqrd_df <- df[variance_cols]^2
  df$total_variance <- rowSums(sqrd_df, na.rm = TRUE)
  # Compute VPC for each component
  for (col in variance_cols) {
    vpc_col_name <- paste0("VPC_", col)
    df[[vpc_col_name]] <- sqrd_df[[col]] / df$total_variance
  }
  return(df)
}
stats_null_3s_renamed <- compute_vpc(stats_null_3s_renamed_v)
stats_mvic_3s_renamed <- compute_vpc(stats_mvic_3s_renamed_v)
stats_tt_3s_renamed <- compute_vpc(stats_tt_3s_renamed_v)
stats_tt_mvic_3s_renamed <- compute_vpc(stats_tt_mvic_3s_renamed_v)
stats_null_6s_renamed <- compute_vpc(stats_null_6s_renamed_v)
stats_mvic_6s_renamed <- compute_vpc(stats_mvic_6s_renamed_v)
stats_tt_6s_renamed <- compute_vpc(stats_tt_6s_renamed_v)
stats_tt_mvic_6s_renamed <- compute_vpc(stats_tt_mvic_6s_renamed_v)
#### ...save bootstrapped datasets ####
write_xlsx(stats_null_3s_renamed, path = "SPECIFY PATH TO STORE BOOTSTRAPPED DATASET FOR THIS MODEL")
write_xlsx(stats_mvic_3s_renamed, path = "SPECIFY PATH TO STORE BOOTSTRAPPED DATASET FOR THIS MODEL")
write_xlsx(stats_tt_3s_renamed, path = "SPECIFY PATH TO STORE BOOTSTRAPPED DATASET FOR THIS MODEL")
write_xlsx(stats_tt_mvic_3s_renamed, path = "SPECIFY PATH TO STORE BOOTSTRAPPED DATASET FOR THIS MODEL")
write_xlsx(stats_null_6s_renamed, path = "SPECIFY PATH TO STORE BOOTSTRAPPED DATASET FOR THIS MODEL")
write_xlsx(stats_mvic_6s_renamed, path = "SPECIFY PATH TO STORE BOOTSTRAPPED DATASET FOR THIS MODEL")
write_xlsx(stats_tt_6s_renamed, path = "SPECIFY PATH TO STORE BOOTSTRAPPED DATASET FOR THIS MODEL")
write_xlsx(stats_tt_mvic_6s_renamed, path = "SPECIFY PATH TO STORE BOOTSTRAPPED DATASET FOR THIS MODEL")


# Summary table for bootstrap coefficients and confidence intervals
summarize_and_collapse <- function(
    stats_null_3s, stats_mvic_3s, stats_tt_3s, stats_tt_mvic_3s,
    stats_null_6s, stats_mvic_6s, stats_tt_6s, stats_tt_mvic_6s,
    rename_vars = NULL  # argument for renaming
) {
  summarize_df <- function(df) {
    sapply(df, function(col) {
      if (is.numeric(col)) {
        m <- mean(col, na.rm = TRUE)
        med <- median(col, na.rm = TRUE)
        lower <- quantile(col, probs = 0.025, na.rm = TRUE)
        upper <- quantile(col, probs = 0.975, na.rm = TRUE)
        sprintf("Mean: %.2f\nMedian: %.2f\n(%.2f:%.2f)", m, med, lower, upper)
      } else {
        NA_character_
      }
    })
  }
  sum_null_3s <- summarize_df(stats_null_3s)
  sum_mvic_3s <- summarize_df(stats_mvic_3s)
  sum_tt_3s <- summarize_df(stats_tt_3s)
  sum_tt_mvic_3s <- summarize_df(stats_tt_mvic_3s)
  sum_null_6s <- summarize_df(stats_null_6s)
  sum_mvic_6s <- summarize_df(stats_mvic_6s)
  sum_tt_6s <- summarize_df(stats_tt_6s)
  sum_tt_mvic_6s <- summarize_df(stats_tt_mvic_6s)
  all_vars <- unique(c(
    names(sum_null_3s), names(sum_mvic_3s), names(sum_tt_3s), names(sum_tt_mvic_3s),
    names(sum_null_6s), names(sum_mvic_6s), names(sum_tt_6s), names(sum_tt_mvic_6s)
  ))
  summary_table <- data.frame(
    Variable = all_vars,
    `Null 3s` = sum_null_3s[all_vars],
    `MVIC 3s` = sum_mvic_3s[all_vars],
    `TT 3s` = sum_tt_3s[all_vars],
    `TT/MVIC 3s` = sum_tt_mvic_3s[all_vars],
    `Null 6s` = sum_null_6s[all_vars],
    `MVIC 6s` = sum_mvic_6s[all_vars],
    `TT 6s` = sum_tt_6s[all_vars],
    `TT/MVIC 6s` = sum_tt_mvic_6s[all_vars],
    stringsAsFactors = FALSE
  )
  summary_table$Variable <- gsub("^baseline_mvic_c", "baseline", summary_table$Variable)
  summary_table$Variable <- gsub("^baseline_twitch_torque_c", "baseline", summary_table$Variable)
  summary_table$Variable <- gsub("^baseline_tt_mvic_ratio_c", "baseline", summary_table$Variable)
  summary_table <- summary_table %>%
    group_by(Variable) %>%
    summarise(across(everything(), ~ {
      vals <- unique(na.omit(.x))
      if (length(vals) == 0) {
        "-"
      } else if (length(vals) == 1) {
        vals
      } else {
        paste(vals, collapse = " | ")
      }
    }), .groups = "drop")
  summary_table <- summary_table %>%
    mutate(order_group = case_when(
      Variable == "(Intercept)" ~ 1,
      grepl("^baseline", Variable) ~ 2,
      grepl("Part_id", Variable) ~ 4,
      grepl("Residual", Variable) ~ 5,
      TRUE ~ 3
    )) %>%
    arrange(order_group, Variable) %>%
    select(-order_group)
  # Rename variables if rename_vars is provided 
  if (!is.null(rename_vars) && length(rename_vars) > 0) {
    summary_table <- summary_table %>%
      mutate(Variable = ifelse(Variable %in% names(rename_vars),
                               rename_vars[Variable],
                               Variable))
  }
  return(summary_table)
}
# Summarize bootstrap estimates into a table
bootstrap_summary <- summarize_and_collapse(
  stats_null_3s_renamed, stats_mvic_3s_renamed, stats_tt_3s_renamed, stats_tt_mvic_3s_renamed,
  stats_null_6s_renamed, stats_mvic_6s_renamed, stats_tt_6s_renamed, stats_tt_mvic_6s_renamed,
  rename_vars = c("Part_id.2_(Intercept)" = "Part-lvl Intercept (SD)",
                  "Part_id_Set_c" = "Part-lvl Slope Set (SD)",
                  "Part_id.1_Rep_c" = "Part-lvl Slope Rep (SD)",
                  "Set_c.Part_id.2_(Intercept)" = "Part-Set Lvl Intercept (SD)",
                  "Set_c.Part_id.1_Rep_c" = "Part-Set Lvl Slope Rep (SD)",
                  "Set_c.Part_id_I(Rep_c^2)" = "Part-Set Lvl Slope Quad.Rep (SD)",
                  "Residual" = "Residual (SD)",
                  "VPC_Part_id.2_(Intercept)" = "VPC Part-lvl Intercept",
                  "VPC_Part_id_Set_c" = "VPC Part-lvl Slope Set",
                  "VPC_Part_id.1_Rep_c" = "VPC Part-lvl Slope Rep",
                  "VPC_Set_c.Part_id.2_(Intercept)" = "VPC Part-Set Lvl Intercept",
                  "VPC_Set_c.Part_id.1_Rep_c" = "VPC Part-Set Lvl Slope Rep",
                  "VPC_Set_c.Part_id_I(Rep_c^2)" = "VPC Part-Set Lvl Slope Quad.Rep",
                  "VPC_Residual" = "VPC Residual",
                  "total_variance" = "Total Variance"
  )
)
# view bootstrapped estimates
print(bootstrap_summary, n = 30)
#### ...Save independent intervention analysis bootstrapped model estimate summary table ####
write_xlsx(bootstrap_summary, path = "SPECIFY PATH TO STORE BOOTSTRAP MODEL ESTIMATE SUMMARY")

#### Part 1: Step 6. Visualizing random effects by model ####
#### ...Plot box plots of model estimated VPCs and their confidence intervals ####
# requires the created bootstrap model datasets and the model estimate summary table, to be loaded..
# Rename the variables in the boot datasets to match the names in the summary table
rename_variables <- function(data, rename_vars) {
  data %>%
    dplyr::rename(!!!rename_vars)
}
rename_vars <- c(
  "Part-lvl Intercept (SD)" = "Part_id.2_(Intercept)",
  "Part-lvl Slope Set (SD)" = "Part_id_Set_c",
  "Part-lvl Slope Rep (SD)" = "Part_id.1_Rep_c",
  "Part-Set Lvl Intercept (SD)" = "Set_c.Part_id.2_(Intercept)",
  "Part-Set Lvl Slope Rep (SD)" = "Set_c.Part_id.1_Rep_c",
  "Part-Set Lvl Slope Quad.Rep (SD)" = "Set_c.Part_id_I(Rep_c^2)",
  "VPC Part-lvl Intercept" = "VPC_Part_id.2_(Intercept)",
  "VPC Part-lvl Slope Set" = "VPC_Part_id_Set_c",
  "VPC Part-lvl Slope Rep" = "VPC_Part_id.1_Rep_c",
  "VPC Part-Set Lvl Intercept" = "VPC_Set_c.Part_id.2_(Intercept)",
  "VPC Part-Set Lvl Slope Rep" = "VPC_Set_c.Part_id.1_Rep_c",
  "VPC Part-Set Lvl Slope Quad.Rep" = "VPC_Set_c.Part_id_I(Rep_c^2)",
  "VPC Residual" = "VPC_Residual",
  "Total Variance" = "total_variance",
  "Residual (SD)" = "Residual"
)

# Replace stats_"model"_renamed with the actual names of bootstrapped datasets for each model
stats_null_3s_renamed <- rename_variables(stats_null_3s_renamed, rename_vars)
stats_mvic_3s_renamed <- rename_variables(stats_mvic_3s_renamed, rename_vars)
stats_tt_3s_renamed <- rename_variables(stats_tt_3s_renamed, rename_vars)
stats_tt_mvic_3s_renamed <- rename_variables(stats_tt_mvic_3s_renamed, rename_vars)
stats_null_6s_renamed <- rename_variables(stats_null_6s_renamed, rename_vars)
stats_mvic_6s_renamed <- rename_variables(stats_mvic_6s_renamed, rename_vars)
stats_tt_6s_renamed <- rename_variables(stats_tt_6s_renamed, rename_vars)
stats_tt_mvic_6s_renamed <- rename_variables(stats_tt_mvic_6s_renamed, rename_vars)

# GET VPC PLOTS
# Specify model names in summary table and variance variables to plot
model_names_3s <- c("Null 3s", "MVIC 3s", "TT 3s", "TT/MVIC 3s")
model_names_6s <- c("Null 6s", "MVIC 6s", "TT 6s", "TT/MVIC 6s")
variance_vars_to_plot <- c(
  "VPC Residual",
  "Total Variance",
  "VPC Part-lvl Intercept",
  "VPC Part-lvl Slope Set",
  "VPC Part-lvl Slope Rep",
  "VPC Part-Set Lvl Intercept",
  "VPC Part-Set Lvl Slope Rep",
  "VPC Part-Set Lvl Slope Quad.Rep")

# Define models names and pair them with their associated bootstrap dataset objects
model_info <- tribble(
  ~model_label,       ~boot_data,
  "Null 3s",          stats_null_3s_renamed,
  "MVIC 3s",          stats_mvic_3s_renamed,
  "TT 3s",            stats_tt_3s_renamed,
  "TT/MVIC 3s",       stats_tt_mvic_3s_renamed,
  "Null 6s",          stats_null_6s_renamed,
  "MVIC 6s",          stats_mvic_6s_renamed,
  "TT 6s",            stats_tt_6s_renamed,
  "TT/MVIC 6s",       stats_tt_mvic_6s_renamed,
)
# Define the function that computes box plots of VPC vars
plot_total_variance_models <- function(variables, summary_table, model_info) {
  require(tidyverse)
  plots <- map(variables, function(var_name) {
    # Determine if this variable is a VPC variable
    is_vpc <- str_detect(var_name, "VPC")
    # Step 1: Extract bootstrap CIs of VPCs from boot datasets
    boot_summary <- model_info %>%
      mutate(
        lower = map_dbl(boot_data, ~ {
          val <- quantile(.x[[var_name]], 0.025, na.rm = TRUE)
          if (is_vpc) val * 100 else val
        }),
        upper = map_dbl(boot_data, ~ {
          val <- quantile(.x[[var_name]], 0.975, na.rm = TRUE)
          if (is_vpc) val * 100 else val
        })
      ) %>%
      select(model_label, lower, upper)
    # Step 2: Extract model estimates of VPCs from Summary table
    summary_values <- summary_table %>%
      filter(variable == var_name) %>%
      pivot_longer(-variable, names_to = "model_label", values_to = "estimate") %>%
      mutate(estimate = as.numeric(str_extract(estimate, "[0-9.]+")),
             estimate = if (is_vpc) estimate * 100 else estimate)
    # Step 3: Combine and relevel factor
    plot_data <- left_join(boot_summary, summary_values, by = "model_label")
    plot_data$model_label <- factor(plot_data$model_label, levels = model_info$model_label)
    # Step 4: Plot
    ggplot(plot_data, aes(x = model_label, y = estimate)) +
      geom_point(color = "blue", size = 3) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
      labs(
        title = if (is_vpc) paste(var_name, "(%)") else var_name,
        x = "",
        y = ""
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 14),
            title = element_text(size = 12))
  })
  
  names(plots) <- variables
  return(plots)
}
# Create a plot list of all variables defined previously to plot
plots_list <- plot_total_variance_models(variance_vars_to_plot, summary_table, model_info)

# Extract plots in the desired order
p1 <- plots_list[["Total Variance"]]
p2 <- plots_list[["VPC Residual"]]
p3 <- plots_list[["VPC Part-lvl Intercept"]]
p4 <- plots_list[["VPC Part-lvl Slope Set"]]
p5 <- plots_list[["VPC Part-lvl Slope Rep"]]
p6 <- plots_list[["VPC Part-Set Lvl Intercept"]]
p7 <- plots_list[["VPC Part-Set Lvl Slope Rep"]]
p8 <- plots_list[["VPC Part-Set Lvl Slope Quad.Rep"]]

# Combine plots using patchwork
final_plot <- (
  p1
) /
  (p3 + p4 + p5) /
  p2 /
  (p6 + p7 + p8) +
  plot_annotation(
    title = "TT Response Variance Decomposition by Model:\n Total variance, VPCs of Random effects, and Residuals",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0))
  )

# To visualize the final VPC plot save with 1100:1100 aspect ratio and open the file


#### ...Plot bootstrapped random effects estimates across null and baseline models ####
# Use the already re-named boot datasets from the code that renamed the variables in the previous VPC plot section.

# Specify Function to create the density distribution plots of random effects
generate_combined_density_plots <- function(dataset_1, dataset_2, dataset_3) {
  all_variables <- unique(c(names(dataset_1), names(dataset_2), names(dataset_3)))
  all_variables <- setdiff(all_variables, "group")  # Remove the 'group' column if present
  plot_list_combined <- lapply(all_variables, function(var) {
    p <- ggplot()
    create_density_plot <- function(data, color, fill_color, alpha, mean_color, mean_linetype) {
      data <- data[!is.na(data)]  # Remove NAs
      if (length(data) > 1) {
        density_data <- density(data)
        density_df <- data.frame(x = density_data$x, y = density_data$y)
        mean_val <- mean(data)
        lower_66 <- quantile(data, 0.17)
        upper_66 <- quantile(data, 0.83)
        lower_95 <- quantile(data, 0.025)
        upper_95 <- quantile(data, 0.975)
        mean_y <- approx(density_data$x, density_data$y, xout = mean_val)$y
        p <- p +
          geom_line(data = density_df, aes(x = x, y = y), color = color, size = 1) +
          geom_ribbon(data = subset(density_df, x >= lower_66 & x <= upper_66), 
                      aes(x = x, ymin = 0, ymax = y), 
                      fill = fill_color, alpha = alpha) +
          geom_ribbon(data = subset(density_df, x >= lower_95 & x <= upper_95), 
                      aes(x = x, ymin = 0, ymax = y), 
                      fill = fill_color, alpha = alpha) +
          geom_segment(aes(x = mean_val, xend = mean_val, y = 0, yend = mean_y), 
                       color = mean_color, linetype = mean_linetype, size = 1)
      }
      return(p)
    }
    if (var %in% names(dataset_1)) {
      p <- create_density_plot(dataset_1[[var]], color = "blue", fill_color = "blue", alpha = 0.2, 
                               mean_color = "blue", mean_linetype = "dashed")
    }
    if (var %in% names(dataset_2)) {
      p <- create_density_plot(dataset_2[[var]], color = "red", fill_color = "red", alpha = 0.2, 
                               mean_color = "red", mean_linetype = "dashed")
    }
    if (var %in% names(dataset_3)) {
      p <- create_density_plot(dataset_3[[var]], color = "green", fill_color = "green", alpha = 0.2, 
                               mean_color = "green", mean_linetype = "dashed")
    }
    # Finalize plot
    p <- p +
      labs(x = var, y = "Density") +
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2)
      )
    return(p)
  })
  return(plot_list_combined)
}
# Create the plots
plot_random_across_models_3s <- generate_combined_density_plots(stats_null_3s_renamed, stats_tt_3s_renamed, stats_tt_mvic_3s_renamed)
plot_random_across_models_6s <- generate_combined_density_plots(stats_null_6s_renamed, stats_tt_6s_renamed, stats_tt_mvic_6s_renamed)
# Visualize plots for 3s intervention
plot_random_across_3s <- grid.arrange(
  plot_random_across_models_3s[[15]], do.call(arrangeGrob, c(plot_random_across_models_3s[12:14], plot_random_across_models_3s[9:11], list(nrow = 3, ncol = 2))),
  nrow = 2, top = textGrob(
    "Density Plots of Random Effects\n across Models for intervention 3s",
    gp = gpar(fontsize = 16, fontface = "bold"),
    hjust = 0.5),
  heights = unit(c(1, 3), "null"))
# Visualize plots for 6s intervention
plot_random_across_6s <- grid.arrange(
  plot_random_across_models_6s[[15]], do.call(arrangeGrob, c(plot_random_across_models_6s[12:14], plot_random_across_models_6s[9:11], list(nrow = 3, ncol = 2))),
  nrow = 2, top = textGrob(
    "Density Plots of Random Effects\n across Models for intervention 6s",
    gp = gpar(fontsize = 16, fontface = "bold"),
    hjust = 0.5),
  heights = unit(c(1, 3), "null"))
# Visualize final plot for both interventions
plot_random_across <- grid.arrange(
  plot_random_across_3s, plot_random_across_6s, ncol = 2)



#### Part 2: Step 1. Model specification (combined intervention Null and Baseline model specifications) ####
# Note: Here we combined the two interventions into a unanimous 4 level model in order to directly assess differences between
# baseline torque variables and PAP dynamics, between interventions while accounting for the fact that the same participants experienced both
# interventions. Differences were assessed through baseline - intervention interactions.

# Combined intervention Null model specification
# Note: Here we used a combined intervention specification of fixed effects that were defined the independent intervention
# analysis null model building..
Null_mod <- lmer(twitch_torque_response ~ 
                   # Fixed effects
                   Rep_c * Set_c * intervention
                 + I(Rep_c^2) * Set_c * intervention
                 + I(Rep_c^3) * Set_c * intervention
                 + I(Set_c^2) * intervention
                 # Random effects
                 + (1 + Rep_c | Part_id)
                 + (1 + Set_c | intervention:Part_id)
                 + (1 + Rep_c | Set_c:intervention:Part_id) + (0 + I(Rep_c^2) | Set_c:intervention:Part_id)
                 , REML = FALSE, data = data)
# Baseline model with interactions with null models fixed effects
Base_mod_mvic <- lmer(twitch_torque_response ~ 
                        # Fixed effects
                        Rep_c * Set_c * intervention * baseline_mvic_c
                      + I(Rep_c^2) * Set_c * intervention * baseline_mvic_c
                      + I(Rep_c^3) * Set_c * intervention * baseline_mvic_c
                      + I(Set_c^2) * intervention * baseline_mvic_c
                      # Random effects
                      + (1 + Rep_c | Part_id)
                      + (1 + Set_c | intervention:Part_id)
                      + (1 + Rep_c | Set_c:intervention:Part_id) + (0 + I(Rep_c^2) | Set_c:intervention:Part_id)
                      , REML = FALSE, data = data)
Base_mod_tt <- lmer(twitch_torque_response ~ 
                      # Fixed effects
                      Rep_c * Set_c * intervention * baseline_twitch_torque_c
                    + I(Rep_c^2) * Set_c * intervention * baseline_twitch_torque_c
                    + I(Rep_c^3) * Set_c * intervention * baseline_twitch_torque_c
                    + I(Set_c^2) * intervention * baseline_twitch_torque_c
                    # Random effects
                    + (1 + Rep_c | Part_id)
                    + (1 + Set_c | intervention:Part_id)
                    + (1 + Rep_c | Set_c:intervention:Part_id) + (0 + I(Rep_c^2) | Set_c:intervention:Part_id)
                    , REML = FALSE, data = data)
Base_mod_tt_mvic <- lmer(twitch_torque_response ~ 
                           # Fixed effects
                           Rep_c * Set_c * intervention * baseline_tt_mvic_ratio_c
                         + I(Rep_c^2) * Set_c * intervention * baseline_tt_mvic_ratio_c
                         + I(Rep_c^3) * Set_c * intervention * baseline_tt_mvic_ratio_c
                         + I(Set_c^2) * intervention * baseline_tt_mvic_ratio_c
                         # Random effects
                         + (1 + Rep_c | Part_id) 
                         + (1 + Set_c | intervention:Part_id)
                         + (1 + Rep_c | Set_c:intervention:Part_id) + (0 + I(Rep_c^2) | Set_c:intervention:Part_id)
                         , REML = FALSE, data = data)

#### Part 2: Step 2. Null Vs Baseline model comparisons (combined dataset and models for both interventions) ####
summarize_combined_models <- function(
    all_model_list, all_model_names,
    all_null_model_list, all_null_model_names,
    rename_vars = NULL  # argument for renaming
) {
  extract_fixed_effects <- function(model, model_name) {
    sum_model <- summary(model)
    coefs <- as.data.frame(fixef(model)) %>%
      tibble::rownames_to_column("variable") %>%
      rename(estimate_num = `fixef(model)`) %>%
      mutate(model = model_name)
    if (!is.null(sum_model$coefficients)) {
      se <- sum_model$coefficients[, "Std. Error"]
      pvals <- sum_model$coefficients[, "Pr(>|t|)"]
      coefs <- coefs %>%
        mutate(
          se = se[variable],
          p_value_num = pvals[variable],
          p_value = ifelse(p_value_num < 0.001, "<0.001", sprintf("%.3f", p_value_num))
        )
    } else {
      coefs <- coefs %>%
        mutate(se = NA_real_, p_value_num = NA_real_, p_value = NA_character_)
    }
    coefs <- coefs %>%
      mutate(
        stars = case_when(
          is.na(p_value_num) ~ "",
          p_value_num < 0.001 ~ "***",
          p_value_num < 0.01 ~ "**",
          p_value_num < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
    coefs <- coefs %>%
      mutate(
        estimate_rounded = round(estimate_num, 3),
        estimate = paste0(
          estimate_rounded, stars, "\n",
          "se: ", round(se, 3), "\n",
          "pval: ", p_value
        )
      ) %>%
      mutate(variable = variable %>%
               str_replace_all("baseline_mvic_c", "baseline") %>%
               str_replace_all("baseline_twitch_torque_c", "baseline") %>%
               str_replace_all("baseline_tt_mvic_ratio_c", "baseline")
      ) %>%
      select(variable, estimate, model)
    return(coefs)
  }
  
  extract_model_metrics <- function(model, model_name, null_model, null_name) {
    aicc_value <- round(AICc(model), 3)
    r2 <- as.data.frame(r.squaredGLMM(model))
    colnames(r2) <- c("marginal_r2", "conditional_r2")
    marginal_r2 <- round(r2$marginal_r2[1], 3)
    conditional_r2 <- round(r2$conditional_r2[1], 3)
    if (!is.null(null_model) && model_name != null_name) {
      lrt <- anova(null_model, model)
      p_value <- lrt$`Pr(>Chisq)`[2]
      df_diff <- lrt$Df[2]
      if (p_value < 0.001) {
        lrt_string <- paste0("<0.001*** (", df_diff, ")")
      } else if (p_value < 0.01) {
        lrt_string <- paste0(round(p_value, 3), "** (", df_diff, ")")
      } else if (p_value < 0.05) {
        lrt_string <- paste0(round(p_value, 3), "* (", df_diff, ")")
      } else {
        lrt_string <- paste0(round(p_value, 3), " (", df_diff, ")")
      }
    } else {
      lrt_string <- NA_character_
    }
    
    # Random effects
    # Refit models with REML
    refit_with_reml <- function(model) {
      update(model, REML = TRUE)
    }
    model_reml <- refit_with_reml(model)
    
    extract_correlations <- function(model_reml) {
      vc <- VarCorr(model_reml)
      cor_list <- list()
      for (grp in names(vc)) {
        mat <- as.matrix(vc[[grp]])
        if (ncol(mat) > 1) {
          cor_mat <- cov2cor(mat)
          cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA
          cor_df <- as.data.frame(as.table(cor_mat))
          cor_df <- cor_df[!is.na(cor_df$Freq), ]
          cor_df <- cor_df %>%
            mutate(
              cor_label = paste0("Corr_", grp, "_", Var1, "_", Var2),
              correlation = round(Freq, 3)
            ) %>%
            select(cor_label, correlation)
          cor_list[[grp]] <- cor_df
        }
      }
      if (length(cor_list) > 0) {
        return(dplyr::bind_rows(cor_list))
      } else {
        return(tibble(cor_label = character(), correlation = numeric()))
      }
    }
    varcomp_raw <- as.data.frame(VarCorr(model_reml)) %>%
      filter(is.na(var2)) %>%
      mutate(
        effect_name = ifelse(is.na(var1), "(Intercept)", var1),
        level = as.character(grp)
      )
    total_var <- sum(varcomp_raw$vcov)
    varcomp <- varcomp_raw %>%
      mutate(VPC = round(vcov / total_var, 3)) %>%
      select(level, effect_name, VPC)
    corrs_raw <- extract_correlations(model_reml)
    metrics_df <- tibble(
      variable = c(
        "AICc", "LRT vs Null", "marginal_r2", "conditional_r2",
        paste0("VPC_", varcomp$level, "_", varcomp$effect_name),
        if (nrow(corrs_raw) > 0) corrs_raw$cor_label,
        "Total Variance"
      ),
      estimate = as.character(c(
        aicc_value,
        lrt_string,
        marginal_r2,
        conditional_r2,
        varcomp$VPC,
        if (nrow(corrs_raw) > 0) corrs_raw$correlation,
        round(total_var, 3)
      )),
      model = model_name
    )
    return(metrics_df)
  }
  null_models_aligned <- purrr::map(all_model_names, function(m_name) {
    if (m_name == "Null_mod") {
      return(NULL)  
    } else {
      return(all_null_model_list[[1]])  
    }
  })
  null_names_aligned <- purrr::map_chr(all_model_names, function(m_name) {
    all_null_model_names[[1]]  
  })
  fixed_list <- purrr::map2(all_model_list, all_model_names, extract_fixed_effects)
  metrics_list <- purrr::pmap(
    list(all_model_list, all_model_names, null_models_aligned, null_names_aligned),
    extract_model_metrics
  )
  
  all_fixed <- bind_rows(fixed_list)
  all_metrics <- bind_rows(metrics_list)
  
  full_df <- bind_rows(all_fixed, all_metrics) %>%
    group_by(model, variable) %>%
    summarise(estimate = dplyr::first(estimate), .groups = "drop") %>%
    pivot_wider(names_from = model, values_from = estimate)
  
  intercept_row <- "(Intercept)"
  baseline_int_rows <- unique(full_df$variable[str_detect(full_df$variable, "intervention6s:baseline")])
  baseline_rows_all <- unique(full_df$variable[str_detect(full_df$variable, "baseline")])
  baseline_rows <- setdiff(baseline_rows_all, baseline_int_rows)
  metric_rows <- c("AICc", "LRT vs Null", "marginal_r2", "conditional_r2")
  varcomp_rows <- full_df$variable[str_starts(full_df$variable, "VPC_") | full_df$variable == "Total Variance"]
  corr_rows <- full_df$variable[str_starts(full_df$variable, "Corr_")]
  other_fixed <- setdiff(
    full_df$variable,
    c(intercept_row, baseline_int_rows, baseline_rows, metric_rows, varcomp_rows, corr_rows)
  )
  final_order <- c(intercept_row, baseline_int_rows, baseline_rows, other_fixed, metric_rows, varcomp_rows, corr_rows)
  full_df <- full_df %>%
    arrange(factor(variable, levels = final_order))
  desired_column_order <- c("variable", all_model_names)
  full_df <- full_df %>%
    select(any_of(desired_column_order))
  # Rename variables if rename_vars is provided
  if (!is.null(rename_vars) && length(rename_vars) > 0) {
    full_df <- full_df %>%
      mutate(variable = ifelse(variable %in% names(rename_vars),
                               rename_vars[variable],
                               variable))
  }
  full_df <- full_df %>%
    dplyr::mutate(dplyr::across(everything(), ~ ifelse(is.na(.), "-", .)))
  return(full_df)
}
# Run the function to extract statistics
summary_combined_table <- summarize_combined_models(
  all_model_list = list(Null_mod, Base_mod_mvic, Base_mod_tt, Base_mod_tt_mvic),
  all_model_names = c("Null Model", "MVIC Model", "TT Model", "TT/MVIC Model"),
  all_null_model_list = list(Null_mod),
  all_null_model_names = c("Null Model"),
  rename_vars = c(
    "marginal_r2" = "Marginal R2",
    "conditional_r2" = "Conditional R2",
    "VPC_Part_id_(Intercept)" = "VPC Part-lvl Intercept",
    "VPC_Part_id_Rep_c" = "VPC Part-lvl Slope Rep",
    "VPC_Residual_(Intercept)" = "VPC Residual",
    "VPC_Set_c.intervention.Part_id.1_(Intercept)" = "VPC Part-Int-Set Lvl Intercept",
    "VPC_Set_c.intervention.Part_id.1_Rep_c" = "VPC Part-Int-Set Lvl Slope Rep",
    "VPC_Set_c.intervention.Part_id_I(Rep_c^2)" = "VPC Part-Int-Set Lvl Slope Quad.Rep",
    "VPC_intervention.Part_id_(Intercept)" = "VPC Part-Int Lvl Intercept",
    "VPC_intervention.Part_id_Set_c" = "VPC Part-Int Lvl Slope Set",
    "Corr_Part_id_(Intercept)_Rep_c" = "Corr. Part Lvl Interc.& Sl. Rep",
    "Corr_Set_c.intervention.Part_id.1_(Intercept)_Rep_c" = "Corr. Part-Int-Set Lvl Interc.& Sl. Rep",
    "Corr_intervention.Part_id_(Intercept)_Set_c" = "Corr. Part-Int Lvl Interc.& Sl. Set")
)
# View the results of the model parameter estimates of combined intervention models and model comparison statistics 
# - summary of combined intervention table
print(summary_combined_table, n = 52)
#### ...Save summary of combined model comparisons ####
write_xlsx(summary_combined_table, path = "SPECIFY PATH TO SAVE THE COMBINED INTERVENTION MODEL ESTIMATE SUMMARY")


#### Part 2: Step 3. Case bootstrapping of combined intervention model estimates ####
# Note: We fitted these models with uncorrelated random effects as preliminary bootstraps resulted in >20% convergence issues.
# We also removed random quadratic slope within sets and participants, as that was found to produce over 10% convergence issues.
# Re-fit models using REML
Null_mod <- lmer(twitch_torque_response ~ 
                   # Fixed effects
                   Rep_c * Set_c * intervention
                 + I(Rep_c^2) * Set_c * intervention
                 + I(Rep_c^3) * Set_c * intervention
                 + I(Set_c^2) * intervention
                 # Random effects
                 + (1 | Part_id) + (0 + Rep_c | Part_id)
                 + (1 | intervention:Part_id) + (0 + Set_c | intervention:Part_id)
                 + (1 | Set_c:intervention:Part_id) + (0 + Rep_c | Set_c:intervention:Part_id)
                 , REML = TRUE, data = data)
Base_mod_mvic <- lmer(twitch_torque_response ~ 
                        # Fixed effects
                        Rep_c * Set_c * intervention * baseline_mvic_c
                      + I(Rep_c^2) * Set_c * intervention * baseline_mvic_c
                      + I(Rep_c^3) * Set_c * intervention * baseline_mvic_c
                      + I(Set_c^2) * intervention * baseline_mvic_c
                      # Random effects
                      + (1 | Part_id) + (0 + Rep_c | Part_id)
                      + (1 | intervention:Part_id) + (0 + Set_c | intervention:Part_id)
                      + (1 | Set_c:intervention:Part_id) + (0 + Rep_c | Set_c:intervention:Part_id)
                      , REML = TRUE, data = data)
# Baseline model with interactions with null models fixed effects
Base_mod_tt <- lmer(twitch_torque_response ~ 
                      # Fixed effects
                      Rep_c * Set_c * intervention * baseline_twitch_torque_c
                    + I(Rep_c^2) * Set_c * intervention * baseline_twitch_torque_c
                    + I(Rep_c^3) * Set_c * intervention * baseline_twitch_torque_c
                    + I(Set_c^2) * intervention * baseline_twitch_torque_c
                    # Random effects
                    + (1 | Part_id) + (0 + Rep_c | Part_id)
                    + (1 | intervention:Part_id) + (0 + Set_c | intervention:Part_id)
                    + (1 | Set_c:intervention:Part_id) + (0 + Rep_c | Set_c:intervention:Part_id)
                    , REML = TRUE, data = data)
Base_mod_tt_mvic <- lmer(twitch_torque_response ~ 
                           # Fixed effects
                           Rep_c * Set_c * intervention * baseline_tt_mvic_ratio_c
                         + I(Rep_c^2) * Set_c * intervention * baseline_tt_mvic_ratio_c
                         + I(Rep_c^3) * Set_c * intervention * baseline_tt_mvic_ratio_c
                         + I(Set_c^2) * intervention * baseline_tt_mvic_ratio_c
                         # Random effects
                         + (1 | Part_id) + (0 + Rep_c | Part_id)
                         + (1 | intervention:Part_id) + (0 + Set_c | intervention:Part_id)
                         + (1 | Set_c:intervention:Part_id) + (0 + Rep_c | Set_c:intervention:Part_id)
                         , REML = TRUE, data = data)
# Set seed for reproducibility of bootstraps
set.seed(123)
# Specify the models to do bootstraps on
comb_models <- list(Null_mod, Base_mod_mvic, Base_mod_tt, Base_mod_tt_mvic)
# Specify a function to extract the parameter estimates of interest after every re-sample and refit
mySumm <- function(.) {
  s <- getME(., "sigma") 
  c(beta = getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta")))}
# Start a list to store bootstrap results in
bootstrap_results_combined <- list()
# Run case bootstraps on the models specified using a loop
for (i in 1:length(comb_models)) {
  # In the loop, run the bootstrap for each model, and store the results in the list
  # Specify Re-sampling scheme only at the participant level
  bootstrap_results_combined[[i]] <- 
    case_bootstrap(comb_models[[i]], .f = mySumm, resample = c(TRUE, FALSE, FALSE, FALSE), .refit = TRUE, B = 10000)
  cat("Finished bootstrap for model", i, "\n")}
# Extract bootstrap results for each model
case_null_mod <- bootstrap_results_combined[[1]]
case_mvic_mod <- bootstrap_results_combined[[2]]
case_tt_mod <- bootstrap_results_combined[[3]]
case_tt_mvic_mod <- bootstrap_results_combined[[4]]
# Bootstrap model fitting diagnostics of warnings
analyze_model_issues <- function(data) {
  data_name <- deparse(substitute(data))
  singular_indices <- which(sapply(data$message, function(x) identical(x, "boundary (singular) fit: see help('isSingular')\n")))
  convergence_indices <- which(sapply(data$warning, function(x) is.character(x) && grepl("Model failed to converge", x[1])))
  cat("The", data_name, "produced", length(singular_indices), "models with singular fit messages and", length(convergence_indices), "models with convergence warnings.\n")
}
analyze_model_issues(case_null_mod)
analyze_model_issues(case_mvic_mod)
analyze_model_issues(case_tt_mod)
analyze_model_issues(case_tt_mvic_mod)
# Filter non-converged models
filter_nonconverged_replicates <- function(bootstrap_result, model_name = "") {
  nonconverged <- sapply(bootstrap_result$warning, function(x) 
    is.character(x) && grepl("Model failed to converge", x[1]))
  keep_indices <- which(!nonconverged)
  n_original <- nrow(bootstrap_result$replicates)
  n_kept <- length(keep_indices)
  n_removed <- n_original - n_kept
  cat(sprintf("Model %s: %d original, %d non-converged removed, %d kept.\n", 
              model_name, n_original, n_removed, n_kept))
  bootstrap_result$replicates <- bootstrap_result$replicates[keep_indices, , drop = FALSE]
  bootstrap_result$warning <- bootstrap_result$warning[keep_indices]
  bootstrap_result$message <- bootstrap_result$message[keep_indices]
  return(bootstrap_result)}
# Filter non-converged models
case_null_mod <- filter_nonconverged_replicates(case_null_mod, "Null Model")
case_mvic_mod <- filter_nonconverged_replicates(case_mvic_mod, "MVIC Model")
case_tt_mod <- filter_nonconverged_replicates(case_tt_mod, "TT Model")
case_tt_mvic_mod <- filter_nonconverged_replicates(case_tt_mvic_mod, "TT/MVIC Model")
# extract datasets of bootstrapped model estimates
stats_null_mod <- as.data.frame(case_null_mod$replicates)
stats_mvic_mod <- as.data.frame(case_mvic_mod$replicates)
stats_tt_mod <- as.data.frame(case_tt_mod$replicates)
stats_tt_mvic_mod <- as.data.frame(case_tt_mvic_mod$replicates)
# Specify function to Rename variables to their actual names..
rename_stats_columns <- function(stats_df, model) {
  beta_names <- names(fixef(model))
  colnames(stats_df)[seq_along(beta_names)] <- beta_names
  varcorr_df <- as.data.frame(VarCorr(model))
  rand_names <- ifelse(varcorr_df$grp == "Residual",
                       "Residual",
                       paste0(varcorr_df$grp, "_", varcorr_df$var1))
  sigma_cols <- grep("^sig0\\d{2,}", colnames(stats_df), value = TRUE)
  rand_effects <- rand_names[varcorr_df$grp != "Residual"]
  if (length(sigma_cols) != length(rand_effects)) {
    warning("Number of sigma columns and random effects do not match!")
  }
  for (i in seq_along(sigma_cols)) {
    colnames(stats_df)[colnames(stats_df) == sigma_cols[i]] <- rand_effects[i]
  }
  if ("sigma" %in% colnames(stats_df)) {
    colnames(stats_df)[colnames(stats_df) == "sigma"] <- "Residual"
  }
  return(stats_df)
}
# Rename columns
stats_null_mod_renamed_v <- rename_stats_columns(stats_null_mod, Null_mod)
stats_mvic_mod_renamed_v <- rename_stats_columns(stats_mvic_mod, Base_mod_mvic)
stats_tt_mod_renamed_v <- rename_stats_columns(stats_tt_mod, Base_mod_tt)
stats_tt_mvic_mod_renamed_v <- rename_stats_columns(stats_tt_mvic_mod, Base_mod_tt_mvic)
# Compute bootstrapped VPCs
compute_vpc <- function(df) {
  variance_cols <- grep("Part_id|Residual", names(df), value = TRUE)
  sqrd_df <- df[variance_cols]^2
  df$total_variance <- rowSums(sqrd_df, na.rm = TRUE)
  # Compute VPC for each component
  for (col in variance_cols) {
    vpc_col_name <- paste0("VPC_", col)
    df[[vpc_col_name]] <- sqrd_df[[col]] / df$total_variance
  }
  return(df)
}
stats_null_mod_renamed <- compute_vpc(stats_null_mod_renamed_v)
stats_mvic_mod_renamed <- compute_vpc(stats_mvic_mod_renamed_v)
stats_tt_mod_renamed <- compute_vpc(stats_tt_mod_renamed_v)
stats_tt_mvic_mod_renamed <- compute_vpc(stats_tt_mvic_mod_renamed_v)
#### ...save bootstrapped datasets ####
write_xlsx(stats_null_mod_renamed, path = " SPECIFY PATH TO SAVE BOOTSTRAP MODEL ESTIMATE DATASET OF THIS COMBINED INTERVENTION MODEL ")
write_xlsx(stats_mvic_mod_renamed, path = " SPECIFY PATH TO SAVE BOOTSTRAP MODEL ESTIMATE DATASET OF THIS COMBINED INTERVENTION MODEL ")
write_xlsx(stats_tt_mod_renamed, path = " SPECIFY PATH TO SAVE BOOTSTRAP MODEL ESTIMATE DATASET OF THIS COMBINED INTERVENTION MODEL ")
write_xlsx(stats_tt_mvic_mod_renamed, path = " SPECIFY PATH TO SAVE BOOTSTRAP MODEL ESTIMATE DATASET OF THIS COMBINED INTERVENTION MODEL ")

# Summary table for combined intervention bootstrap model parameter estimates and confidence intervals
summarize_and_collapse_combined <- function(
    stats_null_mod, stats_mvic_mod, stats_tt_mod, stats_tt_mvic_mod,
    rename_vars = NULL  # argument for renaming
) {
  summarize_df <- function(df) {
    sapply(df, function(col) {
      if (is.numeric(col)) {
        m <- mean(col, na.rm = TRUE)
        med <- median(col, na.rm = TRUE)
        lower <- quantile(col, probs = 0.025, na.rm = TRUE)
        upper <- quantile(col, probs = 0.975, na.rm = TRUE)
        sprintf("Mean: %.2f\nMedian: %.2f\n(%.2f:%.2f)", m, med, lower, upper)
      } else {
        NA_character_
      }
    })
  }
  sum_null <- summarize_df(stats_null_mod)
  sum_mvic <- summarize_df(stats_mvic_mod)
  sum_tt <- summarize_df(stats_tt_mod)
  sum_tt_mvic <- summarize_df(stats_tt_mvic_mod)
  all_vars <- unique(c(
    names(sum_null), names(sum_mvic), names(sum_tt), names(sum_tt_mvic)
  ))
  summary_table <- data.frame(
    Variable = all_vars,
    `Null Model` = sum_null[all_vars],
    `MVIC Model` = sum_mvic[all_vars],
    `TT Model` = sum_tt[all_vars],
    `TT/MVIC Model` = sum_tt_mvic[all_vars],
    stringsAsFactors = FALSE
  )
  summary_table$Variable <- gsub("baseline_mvic_c", "baseline", summary_table$Variable)
  summary_table$Variable <- gsub("baseline_twitch_torque_c", "baseline", summary_table$Variable)
  summary_table$Variable <- gsub("baseline_tt_mvic_ratio_c", "baseline", summary_table$Variable)
  summary_table <- summary_table %>%
    group_by(Variable) %>%
    summarise(across(everything(), ~ {
      vals <- unique(na.omit(.x))
      if (length(vals) == 0) {
        "-"
      } else if (length(vals) == 1) {
        vals
      } else {
        paste(vals, collapse = " | ")
      }
    }), .groups = "drop")
  summary_table <- summary_table %>%
    mutate(order_group = case_when(
      Variable == "(Intercept)" ~ 1,
      grepl("intervention6s:baseline", Variable) ~ 2,
      grepl("baseline:^", Variable) ~ 3,
      grepl("baseline", Variable) ~ 4,
      grepl("Part_id", Variable) ~ 6,
      grepl("VPC", Variable) ~ 7,
      grepl("Residual", Variable) ~ 8,
      TRUE ~ 5
    )) %>%
    arrange(order_group, Variable) %>%
    select(-order_group)
  # Rename variables if rename_vars is provided 
  if (!is.null(rename_vars) && length(rename_vars) > 0) {
    summary_table <- summary_table %>%
      mutate(Variable = ifelse(Variable %in% names(rename_vars),
                               rename_vars[Variable],
                               Variable))
  }
  return(summary_table)
}
bootstrap_combined_summary <- summarize_and_collapse_combined(
  stats_null_mod_renamed, stats_mvic_mod_renamed, stats_tt_mod_renamed, stats_tt_mvic_mod_renamed,
  rename_vars = c("Part_id.1_(Intercept)" = "Part-lvl Intercept (SD)",
                  "Part_id_Set_c" = "Part-lvl Slope Set (SD)",
                  "Part_id_Rep_c" = "Part-lvl Slope Rep (SD)",
                  "Set_c.intervention.Part_id.1_(Intercept)" = "Part-Int-Set Lvl Intercept (SD)",
                  "Set_c.intervention.Part_id_Rep_c" = "Part-Int-Set Lvl Slope Rep (SD)",
                  "intervention.Part_id.1_(Intercept)" = "Part-Int Lvl Intercept (SD)",
                  "intervention.Part_id_Set_c" = "Part-Int Lvl Slope Set (SD)",
                  "VPC_Part_id.1_(Intercept)" = "VPC Part-lvl Intercept",
                  "VPC_Part_id_Set_c" = "VPC Part-lvl Slope Set",
                  "VPC_Part_id_Rep_c" = "VPC Part-lvl Slope Rep",
                  "VPC_Set_c.intervention.Part_id.1_(Intercept)" = "VPC Part-Int-Set Lvl Intercept",
                  "VPC_Set_c.intervention.Part_id_Rep_c" = "VPC Part-Int-Set Lvl Slope Rep",
                  "VPC_intervention.Part_id.1_(Intercept)" = "VPC Part-Int Lvl Intercept",
                  "VPC_intervention.Part_id_Set_c" = "VPC Part-Int Lvl Slope Set",
                  "Residual" = "Residual (SD)")
)
print(bootstrap_combined_summary, n = 51)

#### ...Save combined model bootstrap estimates ####
write_xlsx(bootstrap_combined_summary, path = "SPECIFY PATH TO SAVE COMBINED INTERVENTION BOOTSTRAP MODEL ESTIMATE SUMMARY TABLE")

#### Part 2: Step 4. Visualizing random effects by model ####
#### ... Create box plots of combined intervention VPCs ####
# requires the bootstrap estimate datasets of combined intervention models and summary table of model estimates, to be loaded...
# define function to rename variables unanimously across summary table and boot datasets
rename_variables_mod <- function(data, rename_vars_mod) {
  data %>%
    dplyr::rename(!!!rename_vars_mod)
}
rename_vars_mod <- c(
  "VPC Part-lvl Intercept" = "VPC_Part_id.1_(Intercept)",
  "VPC Part-lvl Slope Rep" = "VPC_Part_id_Rep_c",
  
  "VPC Part-Int Lvl Intercept" = "VPC_intervention.Part_id.1_(Intercept)",
  "VPC Part-Int Lvl Slope Set" = "VPC_intervention.Part_id_Set_c",
  
  "VPC Part-Int-Set Lvl Intercept" = "VPC_Set_c.intervention.Part_id.1_(Intercept)",
  "VPC Part-Int-Set Lvl Slope Rep" = "VPC_Set_c.intervention.Part_id_Rep_c",
  
  "VPC Residual" = "VPC_Residual",
  "Total Variance" = "total_variance"
)
# rename variables in boot datasets
boot_null_mod <- rename_variables_mod(stats_null_mod_renamed, rename_vars_mod)
boot_mvic_mod <- rename_variables_mod(stats_mvic_mod_renamed, rename_vars_mod)
boot_tt_mod <- rename_variables_mod(stats_tt_mod_renamed, rename_vars_mod)
boot_tt_mvic_mod <- rename_variables_mod(stats_tt_mod_renamed, rename_vars_mod)

# GET VPC PLOTS
# specify model names
model_names <- c("Null", "MVIC", "TT", "TT/MVIC")
# specify variables to plot
variance_vars_to_plot_mod <- c(
  "VPC Residual",
  "Total Variance",
  "VPC Part-lvl Intercept",
  "VPC Part-lvl Slope Rep",
  "VPC Part-Int Lvl Intercept",
  "VPC Part-Int Lvl Slope Set",
  "VPC Part-Int-Set Lvl Intercept",
  "VPC Part-Int-Set Lvl Slope Rep")

# Define models and pair them with their associated bootstrap dataset objects
mod_model_info <- tribble(
  ~model_label,       ~boot_data,
  "Null Model",          boot_null_mod,
  "MVIC Model",          boot_mvic_mod,
  "TT Model",            boot_tt_mod,
  "TT/MVIC Model",       boot_tt_mvic_mod,
)
# Define function to plot VPCs of combined intervention models
plot_total_variance_models <- function(variables, summary_table, model_info) {
  require(tidyverse)
  plots <- map(variables, function(var_name) {
    # Determine if this variable is a VPC variable
    is_vpc <- str_detect(var_name, "VPC")
    # Step 1: Bootstrap CIs
    boot_summary <- model_info %>%
      mutate(
        lower = map_dbl(boot_data, ~ {
          val <- quantile(.x[[var_name]], 0.025, na.rm = TRUE)
          if (is_vpc) val * 100 else val
        }),
        upper = map_dbl(boot_data, ~ {
          val <- quantile(.x[[var_name]], 0.975, na.rm = TRUE)
          if (is_vpc) val * 100 else val
        })
      ) %>%
      select(model_label, lower, upper)
    
    # Step 2: Summary estimate
    summary_values <- summary_table %>%
      filter(variable == var_name) %>%
      pivot_longer(-variable, names_to = "model_label", values_to = "estimate") %>%
      mutate(estimate = if_else(estimate == "-", NA_real_,
                                as.numeric(str_extract(estimate, "-?[0-9.]+"))),
             estimate = if (is_vpc) estimate * 100 else estimate)
    
    # Step 3: Combine and relevel factor
    plot_data <- left_join(boot_summary, summary_values, by = "model_label")
    plot_data$model_label <- factor(plot_data$model_label, levels = model_info$model_label)
    print(plot_data)
    # Step 4: Plot
    ggplot(plot_data, aes(x = model_label, y = estimate)) +
      geom_point(color = "blue", size = 3) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
      labs(
        title = if (is_vpc) paste(var_name, "(%)") else var_name,
        x = "",
        y = ""
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 14),
            title = element_text(size = 12))
  })
  
  names(plots) <- variables
  return(plots)
}
# Create a list of VPC plots
plots_list_mod <- plot_total_variance_models(variance_vars_to_plot_mod, summary_combined_table, mod_model_info)

# Extract plots in the desired order
p1_mod <- plots_list_mod[["Total Variance"]]
p2_mod <- plots_list_mod[["VPC Residual"]]
p3_mod <- plots_list_mod[["VPC Part-lvl Intercept"]]
p4_mod <- plots_list_mod[["VPC Part-lvl Slope Rep"]]
p5_mod <- plots_list_mod[["VPC Part-Int Lvl Intercept"]]
p6_mod <- plots_list_mod[["VPC Part-Int Lvl Slope Set"]]
p7_mod <- plots_list_mod[["VPC Part-Int-Set Lvl Intercept"]]
p8_mod <- plots_list_mod[["VPC Part-Int-Set Lvl Slope Rep"]]
# Combine using patchwork
final_plot_mod <- (
  p1_mod + p2_mod
) /
  (p3_mod + p4_mod) /
  (p5_mod + p6_mod) /
  (p7_mod + p8_mod) +
  plot_annotation(
    title = "TT Response Variance Decomposition by Model:\n Total variance, VPCs of Random effects, and Residuals",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0),
                  axis.title.x = )
  )

# To visualize the final VPC plot save with 1100:1100 aspect ratio and open the file


#### Part 3: Model diagnostic plots ####

# Assess models from independent intervention analysis, with specification from model comparisons
# 3s intervention
check_model(base_tt_3s)
check_model(base_tt_mvic_3s)
# 6s intervention
check_model(base_tt_6s)
check_model(base_tt_mvic_6s)

# Assess models from combined intervention analysis, with specification from model comparisons
check_model(Base_mod_tt)
check_model(Base_mod_tt_mvic)




#### Part 4: 3d plots of baseline TT/MVIC models ####
# Create a 3d plot with predicted values of TT responses across interventions, sets  and repetitions

# Function to plot 3d plane surface plot for intervention 3s model specification
plot_torque_surface <- function(full_data, set_number, dataset_name, outcome, scene_id = "scene") {
  
  # Scale variables on full_data (as before)
  full_data <- full_data %>%
    mutate(
      baseline_twitch_torque_c = scale(baseline_twitch_torque, center = TRUE, scale = FALSE),
      baseline_tt_mvic_ratio_c = scale(baseline_tt_mvic_ratio, center = TRUE, scale = FALSE),
      baseline_mvic_c = scale(baseline_mvic, center = TRUE, scale = FALSE),
      Rep_c = scale(Rep, center = TRUE, scale = FALSE),
      Set_c = scale(Set, center = TRUE, scale = FALSE)
    )
  
  # Fit the model once on full_data
  full_data$y <- full_data[[outcome]]
  pl1_6 <- lmer(y ~ 
                  baseline_tt_mvic_ratio_c * Rep_c * Set_c +
                  baseline_tt_mvic_ratio_c * I(Rep_c^2) * Set_c +
                  I(Rep_c^3) +
                  I(Set_c^2) +
                  (1 + Set_c | Part_id) +
                  (0 + Rep_c | Part_id) +
                  (1 + Rep_c | Set_c:Part_id) +
                  (0 + I(Rep_c^2) | Set_c:Part_id),
                REML = FALSE, data = full_data)
  
  # Filter full_data to only the specified set for plotting
  plot_data <- full_data %>% filter(Set == set_number)
  
  # Generate grid for predictions based on filtered plot_data
  grid_data <- expand.grid(
    baseline_tt_mvic_ratio_c = seq(min(plot_data$baseline_tt_mvic_ratio_c),
                                   max(plot_data$baseline_tt_mvic_ratio_c), length.out = 50),
    Rep_c = seq(min(plot_data$Rep_c),
                max(plot_data$Rep_c), length.out = 50),
    Set_c = unique(plot_data$Set_c)  # constant for the set
  )
  
  # Predict only using fixed effects from full model
  grid_data$outcome <- predict(pl1_6, newdata = grid_data, re.form = NA)
  
  # Arrange and prepare for surface matrix
  grid_data <- grid_data %>% arrange(Rep_c, baseline_tt_mvic_ratio_c)
  
  x_vals <- unique(grid_data$baseline_tt_mvic_ratio_c) + attr(full_data$baseline_tt_mvic_ratio_c, "scaled:center")
  y_vals <- unique(grid_data$Rep_c) + attr(full_data$Rep_c, "scaled:center")
  
  z_matrix <- matrix(
    grid_data$outcome, 
    nrow = length(y_vals), 
    ncol = length(x_vals), 
    byrow = TRUE
  )
  
  # Create the 3D surface plot with dynamic scene id
  p <- plot_ly() %>%
    add_surface(
      x = x_vals, 
      y = y_vals, 
      z = z_matrix,
      colorscale = "Viridis",
      showscale = TRUE,
      name = "Predicted Surface",
      scene = scene_id   # <-- Add this line to assign surface to correct subplot
    ) %>%
    layout(
      title = paste("3D Surface Plot of Twitch Torque Response -", dataset_name),
      scene = setNames(list(
        xaxis = list(title = "Baseline TT MVIC Ratio"),
        yaxis = list(title = "Rep Count"),
        zaxis = list(title = "Twitch Torque Response"),
        domain = list(x = c(0, 1)),
        camera = list(
          eye = list(x = 0, y = 0, z = 0)  # tweak to adjust view
        )
      ), scene_id)
    )
  
  # Add actual points for this set only
  p <- p %>%
    add_trace(
      x = plot_data$baseline_tt_mvic_ratio,
      y = plot_data$Rep,
      z = plot_data[[outcome]],
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 5,
        color = as.factor(plot_data$Part_id),
        colorscale = "Jet",
        showscale = TRUE,
        opacity = 0.9
      ),
      name = "Actual Data Points",
      scene = scene_id
    )
  
  return(p)
}
# Function to plot 3d plane surface plot for intervention 6s model specification
plot_torque_surface_6s <- function(full_data, set_number, dataset_name, outcome, scene_id = "scene") {
  
  # Scale variables on full_data (as before)
  full_data <- full_data %>%
    mutate(
      baseline_twitch_torque_c = scale(baseline_twitch_torque, center = TRUE, scale = FALSE),
      baseline_tt_mvic_ratio_c = scale(baseline_tt_mvic_ratio, center = TRUE, scale = FALSE),
      baseline_mvic_c = scale(baseline_mvic, center = TRUE, scale = FALSE),
      Rep_c = scale(Rep, center = TRUE, scale = FALSE),
      Set_c = scale(Set, center = TRUE, scale = FALSE)
    )
  
  # Fit the model once on full_data
  full_data$y <- full_data[[outcome]]
  pl1_6 <- lmer(y ~ 
                  # Fixed effects
                  baseline_tt_mvic_ratio_c * Rep_c * Set_c 
                + baseline_tt_mvic_ratio_c * I(Rep_c^2) * Set_c 
                + I(Rep_c^3) * Set_c
                # Random effects
                + (1 + Set_c | Part_id) + (0 + Rep_c | Part_id)
                + (1 + Rep_c | Set_c:Part_id) + (0 + I(Rep_c^2) | Set_c:Part_id)
                , REML = FALSE, data = full_data)
  
  # Filter full_data to only the specified set for plotting
  plot_data <- full_data %>% filter(Set == set_number)
  
  # Generate grid for predictions based on filtered plot_data
  grid_data <- expand.grid(
    baseline_tt_mvic_ratio_c = seq(min(plot_data$baseline_tt_mvic_ratio_c),
                                   max(plot_data$baseline_tt_mvic_ratio_c), length.out = 50),
    Rep_c = seq(min(plot_data$Rep_c),
                max(plot_data$Rep_c), length.out = 50),
    Set_c = unique(plot_data$Set_c)  # constant for the set
  )
  
  # Predict only using fixed effects from full models
  grid_data$outcome <- predict(pl1_6, newdata = grid_data, re.form = NA)
  
  # Arrange and prepare for surface matrix
  grid_data <- grid_data %>% arrange(Rep_c, baseline_tt_mvic_ratio_c)
  
  x_vals <- unique(grid_data$baseline_tt_mvic_ratio_c) + attr(full_data$baseline_tt_mvic_ratio_c, "scaled:center")
  y_vals <- unique(grid_data$Rep_c) + attr(full_data$Rep_c, "scaled:center")
  
  z_matrix <- matrix(
    grid_data$outcome, 
    nrow = length(y_vals), 
    ncol = length(x_vals), 
    byrow = TRUE
  )
  
  # Create the 3D surface plot with dynamic scene id
  p <- plot_ly() %>%
    add_surface(
      x = x_vals, 
      y = y_vals, 
      z = z_matrix,
      colorscale = "Viridis",
      showscale = TRUE,
      name = "Predicted Surface",
      scene = scene_id   
    ) %>%
    layout(
      title = paste("3D Surface Plot of Twitch Torque Response -", dataset_name),
      scene = setNames(list(
        xaxis = list(title = "Baseline TT MVIC Ratio"),
        yaxis = list(title = "Rep Count"),
        zaxis = list(title = "Twitch Torque Response"),
        domain = list(x = c(0, 1)),
        camera = list(
          eye = list(x = 0, y = 0, z = 0)  
        )
      ), scene_id)
    )
  
  # Add actual points for this set only
  p <- p %>%
    add_trace(
      x = plot_data$baseline_tt_mvic_ratio,
      y = plot_data$Rep,
      z = plot_data[[outcome]],
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 5,
        color = as.factor(plot_data$Part_id),
        colorscale = "Jet",
        showscale = TRUE,
        opacity = 0.9
      ),
      name = "Actual Data Points",
      scene = scene_id
    )
  
  return(p)
}
# Create plots for interventions and sets 1 and 4
plot1 <- plot_torque_surface(post_set_data_3s, set_number = 1, dataset_name = "3s Set 1", outcome = "twitch_torque_response", scene_id = "scene") %>%
  layout(scene = list(domain = list(x = c(0, 0.45), y = c(0.5, 1))))

plot2 <- plot_torque_surface(post_set_data_3s, set_number = 4, dataset_name = "3s Set 4", outcome = "twitch_torque_response", scene_id = "scene2") %>%
  layout(scene2 = list(domain = list(x = c(0.05, 1), y = c(0.5, 1))))

plot3 <- plot_torque_surface_6s(post_set_data_6s, set_number = 1, dataset_name = "6s Set 1", outcome = "twitch_torque_response", scene_id = "scene3") %>%
  layout(scene3 = list(domain = list(x = c(0, 0.45), y = c(0, 0.5))))

plot4 <- plot_torque_surface_6s(post_set_data_6s, set_number = 4, dataset_name = "6s Set 4", outcome = "twitch_torque_response", scene_id = "scene4") %>%
  layout(scene4 = list(domain = list(x = c(0.05, 1), y = c(0, 0.5))))
# Define axis ranges
z_range <- range(c(post_set_data_3s$twitch_torque_response, post_set_data_6s$twitch_torque_response))  
x_range <- range(c(post_set_data_3s$baseline_tt_mvic_ratio, post_set_data_6s$baseline_tt_mvic_ratio)) 

# Combine all plots to form the final plot
combined_plot <- subplot(plot1, plot2, plot3, plot4, nrows = 2, shareY = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.01) %>%
  layout(
    scene = list(camera = list(eye = list(x = -1.38, y = -1.90, z = 0.25)),
                 zaxis = list(title = "TT Response (%)", titlefont = list(size = 16, family = "Arial", color = "black"), range = z_range),
                 yaxis = list(title = "Repetitions"), titlefont = list(size = 16, family = "Arial", color = "black"),
                 xaxis = list(title = "Baseline TT/MVIC (%)", titlefont = list(size = 16, family = "Arial", color = "black"), range = x_range)),
    scene2 = list(camera = list(eye = list(x = -1.38, y = -1.90, z = 0.25)),
                  zaxis = list(title = "TT Response (%)", titlefont = list(size = 16, family = "Arial", color = "black"), range = z_range),
                  yaxis = list(title = "Repetitions"), titlefont = list(size = 16, family = "Arial", color = "black"),
                  xaxis = list(title = "Baseline TT/MVIC (%)", titlefont = list(size = 16, family = "Arial", color = "black"), range = x_range)),
    scene3 = list(camera = list(eye = list(x = -1.38, y = -1.90, z = 0.25)),
                  zaxis = list(title = "TT Response (%)", titlefont = list(size = 16, family = "Arial", color = "black"), range = z_range),
                  yaxis = list(title = "Repetitions"), titlefont = list(size = 16, family = "Arial", color = "black"),
                  xaxis = list(title = "Baseline TT/MVIC (%)", titlefont = list(size = 16, family = "Arial", color = "black"), range = x_range)),
    scene4 = list(camera = list(eye = list(x = -1.38, y = -1.90, z = 0.25)),
                  zaxis = list(title = "TT Response (%)", titlefont = list(size = 16, family = "Arial", color = "black"), range = z_range),
                  yaxis = list(title = "Repetitions"), titlefont = list(size = 16, family = "Arial", color = "black"),
                  xaxis = list(title = "Baseline TT/MVIC (%)", titlefont = list(size = 16, family = "Arial", color = "black"), range = x_range)),
    annotations = list(
      list(text = "<b>Intervention 3s Set 1<b>         ------>", x = 0.40, y = 0.90, showarrow = FALSE,
           xref = "paper", yref = "paper", xanchor = "right", yanchor = "bottom",
           font = list(size = 18)),
      list(text = "<b>Intervention 3s Set 4<b>", x = 0.510, y = 0.90, showarrow = FALSE,
           xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom",
           font = list(size = 18)),
      list(text = "<b>Intervention 6s Set 1<b>         ------>", x = 0.40, y = 0.42, showarrow = FALSE,
           xref = "paper", yref = "paper", xanchor = "right", yanchor = "bottom",
           font = list(size = 18)),
      list(text = "<b>Intervention 6s Set 4<b>", x = 0.510, y = 0.42, showarrow = FALSE,
           xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom",
           font = list(size = 18))
    ),
    title = list(
      text = "<b>Baseline TT/MVIC Model And Twitch Torque Response Across Interventions<b>",
      x = 0.35,  # Centered
      y = 0.94,
      xanchor = "center",
      yanchor = "bottom",
      font = list(size = 22)
    )
  )

# Save final plot and open it to visualize the 3d interactive plots
saveWidget(combined_plot, "combined_plot.html", selfcontained = TRUE)











############################ Boring diameter GLMs
library(lme4)
library(tidyverse)
diam.data <- read.csv("data/2024_boringData_masterSheet - diameter_data.csv")
clean.data <- diam.data %>% select(1:68) %>% mutate(percent_N_dry = sum(percent_N_jan, percent_N_aug))

hist(clean.data$L_onLive_diameter)
qqnorm(clean.data$L_onLive_diameter, main='Normal')
qqline(clean.data$L_onLive_diameter)
shapiro.test(clean.data$L_onLive_diameter)

hist(clean.data$L_onDead_diameter)
qqnorm(clean.data$L_onDead_diameter, main='Normal')
qqline(clean.data$L_onDead_diameter)
shapiro.test(clean.data$L_onDead_diameter)

hist(clean.data$D_onLive_diameter)
qqnorm(clean.data$D_onLive_diameter, main='Normal')
qqline(clean.data$D_onLive_diameter)
shapiro.test(clean.data$D_onLive_diameter)

hist(clean.data$D_onDead_diameter)
qqnorm(clean.data$D_onDead_diameter, main='Normal')
qqline(clean.data$D_onDead_diameter)
shapiro.test(clean.data$D_onDead_diameter)

## Live coral diameters are not normal, dead coral diameters are :(
model_L_onLive <- glm(L_onLive_diameter ~ 
                        live_coral_total + turf_total + macroalgae_total + 
                        depth_to_benthos + 
                        percent_N_may + percent_N_jan + percent_N_aug +
                        jan_d15 + may_d15 + aug_d15 + 
                        avg_speed + #avg_speed_offshore +
                        distanceOutBay, 
                      family = gaussian, 
                      data = clean.data)

step(model_L_onLive)

step_L_onLive <- glm(formula = L_onLive_diameter ~ turf_total + percent_N_may + 
                       percent_N_aug + jan_d15 + may_d15 + aug_d15 + avg_speed + 
                       distanceOutBay, family = gaussian, data = clean.data)
summary(step_L_onLive)

###############
model_L_onDead <- glm(L_onDead_diameter ~ 
                        live_coral_total + turf_total + macroalgae_total + 
                        depth_to_benthos + 
                        percent_N_may + percent_N_jan + percent_N_aug +
                        jan_d15 + may_d15 + aug_d15 + 
                        avg_speed + #avg_speed_offshore +
                        distanceOutBay, 
                      family = gaussian, 
                      data = clean.data)

step(model_L_onDead)

step_L_onDead <- glm(formula = L_onDead_diameter ~ turf_total + macroalgae_total + 
                       depth_to_benthos + percent_N_may + percent_N_jan + jan_d15 + 
                       may_d15 + aug_d15, family = gaussian, data = clean.data)
summary(step_L_onDead)

##########################
model_D_OnLive <- glm(D_onLive_diameter ~ 
                        live_coral_total + turf_total + macroalgae_total + 
                        depth_to_benthos + 
                        percent_N_may + percent_N_jan + percent_N_aug +
                        jan_d15 + may_d15 + aug_d15 + 
                        avg_speed + #avg_speed_offshore +
                        distanceOutBay, 
                      family = gaussian, 
                      data = clean.data)

step(model_D_OnLive)
step_D_onLive <- glm(formula = D_onLive_diameter ~ macroalgae_total + percent_N_jan + 
                       percent_N_aug + jan_d15 + may_d15 + aug_d15, family = gaussian, 
                     data = clean.data)
summary(step_D_onLive)
#########################
model_D_OnDead <- glm(D_onDead_diameter ~ 
                        live_coral_total + turf_total + macroalgae_total + 
                        depth_to_benthos + 
                        percent_N_may + percent_N_jan + percent_N_aug +
                        jan_d15 + may_d15 + aug_d15 + 
                        avg_speed + #avg_speed_offshore +
                        distanceOutBay, 
                      family = gaussian, 
                      data = clean.data)

step(model_D_OnDead)
step_D_onDead <- glm(formula = D_onDead_diameter ~ depth_to_benthos + percent_N_may + 
                       percent_N_jan + jan_d15 + avg_speed, family = gaussian, data = clean.data)
summary(step_D_onDead)

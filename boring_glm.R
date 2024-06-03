############## Data analysis GLM for MBQ 2024 Boring Project ###################

library(lme4)
library(tidyverse)

raw.data0 <- read.csv("data/2024_boringData_masterSheet.csv")
raw.data0.5 <- raw.data0 %>% select(2:64)
raw.data1 <- raw.data0.5 %>% select(-(44:54))
raw.data <- na.omit(raw.data1)

#hist(raw.data$density_L_onLive)
#hist(raw.data$density_L_onDead)
#hist(raw.data$density_D_liveOnLive)
#hist(raw.data$density_D_liveOnDead)
#hist(raw.data$density_D_deadOnLive)
#hist(raw.data$density_D_deadOnDead)
## NOT normally distributed

#shapiro.test(log(raw.data$L_onLive + 1))
## Distribution is NOT Gaussian

## Decided to use GLM with point counts (Poisson distribution), because ratios were not neatly distributed
model_poisson_L_onLive <- glm(L_onLive ~ 
               live_coral_total + dead_coral_total + turf_total +
               macroalgae_total + rock_total + invertebrate_total + depth_to_benthos + 
               percent_N_jan + percent_N_may + percent_N_aug + 
                 jan_d15 + may_d15 + aug_d15 + avg_speed +
                 altDistance, 
             family = poisson, 
             data = raw.data)

step(model_poisson_L_onLive)

step_L_onLive <- glm(formula = L_onLive ~ live_coral_total + dead_coral_total + 
                       turf_total + rock_total + invertebrate_total + depth_to_benthos + 
                       percent_N_jan + percent_N_aug + jan_d15 + may_d15 + aug_d15 + 
                       avg_speed + altDistance, family = poisson, data = raw.data)
summary(step_L_onLive)
## checks which variables should be removed for better fitting model

model_poisson_L_onDead <- glm(L_onDead ~ live_coral_total + dead_coral_total + turf_total +
                                macroalgae_total + rock_total + invertebrate_total + depth_to_benthos + 
                                percent_N_jan + percent_N_may + percent_N_aug + 
                                jan_d15 + may_d15 + aug_d15 + avg_speed +
                                altDistance, 
                              family = poisson, 
                              data = raw.data)


step(model_poisson_L_onDead)
step_L_onDead <- glm(formula = L_onDead ~ live_coral_total + dead_coral_total + 
                       turf_total + macroalgae_total + rock_total + invertebrate_total + 
                       depth_to_benthos + percent_N_jan + percent_N_may + percent_N_aug + 
                       jan_d15 + may_d15 + avg_speed + altDistance, family = poisson, 
                     data = raw.data)
summary(step_L_onDead)
## checks which variables should be removed for better fitting model

model_poisson_D_liveOnLive_count <- glm(D_liveOnLive ~ live_coral_total + dead_coral_total + 
                                          turf_total + macroalgae_total + rock_total + invertebrate_total + 
                                      depth_to_benthos + percent_N_jan + percent_N_may + percent_N_aug + 
                                    jan_d15 + may_d15 + aug_d15 + avg_speed +
                                    altDistance, 
                                  family = poisson, 
                                  data = raw.data)

step(model_poisson_D_liveOnLive_count)
step_D_liveOnLive <- glm(formula = D_liveOnLive ~ live_coral_total + invertebrate_total + 
                           depth_to_benthos + percent_N_jan + percent_N_may + avg_speed + 
                           altDistance, family = poisson, data = raw.data)
summary(step_D_liveOnLive)
## checks which variables should be removed for better fitting model


model_poisson_D_liveOnDead_count <- glm(D_liveOnDead ~ 
                                          live_coral_total + dead_coral_total + 
                                          turf_total + macroalgae_total + rock_total + invertebrate_total + 
                                          depth_to_benthos + percent_N_jan + percent_N_may + percent_N_aug + 
                                          jan_d15 + may_d15 + aug_d15 + avg_speed +
                                          altDistance, 
                                        family = poisson, 
                                        data = raw.data)

step(model_poisson_D_liveOnDead_count)
step_D_liveOnDead <- glm(formula = D_liveOnDead ~ live_coral_total + dead_coral_total + 
                           turf_total + macroalgae_total + rock_total + percent_N_aug + 
                           jan_d15 + may_d15 + avg_speed + altDistance, family = poisson, 
                         data = raw.data)
summary(step_D_liveOnDead)


## checks which variables should be removed for better fitting model

model_poisson_D_deadOnLive_count <- glm(D_deadOnLive ~ live_coral_total + dead_coral_total + 
                                          turf_total + macroalgae_total + rock_total + invertebrate_total + 
                                          depth_to_benthos + percent_N_jan + percent_N_may + percent_N_aug + 
                                          jan_d15 + may_d15 + aug_d15 + avg_speed +
                                          altDistance, 
                                        family = poisson, 
                                        data = raw.data)

step(model_poisson_D_deadOnLive_count)
step_D_deadOnLive <- glm(formula = D_deadOnLive ~ dead_coral_total + macroalgae_total + 
                           depth_to_benthos + percent_N_jan + percent_N_may + percent_N_aug + 
                           jan_d15 + may_d15 + aug_d15 + avg_speed, family = poisson, 
                         data = raw.data)
summary(step_D_deadOnLive)


## checks which variables should be removed for better fitting model


model_poisson_D_deadOnDead_count <- glm(D_deadOnDead ~ live_coral_total + dead_coral_total + 
                                          turf_total + macroalgae_total + rock_total + invertebrate_total + 
                                          depth_to_benthos + percent_N_jan + percent_N_may + percent_N_aug + 
                                          jan_d15 + may_d15 + aug_d15 + avg_speed +
                                          altDistance, 
                                        family = poisson, 
                                        data = raw.data)

step(model_poisson_D_deadOnDead_count)
step_D_deadOnDead <- glm(formula = D_deadOnDead ~ live_coral_total + dead_coral_total + 
                           turf_total + macroalgae_total + rock_total + invertebrate_total + 
                           percent_N_jan + percent_N_may + percent_N_aug + jan_d15 + 
                           may_d15 + aug_d15 + avg_speed + altDistance, family = poisson, 
                         data = raw.data)
summary(step_D_deadOnDead)
## checks which variables should be removed for better fitting model
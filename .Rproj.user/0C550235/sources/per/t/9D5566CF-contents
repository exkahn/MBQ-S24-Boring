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
model_poisson_L_onLive <- glm(L_onLive ~ number_total_points +
               proportion_live_coral + proportion_dead_coral + proportion_turf +
               proportion_macroalgae + proportion_rock + proportion_invertebrate + height + length +
               width + circumference + depth + depth_to_benthos + D_liveOnLive + D_deadOnLive +
                 D_liveOnDead + D_deadOnDead + tridacna + spirobranchus +
               total_live_D + total_D + percent_N_jan + percent_N_may + percent_N_aug + 
                 jan_d15 + may_d15 + aug_d15 + avg_speed +
                 altDistance, 
             family = poisson, 
             data = raw.data)


step(model_poisson_L_onLive)

step_L_onLive <- glm(formula = L_onLive ~ number_total_points + proportion_live_coral + 
                               proportion_dead_coral + proportion_turf + proportion_macroalgae + 
                               proportion_rock + proportion_invertebrate + height + length + 
                               circumference + depth + depth_to_benthos + D_liveOnLive + 
                               spirobranchus + percent_N_jan + percent_N_aug + jan_d15 + 
                               may_d15 + aug_d15 + avg_speed + altDistance, family = poisson, 
                             data = raw.data)
## checks which variables should be removed for better fitting model

model_poisson_L_onDead <- glm(L_onDead ~ number_total_points +
                                proportion_live_coral + proportion_dead_coral + proportion_turf +
                                proportion_macroalgae + proportion_rock + proportion_invertebrate + height + length +
                                width + circumference + depth + depth_to_benthos + D_liveOnLive + D_deadOnLive +
                                D_liveOnDead + D_deadOnDead + tridacna + spirobranchus +
                                total_live_D + total_D + percent_N_jan + percent_N_may + percent_N_aug + 
                                jan_d15 + may_d15 + aug_d15 + avg_speed +
                                altDistance, 
                              family = poisson, 
                              data = raw.data)


step(model_poisson_L_onDead)
step_L_onDead <- glm(formula = L_onDead ~ proportion_live_coral + proportion_turf + 
                               proportion_rock + height + length + circumference + depth + 
                               depth_to_benthos + D_liveOnLive + D_liveOnDead + spirobranchus + 
                               percent_N_jan + percent_N_may + percent_N_aug + may_d15 + 
                               aug_d15 + avg_speed + altDistance, family = poisson, data = raw.data)
## checks which variables should be removed for better fitting model

model_poisson_D_liveOnLive_count <- glm(D_liveOnLive ~ number_total_points +
                                    proportion_live_coral + proportion_dead_coral + proportion_turf +
                                    proportion_macroalgae + proportion_rock + proportion_invertebrate + height + length +
                                    width + depth + circumference + depth_to_benthos + L_onLive + L_onDead + tridacna + spirobranchus +
                                    total_L + percent_N_jan + percent_N_may + percent_N_aug + 
                                    jan_d15 + may_d15 + aug_d15 + avg_speed +
                                    altDistance, 
                                  family = poisson, 
                                  data = raw.data)

step(model_poisson_D_liveOnLive_count)
step_D_liveOnLive <- glm(formula = D_liveOnLive ~ number_total_points + proportion_live_coral + 
                           proportion_dead_coral + proportion_turf + proportion_macroalgae + 
                           proportion_rock + proportion_invertebrate + height + length + 
                           width + depth_to_benthos + L_onDead + percent_N_jan + percent_N_aug + 
                           aug_d15 + avg_speed + altDistance, family = poisson, data = raw.data)
## checks which variables should be removed for better fitting model
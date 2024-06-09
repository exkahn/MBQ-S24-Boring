############## Data analysis GLM for MBQ 2024 Boring Project ###################

library(lme4)
library(tidyverse)

raw.data.full <- read.csv("data/2024_boringData_masterSheet - complete_data(1).csv")
raw.data.no.diameters <- raw.data0 %>% select(2:70)

raw.data <- raw.data.no.diameters %>% select(-(44:54))
#raw.data <- na.omit(raw.data1)

#hist(raw.data$density_L_onLive)
#hist(raw.data$density_L_onDead)
#hist(raw.data$density_D_liveOnLive)
#hist(raw.data$density_D_liveOnDead)
#hist(raw.data$density_D_deadOnLive)
#hist(raw.data$density_D_deadOnDead)
## NOT normally distributed

#shapiro.test(log(raw.data$L_onLive + 1))
## Distribution is NOT Gaussian

z.score <- function(x) {
  centered <- x - mean(x)
  scaled <- centered/sd(centered)
  return(scaled)
}


## Decided to use Poisson distribution for point counts, because densities were not normally distributed
z.data <- raw.data %>% mutate(live_coral_z = z.score(live_coral_total),
                              dead_coral_z = z.score(dead_coral_total),
                              turf_z = z.score(turf_total),
                              macroalgae_z = z.score(macroalgae_total),
                              rock_z = z.score(rock_total),
                              invertebrate_z = z.score(invertebrate_total),
                              number_total_points_z = z.score(number_total_points),
                              depth_to_benthos_z = z.score(depth_to_benthos),
                              percent_N_jan_z =z.score(percent_N_jan),
                              percent_N_may_z = z.score(percent_N_may),
                              percent_N_aug_z = z.score(percent_N_aug),
                              jan_d15_z = z.score(jan_d15),
                              may_d15_z = z.score(may_d15),
                              aug_d15_z = z.score(aug_d15),
                              NHW_speed_z = z.score(NHW_speed),
                              NLW_speed_z = z.score(NLW_speed),
                              ELW_speed_z = z.score(ELW_speed),
                              avg_speed_z = z.score(avg_speed),
                              NHW_speed_offshore_z = z.score(NHW_speed_offshore),
                              NLW_speed_offshore_z = z.score(NLW_speed_offshore),
                              ELW_speed_offshore_z = z.score(ELW_speed_offshore),
                              avg_speed_offshore_z = z.score(avg_speed_offshore),
                              distanceOutBay_z = z.score(distanceOutBay))

##### L on live (z-score)
model_L_onLive_z <- glm(L_onLive ~ 
                                live_coral_z + dead_coral_z + turf_z +
                                macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                                percent_N_jan_z + percent_N_may_z + percent_N_aug_z + 
                                jan_d15_z + may_d15_z + aug_d15_z + 
                          NHW_speed_z + NLW_speed_z + ELW_speed_z +
                          NHW_speed_offshore_z + NLW_speed_offshore_z + ELW_speed_offshore_z
                                #avg_speed_z + avg_speed_offshore_z +
                                #+distanceOutBay_z
                          , 
                              family = poisson, 
                              data = z.data)

step(model_L_onLive_z)

step_L_onLive_z <- glm(formula = L_onLive ~ live_coral_z + dead_coral_z + turf_z + 
                         rock_z + invertebrate_z + depth_to_benthos_z + percent_N_jan_z + 
                         percent_N_may_z + percent_N_aug_z + jan_d15_z + may_d15_z + 
                         aug_d15_z + NLW_speed_z, family = poisson, data = z.data)
summary(step_L_onLive_z)


##### L on dead (z-score)
model_L_onDead_z <- glm(L_onDead ~ 
                          live_coral_z + dead_coral_z + turf_z +
                          macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                          percent_N_jan_z + percent_N_may_z + percent_N_aug_z + 
                          jan_d15_z + may_d15_z + aug_d15_z + 
                          NHW_speed_z + NLW_speed_z + ELW_speed_z, 
                        family = poisson, 
                        data = z.data)

step(model_L_onDead_z)

step_L_onDead_z <- glm(formula = L_onDead ~ live_coral_z + dead_coral_z + turf_z + 
                         macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                         percent_N_jan_z + percent_N_may_z + jan_d15_z + may_d15_z + 
                         aug_d15_z + NHW_speed_z + NLW_speed_z, family = poisson, 
                       data = z.data)
summary(step_L_onDead_z)


##### Live D on live (z-score)
model_D_liveOnLive_z <- glm(D_liveOnLive ~ 
                          live_coral_z + dead_coral_z + turf_z +
                          macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                          percent_N_jan_z + percent_N_may_z + percent_N_aug_z + 
                          jan_d15_z + may_d15_z + aug_d15_z + 
                          NHW_speed_z + NLW_speed_z + ELW_speed_z, 
                        family = poisson, 
                        data = z.data)

step(model_D_liveOnLive_z)

step_D_liveOnLive_z <- glm(formula = D_liveOnLive ~ live_coral_z + invertebrate_z + 
                             depth_to_benthos_z + percent_N_jan_z + percent_N_aug_z + 
                             may_d15_z + aug_d15_z + NHW_speed_z + NLW_speed_z, family = poisson, 
                           data = z.data)
summary(step_D_liveOnLive_z)


##### Live D on dead (z-score)
model_D_liveOnDead_z <- glm(D_liveOnDead ~ 
                              live_coral_z + dead_coral_z + turf_z +
                              macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                              percent_N_jan_z + percent_N_may_z + percent_N_aug_z + 
                              jan_d15_z + may_d15_z + aug_d15_z + 
                              NHW_speed_z + NLW_speed_z + ELW_speed_z, 
                            family = poisson, 
                            data = z.data)

step(model_D_liveOnDead_z)

step_D_liveOnDead_z <- glm(formula = D_liveOnDead ~ live_coral_z + dead_coral_z + turf_z + 
                             macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                             percent_N_jan_z + percent_N_may_z + percent_N_aug_z + jan_d15_z + 
                             may_d15_z + aug_d15_z + NHW_speed_z + NLW_speed_z, family = poisson, 
                           data = z.data)
summary(step_D_liveOnDead_z)


##### Dead D on live (z-score)
model_D_deadOnLive_z <- glm(D_deadOnLive ~ 
                              live_coral_z + dead_coral_z + turf_z +
                              macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                              percent_N_jan_z + percent_N_may_z + percent_N_aug_z + 
                              jan_d15_z + may_d15_z + aug_d15_z + 
                              NHW_speed_z + NLW_speed_z + ELW_speed_z, 
                            family = poisson, 
                            data = z.data)

step(model_D_deadOnLive_z)

step_D_deadOnLive_z <- glm(formula = D_deadOnLive ~ dead_coral_z + macroalgae_z + depth_to_benthos_z + 
                             percent_N_may_z + percent_N_aug_z + jan_d15_z + may_d15_z + 
                             aug_d15_z + NHW_speed_z + NLW_speed_z, family = poisson, 
                           data = z.data)
summary(step_D_deadOnLive_z)


##### Dead D on dead (z-score)
model_D_deadOnDead_z <- glm(D_deadOnDead ~ 
                              live_coral_z + dead_coral_z + turf_z +
                              macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                              percent_N_jan_z + percent_N_may_z + percent_N_aug_z + 
                              jan_d15_z + may_d15_z + aug_d15_z + 
                              NHW_speed_z + NLW_speed_z + ELW_speed_z, 
                            family = poisson, 
                            data = z.data)

step(model_D_deadOnDead_z)

step_D_deadOnDead_z <- glm(formula = D_deadOnDead ~ live_coral_z + dead_coral_z + turf_z + 
                             macroalgae_z + rock_z + invertebrate_z + percent_N_jan_z + 
                             percent_N_may_z + percent_N_aug_z + jan_d15_z + may_d15_z + 
                             aug_d15_z + NHW_speed_z + NLW_speed_z, family = poisson, 
                           data = z.data)
summary(step_D_deadOnDead_z)





######## Old models (not z-scored, effect sizes are not)
model_poisson_L_onLive <- glm(L_onLive ~ 
               live_coral_total + turf_total + macroalgae_total + 
                 depth_to_benthos + 
               percent_N_may + percent_N_jan + percent_N_aug +
                 jan_d15 + may_d15 + aug_d15 + 
                 avg_speed + #avg_speed_offshore +
                 distanceOutBay, 
             family = poisson, 
             data = raw.data)

step(model_poisson_L_onLive)

step_L_onLive <- glm(formula = L_onLive ~ live_coral_total + turf_total + depth_to_benthos + 
                       percent_N_may + percent_N_jan + percent_N_aug + jan_d15 + 
                       may_d15 + aug_d15 + avg_speed, family = poisson, data = raw.data)
summary(step_L_onLive)
## checks which variables should be removed for better fitting model

model_poisson_L_onDead <- glm(L_onDead ~ live_coral_total + turf_total + macroalgae_total + 
                                depth_to_benthos + 
                                percent_N_may + percent_N_jan + percent_N_aug +
                                jan_d15 + may_d15 + aug_d15 + 
                                avg_speed + #avg_speed_offshore +
                                distanceOutBay, 
                              family = poisson, 
                              data = raw.data)


step(model_poisson_L_onDead)
step_L_onDead <- glm(formula = L_onDead ~ live_coral_total + turf_total + macroalgae_total + 
                       depth_to_benthos + percent_N_may + percent_N_jan + percent_N_aug + 
                       jan_d15 + may_d15 + aug_d15 + avg_speed + distanceOutBay, 
                     family = poisson, data = raw.data)
summary(step_L_onDead)
## checks which variables should be removed for better fitting model

model_poisson_D_liveOnLive_count <- glm(D_liveOnLive ~ live_coral_total + turf_total + macroalgae_total + 
                                          depth_to_benthos + 
                                          percent_N_may + percent_N_jan + percent_N_aug +
                                          jan_d15 + may_d15 + aug_d15 + 
                                          avg_speed + #avg_speed_offshore +
                                          distanceOutBay, 
                                        family = poisson, 
                                        data = raw.data)

step(model_poisson_D_liveOnLive_count)
step_D_liveOnLive <- glm(formula = D_liveOnLive ~ live_coral_total + depth_to_benthos + 
                           percent_N_may + percent_N_aug + jan_d15 + may_d15 + aug_d15 + 
                           distanceOutBay, family = poisson, data = raw.data)
summary(step_D_liveOnLive)
## checks which variables should be removed for better fitting model


model_poisson_D_liveOnDead_count <- glm(D_liveOnDead ~ 
                                          live_coral_total + turf_total + macroalgae_total + 
                                          depth_to_benthos + 
                                          percent_N_may + percent_N_jan + percent_N_aug +
                                          jan_d15 + may_d15 + aug_d15 + 
                                          avg_speed + #avg_speed_offshore +
                                          distanceOutBay, 
                                        family = poisson, 
                                        data = raw.data)

step(model_poisson_D_liveOnDead_count)
step_D_liveOnDead <- glm(formula = D_liveOnDead ~ live_coral_total + turf_total + 
                           macroalgae_total + depth_to_benthos + percent_N_may + percent_N_aug + 
                           jan_d15 + may_d15 + distanceOutBay, family = poisson, data = raw.data)
summary(step_D_liveOnDead)


## checks which variables should be removed for better fitting model

model_poisson_D_deadOnLive_count <- glm(D_deadOnLive ~ live_coral_total + dead_coral_total + turf_total +
                                          macroalgae_total + depth_to_benthos + 
                                          percent_N_dry + percent_N_may + #percent_N_aug + 
                                          #jan_d15 + may_d15 + aug_d15 + 
                                          avg_speed + #avg_speed_offshore +
                                          distanceOutBay, 
                                        family = poisson, 
                                        data = raw.data)

step(model_poisson_D_deadOnLive_count)
step_D_deadOnLive <- glm(formula = D_deadOnLive ~ dead_coral_total + macroalgae_total + 
                           depth_to_benthos + percent_N_may, family = poisson, data = raw.data)
summary(step_D_deadOnLive)


## checks which variables should be removed for better fitting model


model_poisson_D_deadOnDead_count <- glm(D_deadOnDead ~ live_coral_total + dead_coral_total + turf_total +
                                          macroalgae_total + depth_to_benthos + 
                                          percent_N_dry + percent_N_may + #percent_N_aug + 
                                          #jan_d15 + may_d15 + aug_d15 + 
                                          avg_speed + #avg_speed_offshore +
                                          distanceOutBay, 
                                        family = poisson, 
                                        data = raw.data)

step(model_poisson_D_deadOnDead_count)
step_D_deadOnDead <- glm(formula = D_deadOnDead ~ live_coral_total + dead_coral_total + 
                           turf_total + macroalgae_total + percent_N_may + avg_speed + 
                           distanceOutBay, family = poisson, data = raw.data)
summary(step_D_deadOnDead)
## checks which variables should be removed for better fitting model
############################ Boring diameter GLMs
library(lme4)
library(tidyverse)
diam.data <- read.csv("data/2024_boringData_masterSheet - diameter_data.csv")
clean.data <- diam.data %>% select(c(14:20, 33, 40, 41, 56:74))


#hist(clean.data$L_onLive_diameter)
#qqnorm(clean.data$L_onLive_diameter, main='Normal')
#qqline(clean.data$L_onLive_diameter)
#shapiro.test(clean.data$L_onLive_diameter)

#hist(clean.data$L_onDead_diameter)
#qqnorm(clean.data$L_onDead_diameter, main='Normal')
#qqline(clean.data$L_onDead_diameter)
#shapiro.test(clean.data$L_onDead_diameter)

#hist(clean.data$D_onLive_diameter)
#qqnorm(clean.data$D_onLive_diameter, main='Normal')
#qqline(clean.data$D_onLive_diameter)
#shapiro.test(clean.data$D_onLive_diameter)

#hist(clean.data$D_onDead_diameter)
#qqnorm(clean.data$D_onDead_diameter, main='Normal')
#qqline(clean.data$D_onDead_diameter)
#shapiro.test(clean.data$D_onDead_diameter)

## Live coral diameters are not normal, dead coral diameters are :(

z.score <- function(x) {
  centered <- x - mean(x)
  scaled <- centered/sd(centered)
  return(scaled)
}

z.data.clean <- clean.data %>% mutate(live_coral_z = z.score(live_coral_total),
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


model_L_onLive <- glm(L_onLive_diameter ~ 
                        live_coral_z + dead_coral_z + turf_z +
                        macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                        percent_N_jan_z + percent_N_may_z + percent_N_aug_z + 
                        jan_d15_z + may_d15_z + aug_d15_z + 
                        NHW_speed_z + NLW_speed_z + ELW_speed_z +
                        NHW_speed_offshore_z + NLW_speed_offshore_z + ELW_speed_offshore_z
                      #avg_speed_z + avg_speed_offshore_z +
                      #+distanceOutBay_z
                      , 
                      family = gaussian, 
                      data = z.data.clean)

step(model_L_onLive)

step_L_onLive <- glm(formula = L_onLive_diameter ~ dead_coral_z + turf_z + invertebrate_z + 
                       percent_N_may_z + jan_d15_z + may_d15_z + aug_d15_z + NHW_speed_z + 
                       NLW_speed_z, family = gaussian, data = z.data.clean)
summary(step_L_onLive)
write.csv(summary.glm(step_L_onLive)$coefficients, "L_onLive_diam_coefficients.csv")

###############
model_L_onDead <- glm(L_onDead_diameter ~ 
                        live_coral_z + dead_coral_z + turf_z +
                        macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                        percent_N_jan_z + percent_N_may_z + percent_N_aug_z + 
                        jan_d15_z + may_d15_z + aug_d15_z + 
                        NHW_speed_z + NLW_speed_z + ELW_speed_z +
                        NHW_speed_offshore_z + NLW_speed_offshore_z + ELW_speed_offshore_z
                      #avg_speed_z + avg_speed_offshore_z +
                      #+distanceOutBay_z
                      , 
                      family = gaussian, 
                      data = z.data.clean)

step(model_L_onDead)

step_L_onDead <- glm(formula = L_onDead_diameter ~ dead_coral_z + turf_z + macroalgae_z + 
                       rock_z + depth_to_benthos_z + percent_N_jan_z + percent_N_aug_z + 
                       may_d15_z, family = gaussian, data = z.data.clean)
summary(step_L_onDead)
write.csv(summary.glm(step_L_onDead)$coefficients, "L_onDead_diam_coefficients.csv")

##########################
model_D_OnLive <- glm(D_onLive_diameter ~ 
                        live_coral_z + dead_coral_z + turf_z +
                        macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                        percent_N_jan_z + percent_N_may_z + percent_N_aug_z + 
                        jan_d15_z + may_d15_z + aug_d15_z + 
                        NHW_speed_z + NLW_speed_z + ELW_speed_z +
                        NHW_speed_offshore_z + NLW_speed_offshore_z + ELW_speed_offshore_z
                      #avg_speed_z + avg_speed_offshore_z +
                      #+distanceOutBay_z
                      , 
                      family = gaussian, 
                      data = z.data.clean)

step(model_D_OnLive)
step_D_onLive <- glm(formula = D_onLive_diameter ~ dead_coral_z + turf_z + macroalgae_z + 
                       rock_z + invertebrate_z + percent_N_jan_z + percent_N_may_z + 
                       jan_d15_z + may_d15_z, family = gaussian, data = z.data.clean)
summary(step_D_onLive)
write.csv(summary.glm(step_D_onLive)$coefficients, "D_onLive_diam_coefficients.csv")

#########################
model_D_OnDead <- glm(D_onDead_diameter ~ 
                        live_coral_z + dead_coral_z + turf_z +
                        macroalgae_z + rock_z + invertebrate_z + depth_to_benthos_z + 
                        percent_N_jan_z + percent_N_may_z + percent_N_aug_z + 
                        jan_d15_z + may_d15_z + aug_d15_z + 
                        NHW_speed_z + NLW_speed_z + ELW_speed_z +
                        NHW_speed_offshore_z + NLW_speed_offshore_z + ELW_speed_offshore_z
                      #avg_speed_z + avg_speed_offshore_z +
                      #+distanceOutBay_z
                      , 
                      family = gaussian, 
                      data = z.data.clean)

step(model_D_OnDead)
step_D_onDead <- glm(formula = D_onDead_diameter ~ live_coral_z + dead_coral_z + 
                       turf_z + depth_to_benthos_z + percent_N_jan_z + may_d15_z + 
                       NHW_speed_z, family = gaussian, data = z.data.clean)
summary(step_D_onDead)
write.csv(summary.glm(step_D_onDead)$coefficients, "D_onDead_diam_coefficients.csv")

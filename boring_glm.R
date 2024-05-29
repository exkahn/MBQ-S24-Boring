############## Data analysis GLM for MBQ 2024 Boring Project ###################

library(lme4)
library(tidyverse)

raw.data <- read.csv("data/2024_boringData_masterSheet.csv")

hist(raw.data$density_L_onLive)
hist(raw.data$density_L_onDead)
hist(raw.data$density_D_liveOnLive)
hist(raw.data$density_D_liveOnDead)
hist(raw.data$density_D_deadOnLive)
hist(raw.data$density_D_deadOnDead)
## NOT normally distributed

shapiro.test(log(raw.data$L_onLive + 1))
## Distribution is NOT Gaussian

## Decided to use GLM with point counts (Poisson distribution), because ratios were not neatly distributed
model_poisson_L_onLive <- glm(L_onLive ~ number_total_points +
               proportion_live_coral + proportion_dead_coral + proportion_turf +
               proportion_macroalgae + proportion_rock + proportion_invertebrate + height + length +
               width + circumference + depth + depth_to_benthos + density_D_liveOnLive + density_D_deadOnLive +
                 density_D_liveOnDead + density_D_deadOnDead + density_tridacna + density_spirobranchus +
               density_total_live_D + density_total_D + percent_N_jan + percent_N_may + percent_N_aug + 
                 jan_d15 + may_d15 + aug_d15 + avg_speed +
                 altDistance + distanceOutBay, 
             family = poisson, 
             data = raw.data)


step(model_poisson_L_onLive)
## checks which variables should be removed for better fitting model
library(tidyverse)
setwd("C:\\Users\\ethan\\Documents\\MBQ")
MCB_raw <- read.csv("data/2024_04_23_middleCooksBay_boringData_clean - cover_and_dimensions_raw.csv")

#summing the number of top and side quadrats and making columns for them
sum_quadrats <- MCB_raw %>% 
  group_by(bommie_number) %>%
  summarise(
    number_top_quadrats = sum(top_or_side == "top"),
    number_side_quadrats = sum(top_or_side == "side")
    ) 

#summing total numbers in each category (live coral, dead coral, turf, etc)
sum_totals <- MCB_raw %>% 
  group_by(bommie_number) %>%
  mutate(across(c(live_coral, dead_coral, turf, macroalgae, rock, invertebrate, water), sum, .names = "{.col}_total")) %>% 
  select(-top_or_side, -coralSpecies, -live_coral, -dead_coral, -turf, -macroalgae, -rock, -invertebrate, -water) %>% 
  distinct()

#making top and side columns 
top_and_side <- MCB_raw %>% 
  group_by(bommie_number) %>%
  mutate(live_coral_top = if_else(top_or_side == "top", 
                                  sum(live_coral[top_or_side == "top"]), NA)) %>% 
  mutate(dead_coral_top = if_else(top_or_side == "top", 
                                  sum(dead_coral[top_or_side == "top"]), NA)) %>% 
  mutate(turf_top = if_else(top_or_side == "top", 
                                  sum(turf[top_or_side == "top"]), NA)) %>% 
  mutate(macroalgae_top = if_else(top_or_side == "top", 
                            sum(macroalgae[top_or_side == "top"]), NA)) %>% 
  mutate(rock_top = if_else(top_or_side == "top", 
                                  sum(rock[top_or_side == "top"]), NA)) %>%
  mutate(invertebrate_top = if_else(top_or_side == "top", 
                            sum(invertebrate[top_or_side == "top"]), NA)) %>%
  mutate(water_top = if_else(top_or_side == "top", 
                            sum(water[top_or_side == "top"]), NA)) %>% 
  mutate(live_coral_side = if_else(top_or_side == "side", 
                                  sum(live_coral[top_or_side == "side"]), NA)) %>% 
  mutate(dead_coral_side = if_else(top_or_side == "side", 
                                  sum(dead_coral[top_or_side == "side"]), NA)) %>% 
  mutate(turf_side = if_else(top_or_side == "side", 
                            sum(turf[top_or_side == "side"]), NA)) %>% 
  mutate(macroalgae_side = if_else(top_or_side == "side", 
                                  sum(macroalgae[top_or_side == "side"]), NA)) %>% 
  mutate(rock_side = if_else(top_or_side == "side", 
                            sum(rock[top_or_side == "side"]), NA)) %>%
  mutate(invertebrate_side = if_else(top_or_side == "side", 
                                    sum(invertebrate[top_or_side == "side"]), NA)) %>%
  mutate(water_side = if_else(top_or_side == "side", 
                             sum(water[top_or_side == "side"]), NA)) %>% 
  select(-top_or_side, -coralSpecies, -live_coral, -dead_coral, -turf, -macroalgae, -rock, -invertebrate, -water)

#making one row for each bommie
top_and_side_one_row <- top_and_side %>% 
  distinct() %>% 
  group_by(bommie_number) %>%
  summarise(across(22:35, ~ sum(., na.rm = TRUE)))

#adding everything together
clean_test <- full_join(sum_quadrats, top_and_side_one_row) %>% 
  full_join(sum_totals)

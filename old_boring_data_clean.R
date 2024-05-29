## DIDN'T REALLY WORK GO WITH ZOE'S

library(dplyr)

setwd("C:/Users/ethan/Documents/MBQ")
bommie_raw <- read.csv("data/2024_04_24_reefFlat_boringData_clean - cover_and_dimensions.csv")

bommie_nums <- 20

bommie_working <- bommie_raw
quadrat_sums_test <- bommie_raw %>%
  #filter(top_or_side == "top") %>%
  group_by(bommie_number)# %>%

#ifelse(bommie_working$top_or_side == "top", 
       yes = bommie_working %>% 
         group_by(bommie_number) %>% 
         summarize(live_coral_top = sum(live_coral), dead_coral_top = sum(dead_coral), turf_top = sum(turf), 
            macroalgae_top = sum(macroalgae), rock_top = sum(rock), invertebrate_top = sum(invertebrate),
            water_top = sum(water)), 
       no = NA)


quadrat_sums_top <- bommie_raw %>%
  filter(top_or_side == "top") %>%
  group_by(bommie_number) %>%
  summarize(live_coral_top = sum(live_coral), dead_coral_top = sum(dead_coral), turf_top = sum(turf), 
            macroalgae_top = sum(macroalgae), rock_top = sum(rock), invertebrate_top = sum(invertebrate),
            water_top = sum(water))

quadrat_sums_side <- bommie_raw %>%
  filter(top_or_side == "side") %>%
  group_by(bommie_number) %>%
  summarize(live_coral_side = sum(live_coral), dead_coral_side = sum(dead_coral), turf_side = sum(turf), 
            macroalgae_side = sum(macroalgae), rock_side = sum(rock), invertebrate_side = sum(invertebrate),
            water_side = sum(water))


top_and_side <- data.frame(matrix(nrow = 1:bommie_nums, ncol = 1 + (2*ncol(quadrat_sums_top))))


for (i in 1:bommie_nums) {
  top_and_side[i,1] <- (1:bommie_nums)[i]
}

for (i in 1:bommie_nums) {
  holder_row <-
    if(i %in% quadrat_sums_top$bommie_number) {
      quadrat_sums_top[quadrat_sums_top$bommie_number == i,]
    } else{
      no = data.frame(0, 0, 0, 0, 0, 0, 0, 0)
}
  
  top_and_side[i,] <- 
    cbind(top_and_side[top_and_side$X1 == i,1], holder_row)
          #quadrat_sums_side[quadrat_sums_side$bommie_number == i,])
}

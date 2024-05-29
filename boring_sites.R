library(tidyverse)

num_bommies <- 20
all_coords <- data.frame(matrix(NA, nrow = num_bommies, ncol = 2))

rm(.Random.seed, envir=globalenv())

for(i in 1:num_bommies){
  rand_y <- sample(-5:5, size = 1)
  rand_x <- sample(1:100, size = 1)
  coords <- data.frame(rand_x, rand_y)
  
  all_coords[i,1] <- rand_x
  all_coords[i,2] <- rand_y
}

if(dim(all_coords %>% distinct())[1] != 20) {stop()}
## If it stops here, there is a duplicate coordinate pair -- simply re-run from the top.

ordered_by_y <- all_coords %>% 
  arrange(X2)


pairs <- c(rep("a", times = 10), rep("b", times = 10))

ordered_by_teams <- cbind(ordered_by_y, pairs)

ordered_final <- ordered_by_teams %>%
  group_by(pairs) %>%
  arrange(X1, .by_group = TRUE)

write.csv(ordered_final, "C:\\Users\\ethan\\Documents\\MBQ\\coords10.csv")

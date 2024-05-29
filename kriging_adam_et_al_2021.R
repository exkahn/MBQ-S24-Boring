## Attempting to replicate nutrient maps from Adam et al. 2021
library(kriging)
library(tidyverse)

options(digits = 9)
setwd("C:/Users/ethan/Documents/MBQ")

nutrient.data <- read.csv("data/MCR_LTER_Adam_EcolAp_N_summary_20200331.csv")

coords <- read.csv("boring site coordinates.csv")

x_values <- coords$lon_avg
y_values <- coords$lat_avg

jan <- nutrient.data %>% dplyr::select(c(Latitude, Longitude, Percent_N_Jan)) %>% na.omit()
jan.clean <- kriging(x = jan$Longitude, y = jan$Latitude, response = jan$Percent_N_Jan)


kriged_coords <- data.frame(matrix(NA, nrow = 9, ncol = 2))
colnames(kriged_coords) <- c("x","y")

for (i in 1:length(x_values)) {
  point_x <- unique(jan.clean$map$x)[which.min(abs(x_values[i] - unique(jan.clean$map$x)))]
  kriged_coords[i, 1] <- point_x
}

for (i in 1:length(y_values)) {
  point_y <- unique(jan.clean$map$y)[which.min(abs(y_values[i] - unique(jan.clean$map$y)))]
  kriged_coords[i, 2] <- point_y
}

kriged_jan_N <- kriged_coords

for (i in 1:dim(kriged_coords)[1]) {
  kriged_jan_N[i,3] <- jan.clean$map$pred[(jan.clean$map$x == kriged_jan_N[i,1])&(jan.clean$map$y == kriged_jan_N[i,2])]
}
colnames(kriged_jan_N) <- c("lon", "lat", "percent_N_jan")


########## percent_N_may
may <- nutrient.data %>% dplyr::select(c(Latitude, Longitude, Percent_N_May)) %>% na.omit()
may.clean <- kriging(x = may$Longitude, y = may$Latitude, response = may$Percent_N_May)


for (i in 1:length(x_values)) {
  point_x <- unique(may.clean$map$x)[which.min(abs(x_values[i] - unique(may.clean$map$x)))]
  kriged_coords[i, 1] <- point_x
}

for (i in 1:length(y_values)) {
  point_y <- unique(may.clean$map$y)[which.min(abs(y_values[i] - unique(may.clean$map$y)))]
  kriged_coords[i, 2] <- point_y
}

kriged_may_N <- kriged_coords

for (i in 1:dim(kriged_coords)[1]) {
  kriged_may_N[i,3] <- may.clean$map$pred[(may.clean$map$x == kriged_may_N[i,1])&(may.clean$map$y == kriged_may_N[i,2])]
}
colnames(kriged_may_N) <- c("lon", "lat", "percent_N_may")


######## percent_N_july
july <- nutrient.data %>% dplyr::select(c(Latitude, Longitude, Percent_N_July)) %>% na.omit()
july.clean <- kriging(x = july$Longitude, y = july$Latitude, response = july$Percent_N_July)


for (i in 1:length(x_values)) {
  point_x <- unique(july.clean$map$x)[which.min(abs(x_values[i] - unique(july.clean$map$x)))]
  kriged_coords[i, 1] <- point_x
}

for (i in 1:length(y_values)) {
  point_y <- unique(july.clean$map$y)[which.min(abs(y_values[i] - unique(july.clean$map$y)))]
  kriged_coords[i, 2] <- point_y
}

kriged_july_N <- kriged_coords

for (i in 1:dim(kriged_coords)[1]) {
  kriged_july_N[i,3] <- july.clean$map$pred[(july.clean$map$x == kriged_july_N[i,1])&(july.clean$map$y == kriged_july_N[i,2])]
}
colnames(kriged_july_N) <- c("lon", "lat", "percent_N_july")


######## d15NNorm_Jan
jand15 <- nutrient.data %>% dplyr::select(c(Latitude, Longitude, d15NNorm_Jan)) %>% na.omit()
jand15.clean <- kriging(x = jand15$Longitude, y = jand15$Latitude, response = jand15$d15NNorm_Jan)


for (i in 1:length(x_values)) {
  point_x <- unique(jand15.clean$map$x)[which.min(abs(x_values[i] - unique(jand15.clean$map$x)))]
  kriged_coords[i, 1] <- point_x
}

for (i in 1:length(y_values)) {
  point_y <- unique(jand15.clean$map$y)[which.min(abs(y_values[i] - unique(jand15.clean$map$y)))]
  kriged_coords[i, 2] <- point_y
}

kriged_jand15 <- kriged_coords

for (i in 1:dim(kriged_coords)[1]) {
  kriged_jand15[i,3] <- jand15.clean$map$pred[(jand15.clean$map$x == kriged_jand15[i,1])&(jand15.clean$map$y == kriged_jand15[i,2])]
}
colnames(kriged_jand15) <- c("lon", "lat", "jan_d15")



######## d15NNorm_May
mayd15 <- nutrient.data %>% dplyr::select(c(Latitude, Longitude, d15NNorm_May)) %>% na.omit()
mayd15.clean <- kriging(x = mayd15$Longitude, y = mayd15$Latitude, response = mayd15$d15NNorm_May)


for (i in 1:length(x_values)) {
  point_x <- unique(mayd15.clean$map$x)[which.min(abs(x_values[i] - unique(mayd15.clean$map$x)))]
  kriged_coords[i, 1] <- point_x
}

for (i in 1:length(y_values)) {
  point_y <- unique(mayd15.clean$map$y)[which.min(abs(y_values[i] - unique(mayd15.clean$map$y)))]
  kriged_coords[i, 2] <- point_y
}

kriged_mayd15 <- kriged_coords

for (i in 1:dim(kriged_coords)[1]) {
  kriged_mayd15[i,3] <- mayd15.clean$map$pred[(mayd15.clean$map$x == kriged_mayd15[i,1])&(mayd15.clean$map$y == kriged_mayd15[i,2])]
}
colnames(kriged_mayd15) <- c("lon", "lat", "may_d15")


######## d15NNorm_July
julyd15 <- nutrient.data %>% dplyr::select(c(Latitude, Longitude, d15NNorm_July)) %>% na.omit()
julyd15.clean <- kriging(x = julyd15$Longitude, y = julyd15$Latitude, response = julyd15$d15NNorm_July)


for (i in 1:length(x_values)) {
  point_x <- unique(julyd15.clean$map$x)[which.min(abs(x_values[i] - unique(julyd15.clean$map$x)))]
  kriged_coords[i, 1] <- point_x
}

for (i in 1:length(y_values)) {
  point_y <- unique(julyd15.clean$map$y)[which.min(abs(y_values[i] - unique(julyd15.clean$map$y)))]
  kriged_coords[i, 2] <- point_y
}

kriged_julyd15 <- kriged_coords

for (i in 1:dim(kriged_coords)[1]) {
  kriged_julyd15[i,3] <- julyd15.clean$map$pred[(julyd15.clean$map$x == kriged_julyd15[i,1])&(julyd15.clean$map$y == kriged_julyd15[i,2])]
}
colnames(kriged_julyd15) <- c("lon", "lat", "july_d15")




full.output <- kriged_jan_N %>% full_join(kriged_may_N) %>%
  full_join(kriged_july_N) %>% full_join(kriged_jand15) %>%
  full_join(kriged_mayd15) %>% full_join(kriged_julyd15)


write.csv(full.output, "test_adam_data.csv")

#######################################################
################ Also trying data from Disa et al. 2024.
library(terra)
library (tidyverse)
library(raster)
setwd("C:/Users/ethan/Documents/MBQ")

velocity.datau <- brick("data/fig4a_NHW_u_v.nc", varname = "u")


velocity.datav <- brick("data/fig4a_NHW_u_v.nc", level = 2, varname = "v")
#day 3 starts at X174600, layer 49/440
#day 16 ends at 1384200, last layer before is layer 384/440

day2to16 <- raster::subset(velocity.data, subset = 49:384)


library(ncdf4)
options(digits = 9)
setwd("C:/Users/ethan/Documents/MBQ")
nctest <- nc_open("data/fig4a_NHW_u_v.nc")
print(nctest)
lon.u <- as.list(ncvar_get(nctest, varid = "lon_u"))
lat.u <- as.list(ncvar_get(nctest, varid = "lat_u"))
val.u <- as.list(ncvar_get(nctest, varid = "u"))



layers_num <- seq(from = 551, to = 4330, by = 10)
name <- paste0('"v@', layers_num, '" + ')

sink("qgis_layers.txt")
cat(name)
sink()

file.show("qgis_layers.txt")

#####################################################################
#### Principal Component Regression for MBQ 2024 Boring Project #####
library(lme4)
library(tidyverse)
library(MASS)
library(pscl)
library(topmodels) # to check fit of models (must install from RForge)

raw.data.full <- read.csv("data/2024_boringData_masterSheet - complete_data(1).csv")
raw.data.no.diameters <- raw.data.full %>% dplyr::select(2:70)
    #removes diameters, collector info, notes
raw.data.comb.dead <- raw.data.no.diameters %>% dplyr::mutate(not_alive_coral = dead_coral_total + turf_total + macroalgae_total + invertebrate_total)
raw.data <- raw.data.comb.dead %>% dplyr::select(-(44:54)) %>% na.omit
    #removes density calculations, which are messy
    #removes one row where we did not measure a bommie dimension


#### Model choice (data are overdispersed and count data, so either quasipoisson or negative binomial) ####
## Per Ver Hoef & Boveng 2007, quadratic relationship of variance to mean, so I should use negative binomial!
## Correlation matrix shows strong correlations of some predictors
      # In some cases, unclear which variable to choose
      # Opt for PCA to reduce strongly correlated variables
## Rootogram suggests zero-inflated negative binomial is better fit than plain NB

# See appendix for testing of these assumptions


## Decided to use up to PC13 (the last PC which explains > 1.0% of variance)
## PC1-6 have eigenvalues > 1.0




#####################################################################################################
###### Apply Principal Coordinate analysis to reduce dimensionality (i.e. reduce collinear variables)
pcr_input_predictors <- raw.data %>% dplyr::select(c(13:16,19, 28:32, 39, 40, 44:59))

pcr <- prcomp(pcr_input_predictors, center = TRUE, scale = TRUE)
pcr$rotation
  #PC1 is loading on water speed (esp NLW) and distance out of the bay; opposite d15 (esp. May)
  #PC2 is loading on bommie size (ln, wd, circ, total points, not alive coral)
  #PC3 is loading on nitrogen (esp May) and offshore speed; opposite spirobranchus
  #PC4 is loading on dead coral, not alive coral, depth; opposite live coral (a lot!) and May nitrogen
  #PC5 is loading on depth and Aug nitrogen (a lot); opposite Jan nitrogen
  #PC6 is loading on dead coral, height, Jan nitrogen; opposite turf
  #PC7 is loading on dead coral, tridacna, May nitrogen
  #PC8 is loading on tridacna a lot!; opposite dead coral
  #PC9 is loading on depth a lot!; opposite height, spirobranchus
  #PC10 is loading on macroalgae a lot!; opposite turf, spirobranchus
  #PC11 is loading on height; opposite spirobranchus a lot!
  #PC12 is loading on depth, May nitrogen
  #PC13 is loading opposite height, January d15

#write.csv(pcr$rotation, "output/pca_loadings.csv")

pcr$x
  #this is where the PCA results for each row are
biplot(pcr, scale = 0)

#calculate total variance explained by each principal component
var_explained <- pcr$sdev^2 / sum(pcr$sdev^2)
  #PC1 explains ~40% of the variance
  #PC2 explains ~14% of the variance
  #PC13 is the last PC to explain >1%
plot(var_explained)

var_per_pc <- cbind(var_explained, colnames(pcr$x))
colnames(var_per_pc) <- c("var_explained", "PC")
#write.csv(var_per_pc, "output/pca_variance_explained.csv")

library(Hmisc)
rcorr(as.matrix(data_with_pca[,c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "L_onLive")]))
  # As it should be, the PCs are not correlated with each other





##############################################
##### Negative Binomial GLMs #################
data_with_pca <- cbind(raw.data, pcr$x)
#write.csv(data_with_pca, "output/boring_data_withPrincipalComponents.csv")

#Stop at PC8 because the model doesn't behave properly if I add PC9 or higher (alternation limit reached for D_liveOnLive) or PC10 (other issue for L_onLive)
#Zero-inflated NB didn't seem to provide any benefit
# Rootograms like not great, but I don't think I can improve on it
    # Generally show that models slightly overpredict low counts (~1:5), and underpredict higher counts (especially tail end past 20)


### L_onLive
pca_L_onLive_nb <- 
  zeroinfl(data = data_with_pca, dist = "negbin", 
           L_onLive ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12)
      # Remove PC13 to prevent 'system is computationally singular' warning
summary(pca_L_onLive_nb)
L_onLive_nb_root <- rootogram(pca_L_onLive_nb, style = "hanging", plot = FALSE)
  # 0 and 1 count well-fitted, 3-8 overestimated, beyond that mostly underestimated


### L_onDead
pca_L_onDead_nb <- 
  zeroinfl(data = data_with_pca, dist = "negbin", 
           L_onDead ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13)
summary(pca_L_onDead_nb)
L_onDead_nb_root <- rootogram(pca_L_onDead_nb, style = "hanging", plot = FALSE)
  # 0 and 2 count well-fitted, 1 underestimated, beyond 20 mostly underestimated


### D_liveOnLive
pca_D_liveOnLive_nb <- 
  zeroinfl(data = data_with_pca, dist = "negbin", 
           D_liveOnLive ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11)
  # Remove PC12 and PC13 to prevent 'system is computationally singular' warning
summary(pca_D_liveOnLive_nb)
D_liveOnLive_nb_root <- rootogram(pca_D_liveOnLive_nb, style = "hanging", plot = FALSE)
  # 0 and 2-6 and 8-11 count well-fitted, 1 overestimated, tail beyond 19 underestimated


### D_liveOnDead
pca_D_liveOnDead_nb <- 
  zeroinfl(data = data_with_pca, dist = "negbin", 
           D_liveOnDead ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13)
summary(pca_D_liveOnDead_nb)
D_liveOnDead_nb_root <- rootogram(pca_D_liveOnDead_nb, style = "hanging", plot = FALSE)
  # 0-1 and 4 and 6-9 well-estimated, 2-3 overestimated, 5 underestimated, tail beyond 10 underestimated


### D_deadOnLive
pca_D_deadOnLive_nb <- 
  zeroinfl(data = data_with_pca, dist = "negbin", 
           D_deadOnLive ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13)
summary(pca_D_deadOnLive_nb)
D_deadOnLive_nb_root <- rootogram(pca_D_deadOnLive_nb, style = "hanging", plot = FALSE)
  # 0 and 2 well-estimated, 1 underestimated, tail beyond 10 underestimated


### D_deadOnDead
pca_D_deadOnDead_nb <- 
  zeroinfl(data = data_with_pca, dist = "negbin", 
           D_deadOnDead ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13)
summary(pca_D_deadOnDead_nb)
D_deadOnDead_nb_root <- rootogram(pca_D_deadOnDead_nb, style = "hanging", plot = FALSE)
  # Zero-inflation is very beneficial here. 0-9 well estimated except 2 and 7 overestimated, tail beyond 10 mostly underestimated.






#########################################
##### Appendix A ########################
##### Test for overdispersion (mostly useful for showing Poisson model assumption of variance=mean is violated)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}


###################################################
###### Appendix B #################################
# Testing for quadratic relationship of variance to mean
# Alongside overdispersion and count data, justifies chioce of negative binomial
# Also demonstrate need for PCA, due to highly correlated predictors variables
hist(raw.data$density_L_onLive)
hist(raw.data$density_L_onDead)
hist(raw.data$density_D_liveOnLive)
hist(raw.data$density_D_liveOnDead)
hist(raw.data$density_D_deadOnLive)
hist(raw.data$density_D_deadOnDead)
# NOT normally distributed

shapiro.test(log(raw.data$L_onLive + 1))
# Distribution is NOT Gaussian, even with log transform


#L_onLive
L_onLive_var <- (raw.data$L_onLive - mean(raw.data$L_onLive))^2
plot(L_onLive_var ~ raw.data$L_onLive)
L_onLive2 <- (raw.data$L_onLive)^2
linear_L_onLive <- lm(L_onLive_var ~ raw.data$L_onLive)
#R^2 is 0.77
quad_L_onLive <- lm(L_onLive_var ~ raw.data$L_onLive + L_onLive2)
#R^2 is 1
#Quadratic relationship --> negative binomial model over quasipoisson

#L_onDead
L_onDead_var <- (raw.data$L_onDead - mean(raw.data$L_onDead))^2
plot(L_onDead_var ~ raw.data$L_onDead)
L_onDead2 <- (raw.data$L_onDead)^2
linear_L_onDead <- lm(L_onDead_var ~ raw.data$L_onDead)
summary(linear_L_onDead)
#R^2 is 0.81
quad_L_onDead <- lm(L_onDead_var ~ raw.data$L_onDead + L_onDead2)
summary(quad_L_onDead)
#R^2 is 1
#Quadratic relationship --> negative binomial model

#D_liveOnLive
D_liveOnLive_var <- (raw.data$D_liveOnLive - mean(raw.data$D_liveOnLive))^2
plot(D_liveOnLive_var ~ raw.data$D_liveOnLive)
D_liveOnLive2 <- (raw.data$D_liveOnLive)^2
linear_D_liveOnLive <- lm(D_liveOnLive_var ~ raw.data$D_liveOnLive)
summary(linear_D_liveOnLive)
#R^2 is 0.74
quad_D_liveOnLive <- lm(D_liveOnLive_var ~ raw.data$D_liveOnLive + D_liveOnLive2)
summary(quad_D_liveOnLive)
#R^2 is 1
#Quadratic relationship --> negative binomial model

#D_liveOnDead
D_liveOnDead_var <- (raw.data$D_liveOnDead - mean(raw.data$D_liveOnDead))^2
plot(D_liveOnDead_var ~ raw.data$D_liveOnDead)
D_liveOnDead2 <- (raw.data$D_liveOnDead)^2
linear_D_liveOnDead <- lm(D_liveOnDead_var ~ raw.data$D_liveOnDead)
summary(linear_D_liveOnDead)
#R^2 is 0.89
quad_D_liveOnDead <- lm(D_liveOnDead_var ~ raw.data$D_liveOnDead + D_liveOnDead2)
summary(quad_D_liveOnDead)
#R^2 is 1
#Quadratic relationship --> negative binomial model

#D_deadOnLive
D_deadOnLive_var <- (raw.data$D_deadOnLive - mean(raw.data$D_deadOnLive))^2
plot(D_deadOnLive_var ~ raw.data$D_deadOnLive)
D_deadOnLive2 <- (raw.data$D_deadOnLive)^2
linear_D_deadOnLive <- lm(D_deadOnLive_var ~ raw.data$D_deadOnLive)
summary(linear_D_deadOnLive)
#R^2 is 0.90
quad_D_deadOnLive <- lm(D_deadOnLive_var ~ raw.data$D_deadOnLive + D_deadOnLive2)
summary(quad_D_deadOnLive)
#R^2 is 1
#Quadratic relationship --> negative binomial model

#D_deadOnDead
D_deadOnDead_var <- (raw.data$D_deadOnDead - mean(raw.data$D_deadOnDead))^2
plot(D_deadOnDead_var ~ raw.data$D_deadOnDead)
D_deadOnDead2 <- (raw.data$D_deadOnDead)^2
linear_D_deadOnDead <- lm(D_deadOnDead_var ~ raw.data$D_deadOnDead)
summary(linear_D_deadOnDead)
#R^2 is 0.73
quad_D_deadOnDead <- lm(D_deadOnDead_var ~ raw.data$D_deadOnDead + D_deadOnDead2)
summary(quad_D_deadOnDead)
#R^2 is 1
#Quadratic relationship --> negative binomial model


######## Looking at correlation matrix of predictors
library(Hmisc)
rcorr(as.matrix(raw.data[,c(59:62, 66:75, 81)]))
##### SHOWS I NEED TO SIMPLIFY MODEL
############################################
#### Analytical Model & Rates of Change ####
############################################

# Packages ----
library(tidyverse)
library(patchwork)
theme_set(theme_classic())
nice_colors <- c("#ADDFB3",
                 "#4A5E4C",
                 "#916C63",
                 "#8F8091",
                 "#DEAEA2",
                 "#D8B8DE",
                 "#7B9E7F")

# Parameters ----

# Baseline fertility for age-class 4 
b4 <- 8

# Fertilities 
Fs <- c(0, 0, 0, b4, 0)

# Survival Probabilities
S <- c(0.9,0.8,0.6,0.5,0.2)

# Initial population 
pop_0 <- c(20, 20, 5, 5, 0)

# Number of time-steps
num_timesteps <- 60

# Initial proportion of the trait T 
T_0 <- c(0,rep(0.01,4))

# Fertility Bonus for having T 
w <- 3

# Probabilities of learning such that they decrease 
prob_learns <- c(0.99, 0.5, 0.3, 0.1, 0.05)

# Set-Up ----
# Leslie Matrix 
L <- matrix(c(Fs, 
              c(S[1],rep(0,4)),
              c(0, S[2], rep(0,3)), 
              c(rep(0,2), S[3], rep(0,2)), 
              c(rep(0,3), S[4], S[5])),
            byrow = T, 
            ncol = 5, 
            nrow = 5)

# Dataframe to store populations
resulting_populations <- tibble(
  timestep = 1:num_timesteps, 
  pop_age_class1 = rep(NA_real_, num_timesteps), 
  pop_age_class2 = rep(NA_real_, num_timesteps), 
  pop_age_class3 = rep(NA_real_, num_timesteps), 
  pop_age_class4 = rep(NA_real_, num_timesteps),
  pop_age_class5 = rep(NA_real_, num_timesteps)
)

# Dataframe to store the proportions
resulting_proportions <- tibble(
  timestep = 1:num_timesteps, 
  prop_age_class1 = rep(NA_real_, num_timesteps), 
  prop_age_class2 = rep(NA_real_, num_timesteps), 
  prop_age_class3 = rep(NA_real_, num_timesteps), 
  prop_age_class4 = rep(NA_real_, num_timesteps),
  prop_age_class5 = rep(NA_real_, num_timesteps)
)

# Record initial population 
resulting_populations[1,2:6] <- t(pop_0)

# Record initial proportions 
resulting_proportions[1,2:6] <- t(T_0)

# Reproduction
for (t in 2:num_timesteps) {
  
  # Updated fertility for class 4
  L[1,4] <- as.numeric((b4 + w)*resulting_proportions[t-1,5] + b4 * (1 - resulting_proportions[t-1,5]))
  
  # Calculate new populations
  resulting_populations[t,2:6] <- t(L %*% t(resulting_populations[t-1,2:6]))
  
  # New proportions for newborns 
  resulting_proportions[t,2] <- ((b4 + w) * as.numeric(resulting_proportions[t-1,5]) / ((b4 + w) * as.numeric(resulting_proportions[t-1,5]) +
    b4 * (1 - as.numeric(resulting_proportions[t-1,5])))) * prob_learns[1]
  
  # New proportions for other age groups 
  for (a in 2:5) {
    if (sum(as.numeric(resulting_populations[t-1,(a+1):6])) == 0) {
      learn_prop_T <- 0
    } else {
    learn_prop_T <- sum(as.numeric(resulting_populations[t-1,(a+1):6]) * as.numeric(resulting_proportions[t-1,(a+1):6])) / sum(as.numeric(resulting_populations[t-1,(a+1):6])) 
    }
    
    resulting_proportions[t,(a+1)] <- as.numeric(resulting_proportions[t-1,
                                                            (a)]) + ((1 - as.numeric(resulting_proportions[t-1,
                                                                                                            (a)])) * (learn_prop_T * prob_learns[a]))
  }

}

total_df <- left_join(resulting_proportions,
                  resulting_populations, 
                  by = "timestep")

weighted_prop <- vector(mode = "numeric", length = num_timesteps)

for (k in 1:num_timesteps) {
weighted_prop[k] <- sum(as.numeric(total_df[k,7:11]) * as.numeric(total_df[k,2:6])) / sum(as.numeric(total_df[k,7:11]))
}

total_df$weighted_prop <- weighted_prop

p1 <- total_df %>% 
  ggplot( 
    aes( x = timestep, 
         y = weighted_prop)) +
  geom_line()

p1
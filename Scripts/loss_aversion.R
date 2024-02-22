######################################
#### Script for loss-aversion ABM ####
######################################

# Packages ----
library(tidyverse)
theme_set(theme_classic())

# Enviroment (Set-up) ----
# We need a cost function for the policy
cost_function <- function(t,p) {
  cost <- 0.2 + 1.3*p + 0.2*t
  return(cost)
}

# We need a benefit function for the policy
benefit_function <- function(t,p) {
  benefit <- 0.1 + 1.5*p + -0.75*(p^2) + 0.8*t*p
}

# Let's try it 
functions_df <- expand_grid(p = seq(0, 1, length.out = 100),
                            t = c(0.1, 
                                  0.4, 
                                  0.6, 
                                  0.8))

benefits <- pmap_dbl(functions_df, benefit_function)
costs <- pmap_dbl(functions_df, cost_function)

functions_df <- functions_df %>% 
  mutate(benefits = benefits, 
         costs = costs)

functions_df %>% 
  ggplot(aes(
    x = p, 
    y = benefits
  )) +
  geom_line() +
  geom_line(data = functions_df, 
            aes(
              x = p, 
              y = costs
            ), 
            linetype = "dashed") +
  ylim(c(0,2)) +
  facet_wrap(~t)

# Okay this seems relatively tenable 

# Agents ----

# Now let's think about how this might look like in an ABM form 

N <- 50

turns <- 100

rounds <- 10

output_df <- tibble(
  turn = rep(1:turns, rounds),
  p = as.numeric(rep(NA_real_, turns * rounds)),
  round = as.factor(rep(1:rounds, each = turns))
)

# Begin the rounds
for (round in 1:rounds) {
  policy_preferences_df <- matrix(data = NA_real_,
                                  nrow = 50,
                                  ncol = 100) %>%
    as.data.frame()
  
  colnames(policy_preferences_df) <- c(1:100)
  
  agents_df <- matrix(data = NA_real_,
                      nrow = N,
                      ncol = 2) %>%
    as.data.frame()
  
  colnames(agents_df) <- c("id", "type")
  
  # We begin by assigning each agent a type at random
  # And a starting policy at random
  
  agents_df$id <- c(1:N)
  agents_df$type <- runif(N)
  policy_preferences_df[, 1] <- runif(N)
  
  # Initial average preference
  output_df[output_df$turn == 1 &
              output_df$round == round, "p"] <-
    mean(policy_preferences_df[, 1])
  
  # Build the loop for one round
  
  for (turn in 2:turns) {
    # Calculate benefits for each agent of current policy
    benefits <- map2_dbl(.x = agents_df$type,
                         .y = policy_preferences_df[, (turn - 1)],
                         benefit_function)
    
    # Calculate costs for each agent for current policy
    costs <- map2_dbl(.x = agents_df$type,
                      .y = policy_preferences_df[, (turn - 1)],
                      cost_function)
    
    # Get net benefits
    net_benefits <- benefits - costs
    
    # Pick demonstrators at random
    demonstrators <- sample(c(1:N),
                            size = N,
                            replace = T)
    
    benefits_comparison <-
      net_benefits[demonstrators] - net_benefits[1:N]
    
    learners <- which(benefits_comparison > 0)
    nonlearners <- which(benefits_comparison <= 0)
    
    policy_preferences_df[learners, turn] <-
      policy_preferences_df[, (turn - 1)][demonstrators[learners]]
    policy_preferences_df[nonlearners, turn] <-
      policy_preferences_df[nonlearners, (turn - 1)]
    
    output_df[output_df$turn == turn &
                output_df$round == round, "p"] <-
      mean(policy_preferences_df[, turn])
  }
}

# Examine the results 

pp_long <- policy_preferences_df %>% 
  mutate(id = 1:N) %>% 
  select(id, everything()) %>% 
  pivot_longer(2:101, 
               names_to = "turn", 
               values_to = "p") %>% 
  mutate(turn = as.numeric(turn))

pp_long %>% 
  group_by(turn) %>% 
  summarise(avg = median(p)) %>% 
  ggplot(
    aes(x = turn, 
        y = avg)
  ) +
  geom_line(linetype = "dashed")


output_df %>% 
  ggplot(
    aes(x = turn, 
        y = p, 
        color = round)  
  ) +
  geom_line(linetype = "dashed")

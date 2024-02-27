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
functions_df <- expand_grid(p = seq(0, 1, 
                                    length.out = 100),
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

lambda <- 0.05

b_rate <- 0.1

output_df <- tibble(
  turn = rep(1:turns, rounds),
  p = as.numeric(rep(NA_real_, turns * rounds)),
  round = as.factor(rep(1:rounds, each = turns)), 
  lambda = lambda
)

# Begin the rounds
for (round in 1:rounds) {
  
  agents_df <- matrix(data = NA_real_,
                      nrow = N,
                      ncol = (3+turns)) %>%
    as.data.frame()
  
  colnames(agents_df) <- c("id", "type", "age",1:turns)
  
  # We begin by assigning each agent a type at random
  # And a starting policy at random
  
  agents_df$id <- c(1:N)
  agents_df$type <- runif(N)
  agents_df[,4] <- runif(N)
  
  # Initial average preference
  output_df[output_df$turn == 1 &
              output_df$round == round, "p"] <-
    mean(agents_df[,4])
  
  # Then we assing age categories at random as well
  # Let's start with 10 age categories
  agents_df$age <- sample(c(1:10), 
                          size = N, 
                          replace = T)
  
  # Build the loop for one round
  
  for (turn in 2:turns) {
    
    # Calculate benefits for each agent of current policy
    benefits <- map2_dbl(.x = agents_df$type,
                         .y = agents_df[,(turn+2)],
                         benefit_function)
    
    # Calculate costs for each agent for current policy
    costs <- map2_dbl(.x = agents_df$type,
                      .y = agents_df[, (turn+2)],
                      cost_function)
    
    # Get net benefits
    net_benefits <- benefits-costs
    
    # Pick demonstrators at random
    demonstrators <- sample(c(1:nrow(agents_df)),
                            size = nrow(agents_df),
                            replace = T)
    
    # I am going to assume that the status quo policy will be the one that individuals currently hold 
    # For whom is the observed new policy lower than where they currently are?
    lower_policy <- which(agents_df[demonstrators,turn+2] < agents_df[,turn+2])
    
    # For whom is the observed new policy higher than where they currently are? 
    higher_policy <- which(agents_df[demonstrators,turn+2] >= agents_df[,turn+2])
    
    # Empty container for benefit comparison
    benefits_comparison <- vector(length = N, mode = "numeric")
    
    # Benefit comparisons for those where new policy is lower
    benefits_comparison[lower_policy] <- (net_benefits[demonstrators[lower_policy]] - net_benefits[lower_policy]) - 
      lambda * (benefits[lower_policy]-benefits[demonstrators[lower_policy]])
    
    # Benefit comparisons for those where new policy is higher 
    benefits_comparison[higher_policy] <- (net_benefits[demonstrators[higher_policy]] - net_benefits[higher_policy]) - 
      lambda * (costs[demonstrators[higher_policy]]-costs[higher_policy])
    
    # People who perceive a higher net-benefit with the observe policy learn
    # Otherwise they stay where they are 
    learners <- which(benefits_comparison > 0)
    nonlearners <- which(benefits_comparison <= 0)
    
    # Learners update
    agents_df[learners, (turn+3)] <-
      agents_df[, turn+2][demonstrators[learners]]
    
    # Non-learners stay
    agents_df[nonlearners, (turn+3)] <-
      agents_df[nonlearners, (turn+2)]
    
    # Keep track of average policy
    output_df[output_df$turn == turn &
                output_df$round == round, "p"] <-
      mean(agents_df[, (turn+2)], na.rm = T)
    
    # Now, people on the last category die 
    agents_df <- agents_df %>% 
      filter(age != 10)
    
    # And everyone else ages 1 year
    agents_df$age <- agents_df$age+1
    
    # New births 
    new_births <- round(nrow(agents_df) * b_rate)
    
    if(new_births > 0) {
    new_ids <- c((max(agents_df$id)+1):(max(agents_df$id)+(new_births)))
    # Add new members to the dataframe
    agents_df <- agents_df %>% 
      add_row(
        id = new_ids,
        type = sample(agents_df$type, size = new_births, replace = T), 
        age = 1
      )
    
    agents_df[agents_df$id %in% new_ids,turn+3] <- sample(na.omit(agents_df[,turn+3]),
                                                          size = new_births,
                                                          replace = T)
    }
  }
}

# Examine the results ----

output_df %>% 
  ggplot(
    aes(
      x = turn, 
      y = p, 
      color = round
    )
  ) +
  geom_line(linetype = "dashed")

######################################
#### Script for loss-aversion ABM ####
######################################

# Packages ----
library(tidyverse)
theme_set(theme_classic())
nice_colors <- c("#ADDFB3",
                 "#4A5E4C",
                 "#916C63",
                 "#8F8091",
                 "#DEAEA2",
                 "#D8B8DE",
                 "#7B9E7F")

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

# ABM ----

# Parameters 

N <- 50

turns <- 100

rounds <- 1

b_rate <- 0.1

age_categories <- 10

learn_prob_age <- c(rep(0.1, times = age_categories))
age_probs <- c(rep(0.1, times = age_categories))


# Dataframe to keep track of results
output_df <- matrix(
  data = NA_real_,
  nrow = turns * rounds,
  ncol = 3 + age_categories
) %>%
  as.data.frame()

colnames(output_df) <- c("turn", 
                         "round", 
                         map_chr(.x = 1:age_categories, 
                                 ~ paste0("p_",.x)), 
                         "total_p")

output_df$turn <- rep(1:turns, rounds)
output_df$round <- as.factor(rep(1:rounds, each = turns))

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
  
  # Then we assing age categories at random as well
  # Let's start with 10 age categories
  agents_df$age <- sample(c(1:10), 
                          size = N, 
                          replace = T, 
                          prob = age_probs)
  
  print(table(agents_df$age))
  
  # Average policy for each age-group
  smdf <- agents_df %>% 
    select(age,4) 
  colnames(smdf) <- c("age","p")
  smdf <- smdf %>% group_by(age) %>% summarise(avg = mean(p)) %>% ungroup()
  
  smdf <- smdf  %>% 
    add_row(
      age = setdiff(1:age_categories, smdf$age), 
      avg = rep(0, times = length(setdiff(1:age_categories, smdf$age)))) %>% 
    arrange(age)
  
  # Initial average preference
  output_df[output_df$turn == 1 &
              output_df$round == round, 3:(ncol(output_df)-1)] <-
    smdf %>% pull(avg)
  
  output_df[output_df$turn == 1 &
              output_df$round == round,ncol(output_df)] <- mean(agents_df[,4])
  
  # Build the loop for each turn
  
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
    
    # Function for prob of learning by age
    # We will begin with equal probability per category
    prob_df <- tibble(
      age = 1:10,
      learning_probs = rmutil::dbetabinom(
        y = c(1:age_categories),
        size = 10,
        m = 0.5,
        s = 2
      )
    )
    
    prob_df <- agents_df %>% 
      left_join(., prob_df, by = "age") %>% 
      select(id, age, learning_probs)
    
    learn_outcome <- rbinom(n = nrow(agents_df), 
                       size = 1,
                      p = prob_df$learning_probs)
    
    learners <- which(learn_outcome==1)
  
    # Empty container for benefit comparison
    # Only for learners
    benefits_comparison <- vector(length = length(learners), 
                                  mode = "numeric")
    
     # Benefit comparisons for those where new policy is lower
    benefits_comparison <- net_benefits[demonstrators[learners]] - net_benefits[learners]
    
    # People who perceive a higher net-benefit with the observe policy learn
    # Otherwise they stay where they are 
    changers <- learners[which(benefits_comparison > 0)]
    
    if(is_empty(changers)) {
      stayers <- c(1:nrow(agents_df))
      
      # Non-learners stay
      agents_df[stayers, (turn+3)] <-
        agents_df[stayers, (turn+2)]
      
    } else {
      stayers <- c(1:nrow(agents_df))[-c(changers)]
      
      # Changers update
      agents_df[changers, (turn+3)] <-
        agents_df[, turn+2][demonstrators[changers]]
      
      # Non-learners stay
      agents_df[stayers, (turn+3)] <-
        agents_df[stayers, (turn+2)]
      
    }
    
    
    # Keep track of average policy for each age-group
    smdf <- agents_df %>% 
      select(age,(turn+3)) 
    colnames(smdf) <- c("age","p")
    smdf <- smdf %>% group_by(age) %>% summarise(avg = mean(p)) %>% ungroup()
    smdf <- smdf  %>% 
      add_row(
        age = setdiff(1:age_categories, smdf$age), 
        avg = rep(0, times = length(setdiff(1:age_categories, smdf$age)))) %>% 
      arrange(age)
    
    output_df[output_df$turn == turn &
                output_df$round == round, 3:(ncol(output_df)-1)] <-
      smdf %>% pull(avg)
    
    output_df[output_df$turn == turn &
                output_df$round == round, ncol(output_df)] <- mean(agents_df[,(turn+3)])
    
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
      
      print(sample(na.omit(agents_df[,turn+3]),
                   size = new_births,
                   replace = T))
      
      # New policies for the newborns
      agents_df[agents_df$id %in% new_ids,turn+3] <- sample(na.omit(agents_df[,turn+3]),
                                                            size = new_births,
                                                            replace = T)
    }
  }
}

# Examine the results ----

output_df %>% 
  pivot_longer(4:13, 
               values_to = "p", 
               names_to = "group") %>% 
  ggplot(
    aes(
      x = turn, 
      y = p, 
      color = group
    )
  ) +
  geom_line() +
  facet_wrap(~round)

output_df %>% 
  ggplot(
    aes(
      x = turn, 
      y = total_p
    )
  ) +
  geom_line()  +
  facet_wrap(~round)

# Change per round ----

calculate_change_per_round <- function(rounds, turns) {
  change_matrix <- matrix(NA_real_, 
                          nrow = (turns-1), 
                          ncol = rounds)
  
  for (r in 1:rounds) {
    change_matrix[,r] <- abs(diff(output_df[output_df$round==r,]$total_p))
  }
  
  return(change_matrix)
}

chgs <- calculate_change_per_round(rounds = 20, turns = 100)

colSums(chgs)
mean(colSums(chgs))
median(colSums(chgs))
sd(colSums(chgs))
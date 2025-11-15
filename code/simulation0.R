# Simulation 0 modified by Junsong

# Author: 
# Version: 2025-10-27

# Libraries
library(tidyverse)

# Sources

# Parameters

# ============================== simu functions ================================

p_win <- function(sa, sb) {
  sa <- pmax(sa, 1e-9); 
  sb <- pmax(sb, 1e-9)
  p <- exp(sa) / (exp(sa) + exp(sb))
  return(p)
}

N = 60
pC = 0.2
strength_shape = 2
strength_rate = 2
random_cost = TRUE
reward = 1
cost_fight = 0.25
round_bouts = 10

coalitional_aggres_single_gen <- function(
    N = 100,
    pC = 0.1,
    round_bouts = 400,
    random_cost = TRUE,
    reward = 1,
    cost_fight = 0.25,
    strength_shape = 2,
    strength_rate = 2,
    seed = NULL
){
  # set seed
  if (!is.null(seed)) set.seed(seed)
  
  types <- c(rep("C", times = as.integer(N * pC)),
             rep("L", times = N - as.integer(N * pC)))
  id_c <- which(types == "C")
  # strength distribution
  stren <- rgamma(N, shape = strength_shape,
                  rate = strength_rate)
  payoff <- numeric(N)
  names(payoff) <- types
  
  # memory of coalition members (each individual can only have 1)
  coalition_members <- vector("list", N)
  coalition_members <- lapply(coalition_members, function(x) NA_real_)
  
  # save bouts
  dat_bouts <- tibble(round_id = numeric(),
                      i = numeric(), 
                      i_strength = numeric(),
                      i_type = character(), 
                      i_coal_formed = logical(),
                      i_ally = numeric(),
                      i_payoff = numeric(),
                      j = numeric(), 
                      j_strength = numeric(),
                      j_type = character(), 
                      j_coal_formed = logical(),
                      j_ally = numeric(),
                      j_payoff = numeric())
  
  for (r in seq_len(round_bouts)) {
    # r = 1
    # print(paste("round:", r)) 
    # sample conflicting members
    ij <- sample.int(N, 2)
    i <- ij[1]
    j <- ij[2]
    i_ally <- NA
    j_ally <- NA
    i_coal_formed <- NA
    j_coal_formed <- NA
    
    avail_C <- setdiff(which(types == "C"), c(i, j))
    
    # cost-benefit calculation
    
    ## for i
    
    if (types[j] == "L" || types[i] == "L"){
      # here, we assume that L is unable to perceive coalition, but C can
      stren_opp <- stren[j]
    } else {
      # actually strength equals self + ally
      ## the sum here is typically empty due to the incapability of 
      ## forming coalition
      stren_opp <- stren[j] + ifelse(!is.na(coalition_members[[j]]),
                                     sum(stren[coalition_members[[j]]]), 0)
    }
    p_i_solo <- p_win(stren[i], stren_opp)
    
    if (types[i] == "C") {
      # sample an ally if there is a prior ally
      # the sample here is only for EV calculation and do not guarantee coalition
      if (!is.na(coalition_members[[i]])) {
        i_ally <- sample(coalition_members[[i]], 1)
      } else if(length(avail_C) > 0) {
        # no prior ally, sample one from the pool
        i_ally <- sample(avail_C, 1)
        # remove this individual from the pool
        avail_C <- avail_C[avail_C != i_ally]
      } else {
        # no coalition can be formed
        i_ally <- NA
      }
    }
    ### this equals p_i_solo when the type is "L"
    p_i_coal <- p_win(stren[i] + ifelse(is.na(i_ally), 0, stren[i_ally]), stren_opp)
    
    ## for j
    
    if (types[i] == "L" || types[j] == "L"){
      stren_opp <- stren[i]
    } else {
      stren_opp <- stren[i] + ifelse(!is.na(coalition_members[[i]]), 
                                     sum(stren[coalition_members[[i]]]), 0)
    }
    p_j_solo <- p_win(stren[j], stren_opp)
    
    if (types[j] == "C") {
      if (!is.na(coalition_members[[j]])) {
        j_ally <- sample(coalition_members[[j]], 1)
      } else if(length(avail_C) > 0) {
        # no prior ally, sample one from the pool
        j_ally <- sample(avail_C, 1)
        # remove this individual from the pool
        avail_C <- avail_C[avail_C != j_ally]
      } else {
        # no coalition can be formed
        j_ally <- NA
      }
    }
    
    p_j_coal <- p_win(stren[j] + ifelse(is.na(j_ally), 0, stren[j_ally]), stren_opp)
    
    if (random_cost) {
      # random fighting costs
      ev_i_solo <- reward * p_i_solo - cost_fight
      ev_i_coal <- (reward * p_i_coal - cost_fight) / 2 # this always smaller than solo for type "L"
      ev_j_solo <- reward * p_j_solo - cost_fight
      ev_j_coal <- (reward * p_j_coal - cost_fight) / 2
      
    } else {
      # stronger individuals suffer more
      ev_i_solo <- reward * p_i_solo - cost_fight
      ev_i_coal <- reward * p_i_coal / 2 - 
        cost_fight * (stren[i] / (stren[i] + ifelse(!is.na(coalition_members[[i]]), 
                                                    sum(stren[coalition_members[[i]]]), 0) ))
      
      ev_j_solo <- reward * p_j_solo - cost_fight
      ev_j_coal <- reward * p_j_coal / 2 - 
        cost_fight * (stren[j] / (stren[j] + ifelse(!is.na(coalition_members[[j]]), 
                                                    sum(stren[coalition_members[[j]]]), 0) ))
    }

    
    # coalition formation and updating
    if (ev_i_coal > ev_i_solo && types[i] == "C") {
      # once settled, no new member will be added (e.g., a fixed group size of 2)
      i_coal_formed <- TRUE
      # print(paste("coalition_members[[i]] is:", coalition_members[[i]]))
      # print(paste("coalition_members[[i_ally]] is:", coalition_members[[i_ally]]))
      coalition_members[[i]] <- i_ally
      # reciprocity
      coalition_members[[i_ally]] <- i
    } else {
      # if not, then the ally candidate will be removed
      i_ally <- NA
    }
    
    if (ev_j_coal > ev_j_solo && types[j] == "C") {
      j_coal_formed <- TRUE
      # print(paste("coalition_members[[j]] is:", coalition_members[[j]]))
      # print(paste("coalition_members[[j_ally]] is:", coalition_members[[j_ally]]))
      coalition_members[[j]] <- j_ally
      coalition_members[[j_ally]] <- j
    } else {
      j_ally <- NA
    }
    
    # resolve conflicts
    i_stren_total <- ifelse(!is.na(i_ally), stren[i] + sum(stren[i_ally]), stren[i])
    j_stren_total <- ifelse(!is.na(j_ally), stren[j] + sum(stren[j_ally]), stren[j])
    
    ## determine winner and assign rewards
    if (runif(1) < p_win(i_stren_total, j_stren_total)) {
      # i wins - assign rewards
      if (!is.na(i_ally)) {
        payoff[i] <- payoff[i] + reward / 2
        payoff[i_ally] <- payoff[i_ally] + reward / 2
      } else {
        payoff[i] <- payoff[i] + reward
      }
    } else {
      # j wins - assign rewards
      if (!is.na(j_ally)) {
        payoff[j] <- payoff[j] + reward / 2
        payoff[j_ally] <- payoff[j_ally] + reward / 2
      } else {
        payoff[j] <- payoff[j] + reward
      }
    }
    
    ## both sides pay costs (winner and loser)
    # i and i_ally pay costs
    if (!is.na(i_ally)) {
      # coalition formed
      if (random_cost) {
        cost_ind <- sample.int(2, 1)
        payoff[i] <- payoff[i] - cost_fight * (2 - cost_ind)
        payoff[i_ally] <- payoff[i_ally] - cost_fight * (cost_ind - 1)
      } else { 
        payoff[i] <- payoff[i] - cost_fight * (stren[i] / (stren[i] + stren[i_ally]))
        payoff[i_ally] <- payoff[i_ally] - cost_fight * (stren[i_ally] / (stren[i] + stren[i_ally]))
      }
    } else {
      payoff[i] <- payoff[i] - cost_fight
    }
    
    # j and j_ally pay costs
    if (!is.na(j_ally)) {
      if (random_cost) {
        cost_ind <- sample.int(2, 1)
        payoff[j] <- payoff[j] - cost_fight * (2 - cost_ind)
        payoff[j_ally] <- payoff[j_ally] - cost_fight * (cost_ind - 1)
      } else {
        payoff[j] <- payoff[j] - cost_fight * (stren[j] / (stren[j] + stren[j_ally]))
        payoff[j_ally] <- payoff[j_ally] - cost_fight * (stren[j_ally] / (stren[j] + stren[j_ally]))
      }
    } else {
      payoff[j] <- payoff[j] - cost_fight
    }
    
    dat_bouts <- dat_bouts %>%
      add_row(
        round_id = r,
        i = i,
        i_strength = stren[i],
        i_type = as.character(types[i]),
        i_coal_formed = ifelse(is.na(i_coal_formed), NA, as.logical(i_coal_formed)),
        i_ally = ifelse(is.na(i_ally), NA_real_, as.numeric(i_ally)),
        i_payoff = as.numeric(payoff[i]),
        j = j,
        j_strength = stren[j],
        j_type = as.character(types[j]),
        j_coal_formed = ifelse(is.na(j_coal_formed), NA, as.logical(j_coal_formed)),
        j_ally = ifelse(is.na(j_ally), NA_real_, as.numeric(j_ally)),
        j_payoff = as.numeric(payoff[j])
      )
    
    
  } # end the for loop
  
  return(dat_simu = tibble(dat_bouts = list(dat_bouts),
                           strenth = list(stren),
                           payoff = list(payoff)))
}

simu_dat <- coalitional_aggres_single_gen(N = 100,
                                          pC = 0.10,
                                          round_bouts = 1000,
                                          random_cost = FALSE,
                                          reward = 1,
                                          cost_fight = 0.35,
                                          strength_shape = 2,
                                          strength_rate = 2,
                                          seed = NULL)

dat_bout <- simu_dat$dat_bouts[[1]] %>%
  filter(i_type == "C" | j_type == "C")

logit_reg <- glm(admit ~ gre + gpa + rank, data = dat_bout, family = "binomial")

payoff <- simu_dat %>% pull(payoff) %>% unlist()
payoff_C <- payoff[names(payoff) == "C"]
payoff_L <- payoff[names(payoff) == "L"]

mean(payoff[names(payoff) == "C"])
mean(payoff[names(payoff) == "L"])

ggplot(tibble(payoff = c(payoff_C, payoff_L),
              type = c(rep("C", times = length(payoff_C)),
                       rep("L", times = length(payoff_L))))) +
  geom_density(aes(payoff, fill = type), color = "white", alpha = 0.5) +
  theme_bw()

# plot gamma 
x <- seq(0, 10, length.out = 300)
y <- dgamma(x, shape = 5, rate = 1)
df_gamma <- data.frame(x = x, y = y)

ggplot(df_gamma, aes(x, y)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(x = "x", y = "Density") +
  theme_bw()

# higher gini, more imbalance
gini_mc <- ineq::ineq(y, type = "Gini")
gini_mc


# ============================= batch simulations ==============================

# specify conditions

simu_condi <- expand_grid(N = 100,
                          pC = c(0.1, 0.2, 0.5),
                          round_bouts = c(500, 1000, 1500),
                          strength_shape = c(1, 2, 3, 4, 5),
                          strength_rate = 1,
                          random_cost = c(TRUE, FALSE),
                          reward = 1,
                          cost_fight = c(0.02, 0.05, 0.1, 0.25, 0.5))

# parallel computing
library(furrr)
plan(multisession, workers = parallel::detectCores() - 2) # 12 - 2 = 10

res_list <- future_pmap(.l = simu_condi,
                        .f = function(N, pC, round_bouts, strength_shape, strength_rate,
                                      random_cost, reward, cost_fight) {
                          coalitional_aggres_single_gen(
                            N = N, pC = pC, round_bouts = round_bouts,
                            strength_shape = strength_shape, strength_rate = strength_rate,
                            random_cost = random_cost, reward = reward, cost_fight = cost_fight,
                            seed = sample.int(.Machine$integer.max, 1))
                        },
                        .options = furrr_options(seed = TRUE))

simu_dat <- bind_cols(simu_condi, tibble(result = res_list))
#save(simu_dat, file = "./code/output/simu_dat.RData")

# ================================== analysis ==================================

load("./code/output/simu_dat.RData")

simu_payoff <- simu_dat %>%
  mutate(m_payoff = map(result, ~ {
    payoff <- .x$payoff[[1]]
    payoff_C <- mean(payoff[names(payoff) == "C"])
    payoff_L <- mean(payoff[names(payoff) == "L"])
    list(payoff_C = payoff_C,
         payoff_L = payoff_L)
  })) %>%
  unnest_wider(m_payoff)


payoff_delta <- simu_payoff %>%
  mutate(payoff_delta = payoff_C - payoff_L) %>%
  select(-result)

lm1 <- lm(payoff_delta ~ round_bouts + strength_shape + cost_fight + random_cost,
          data = payoff_delta %>%
            mutate(random_cost = if_else(random_cost, 1, 0)))

summary(lm1)

# =========================== replicator dynamics ==============================
library(deSolve)

payoff <- simu_dat %>%
  unnest(dat_bouts) %>%
  group_by(j_type) %>%
  summarise(mean_pay = mean(j_payoff)) %>%
  pull(mean_pay)

payoff_name <- simu_dat %>%
  unnest(dat_bouts) %>%
  group_by(j_type) %>%
  summarise(mean_pay = mean(j_payoff)) %>%
  pull(j_type)

names(payoff) <- payoff_name

# define the replicator equation

replicator_dynamics <- function(t, p, payoff) {
  
  p <- pmax(0, pmin(1, p))
  p <- p / sum(p)
  
  f <- payoff
  f_bar <- sum(p * payoff)
  
  dp <- p * (f - f_bar)
  
  return(list(dp))
}

# solve the equation
initial_p <- c(0.1, 0.9)
time_span <- seq(0, 50, by = 0.1)

parameters <- c(payoff = payoff)

result <- ode(y = initial_p, times = time_span, 
              func = replicator_dynamics, 
              parms = parameters)

# Convert to data frame
result_df <- as_tibble(result)
colnames(result_df) <- c("time", "C", "L")

result_long <- result_df %>%
  pivot_longer(cols = c("C", "L"), 
               names_to = "strategy", 
               values_to = "frequency")

ggplot(result_long, aes(x = time, y = frequency, color = strategy)) +
  geom_line(size = 1.2) +
  labs(title = "Replicator Dynamics Over Time",
       x = "Time",
       y = "Strategy Frequency",
       color = "Strategy") +
  theme_bw()

# Simulation 0 with yielding/failure cost

# Author: 
# Version: 2025-11-11

# Libraries
library(tidyverse)

# Sources

# Parameters

# =========================== simulation functions =============================

p_win <- function(sa, sb) {
  sa <- pmax(sa, 1e-9); 
  sb <- pmax(sb, 1e-9)
  p <- exp(sa) / (exp(sa) + exp(sb))
  return(p)
}

# for testing
N = 100
pC = 0.2
strength_shape = 2
strength_rate = 2
random_cost = TRUE
reward = 1
cost_fight = 0.25
cost_lose = 0.10
round_bouts = 10

coalitional_aggres_single_gen <- function(
    N = 100,
    pC = 0.1,
    round_bouts = 400,
    random_cost = TRUE,
    reward = 1,
    cost_fight = 0.15,
    cost_lose = 0.10,
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
                      i_yield = logical(),
                      i_coal_formed = logical(),
                      i_ally = numeric(),
                      i_payoff = numeric(),
                      j = numeric(), 
                      j_strength = numeric(),
                      j_type = character(), 
                      j_yield = logical(),
                      j_coal_formed = logical(),
                      j_ally = numeric(),
                      j_payoff = numeric(),
                      i_win = logical())
  
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
    i_yield <- FALSE
    j_yield <- FALSE
    i_win <- FALSE
    
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
    
    
    # EV calculation
    ev_i_solo <- reward * p_i_solo - cost_fight - cost_lose * (1 - p_i_solo)
    ev_j_solo <- reward * p_j_solo - cost_fight - cost_lose * (1 - p_j_solo)
    
    if (types[i] == "C" && !is.na(i_ally)) {
      # random cost or not
      if (random_cost) {
        ev_i_coal <- (reward * p_i_coal - cost_fight - cost_lose * (1 - p_i_coal)) / 2 
      } else {
        # stronger individuals suffer less
        coal_i_stren_total <- stren[i] + stren[i_ally]
        
        ev_i_coal <- reward * p_i_coal / 2 - 
          cost_fight * (1 - stren[i] / coal_i_stren_total) -
          cost_lose * (1 - p_i_coal) * (1 - stren[i] / coal_i_stren_total)
      }
      
    } else {
      ev_i_coal <- -Inf
    }
    
    if (types[j] == "C" && !is.na(j_ally)) {
      # random cost or not
      if (random_cost) {
        ev_j_coal <- (reward * p_j_coal - cost_fight - cost_lose * (1 - p_j_coal)) / 2 
      } else {
        # stronger individuals suffer less
        coal_j_stren_total <- stren[j] + stren[j_ally]
        
        ev_j_coal <- reward * p_j_coal / 2 - 
          cost_fight * (1 - stren[j] / coal_j_stren_total) -
          cost_lose * (1 - p_j_coal) * (1 - stren[j] / coal_j_stren_total)
      }
      
    } else {
      ev_j_coal <- -Inf
    }
    
    # if (random_cost) {
    #   # random fighting costs
    #   ev_i_coal <- (reward * p_i_coal - cost_fight - cost_lose * (1 - p_i_coal)) / 2 # this always smaller than solo for type "L"
    #   ev_j_coal <- (reward * p_j_coal - cost_fight - cost_lose * (1 - p_j_coal)) / 2
    #   
    # } else {
    #   # stronger individuals suffer less
    #   ev_i_coal <- reward * p_i_coal / 2 - 
    #     cost_fight * (1 - stren[i] / (stren[i] + ifelse(!is.na(i_ally), 
    #                                                     stren[i_ally], 0) )) -
    #     cost_lose * (1 - p_i_coal) * (1 - stren[i] / (stren[i] + ifelse(!is.na(i_ally), 
    #                                                                     stren[i_ally], 0) ))
    #   
    #   ev_j_coal <- reward * p_j_coal / 2 - 
    #     cost_fight * (1 - stren[j] / (stren[j] + ifelse(!is.na(j_ally), 
    #                                                     stren[j_ally], 0) )) -
    #     cost_lose * (1 - p_j_coal) * (1 - stren[j] / (stren[j] + ifelse(!is.na(j_ally), 
    #                                                                     stren[j_ally], 0) ))
    # }
    
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
    
    # however, yield when the payoff is negative
    if (max(c(ev_i_coal, ev_i_solo)) < 0) {
      i_coal_formed <- TRUE
      i_ally <- NA
      i_yield <- TRUE
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
    
    # however, yield when the payoff is negative
    if (max(c(ev_j_coal, ev_j_solo)) < 0) {
      j_coal_formed <- TRUE
      j_ally <- NA
      j_yield <- TRUE
    }
    
    # if one yields, move to the next iteration
    if (i_yield && j_yield) {
      # both yield (unlikely) then no one gets the reward
      payoff[i] <- payoff[i] + 0
      payoff[j] <- payoff[j] + 0
      
      dat_bouts <- dat_bouts %>%
        add_row(
          round_id = r,
          i = i,
          i_strength = stren[i],
          i_type = as.character(types[i]),
          i_yield = i_yield,
          i_coal_formed = ifelse(is.na(i_coal_formed), NA, as.logical(i_coal_formed)),
          i_ally = ifelse(is.na(i_ally), NA_real_, as.numeric(i_ally)),
          i_payoff = as.numeric(payoff[i]),
          j = j,
          j_strength = stren[j],
          j_type = as.character(types[j]),
          j_yield = j_yield,
          j_coal_formed = ifelse(is.na(j_coal_formed), NA, as.logical(j_coal_formed)),
          j_ally = ifelse(is.na(j_ally), NA_real_, as.numeric(j_ally)),
          j_payoff = as.numeric(payoff[j]),
          i_win = i_win
        )
      
    } else if (i_yield){
      # no reward but also no cost for i
      payoff[i] <- payoff[i] + 0
      payoff[j] <- payoff[j] + reward
      
      dat_bouts <- dat_bouts %>%
        add_row(
          round_id = r,
          i = i,
          i_strength = stren[i],
          i_type = as.character(types[i]),
          i_yield = i_yield,
          i_coal_formed = ifelse(is.na(i_coal_formed), NA, as.logical(i_coal_formed)),
          i_ally = ifelse(is.na(i_ally), NA_real_, as.numeric(i_ally)),
          i_payoff = as.numeric(payoff[i]),
          j = j,
          j_strength = stren[j],
          j_type = as.character(types[j]),
          j_yield = j_yield,
          j_coal_formed = ifelse(is.na(j_coal_formed), NA, as.logical(j_coal_formed)),
          j_ally = ifelse(is.na(j_ally), NA_real_, as.numeric(j_ally)),
          j_payoff = as.numeric(payoff[j]),
          i_win = i_win
        )
      
      next
    } else if (j_yield) {
      # no reward but also no cost for j
      payoff[i] <- payoff[i] + reward
      payoff[j] <- payoff[j] + 0
      i_win <- TRUE
      
      dat_bouts <- dat_bouts %>%
        add_row(
          round_id = r,
          i = i,
          i_strength = stren[i],
          i_type = as.character(types[i]),
          i_yield = i_yield,
          i_coal_formed = ifelse(is.na(i_coal_formed), NA, as.logical(i_coal_formed)),
          i_ally = ifelse(is.na(i_ally), NA_real_, as.numeric(i_ally)),
          i_payoff = as.numeric(payoff[i]),
          j = j,
          j_strength = stren[j],
          j_type = as.character(types[j]),
          j_yield = j_yield,
          j_coal_formed = ifelse(is.na(j_coal_formed), NA, as.logical(j_coal_formed)),
          j_ally = ifelse(is.na(j_ally), NA_real_, as.numeric(j_ally)),
          j_payoff = as.numeric(payoff[j]),
          i_win = i_win
        )
      
      next
    }
    
    # if not, resolve conflicts
    i_stren_total <- ifelse(!is.na(i_ally), stren[i] + sum(stren[i_ally]), stren[i])
    j_stren_total <- ifelse(!is.na(j_ally), stren[j] + sum(stren[j_ally]), stren[j])
    
    ## determine winner and assign rewards
    i_win <- runif(1) < p_win(i_stren_total, j_stren_total)
    if (i_win) {
      # i wins - assign rewards
      # also add extra cost to j
      if (!is.na(i_ally)) {
        payoff[i] <- payoff[i] + reward / 2
        payoff[i_ally] <- payoff[i_ally] + reward / 2
      } else {
        # solo
        payoff[i] <- payoff[i] + reward
      }
    } else {
      # j wins - assign rewards
      # also add extra cost to i
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
        
        # if i loses
        if(!i_win) {
          payoff[i] <- payoff[i] - cost_lose * (2 - cost_ind)
          payoff[i_ally] <- payoff[i_ally] - cost_lose * (cost_ind - 1)
        }
        
      } else { 
        # non-random cost
        payoff[i] <- payoff[i] - cost_fight * (1 - stren[i] / (stren[i] + stren[i_ally]))
        payoff[i_ally] <- payoff[i_ally] - cost_fight * (1 - stren[i_ally] / (stren[i] + stren[i_ally]))
        
        # if i loses
        if(!i_win) {
          payoff[i] <- payoff[i] - cost_lose * (1 - stren[i] / (stren[i] + stren[i_ally]))
          payoff[i_ally] <- payoff[i_ally] - cost_lose *  (1 - stren[i_ally] / (stren[i] + stren[i_ally]))
        }
      }
    } else {
      # solo
      payoff[i] <- payoff[i] - cost_fight
      
      if(!i_win) {
        payoff[i] <- payoff[i] - cost_lose
      }
    }
    
    # j and j_ally pay costs
    if (!is.na(j_ally)) {
      # coalition formed
      if (random_cost) {
        cost_ind <- sample.int(2, 1)
        payoff[j] <- payoff[j] - cost_fight * (2 - cost_ind)
        payoff[j_ally] <- payoff[j_ally] - cost_fight * (cost_ind - 1)
        
        # if j loses
        if(i_win) {
          payoff[j] <- payoff[j] - cost_lose * (2 - cost_ind)
          payoff[j_ally] <- payoff[j_ally] - cost_lose * (cost_ind - 1)
        }
        
      } else {
        # non-random cost
        payoff[j] <- payoff[j] - cost_fight * (1 - stren[j] / (stren[j] + stren[j_ally]))
        payoff[j_ally] <- payoff[j_ally] - cost_fight * (1 - stren[j_ally] / (stren[j] + stren[j_ally]))
        
        # if j loses
        if(i_win) {
          payoff[j] <- payoff[j] - cost_lose * (1 - stren[j] / (stren[j] + stren[j_ally]))
          payoff[j_ally] <- payoff[j_ally] - cost_lose *  (1 - stren[j_ally] / (stren[j] + stren[j_ally]))
        }
      }
    } else {
      payoff[j] <- payoff[j] - cost_fight
      
      if(i_win) {
        payoff[j] <- payoff[j] - cost_lose
      }
    }
    
    dat_bouts <- dat_bouts %>%
      add_row(
        round_id = r,
        i = i,
        i_strength = stren[i],
        i_type = as.character(types[i]),
        i_yield = i_yield,
        i_coal_formed = ifelse(is.na(i_coal_formed), NA, as.logical(i_coal_formed)),
        i_ally = ifelse(is.na(i_ally), NA_real_, as.numeric(i_ally)),
        i_payoff = as.numeric(payoff[i]),
        j = j,
        j_strength = stren[j],
        j_type = as.character(types[j]),
        j_yield = j_yield,
        j_coal_formed = ifelse(is.na(j_coal_formed), NA, as.logical(j_coal_formed)),
        j_ally = ifelse(is.na(j_ally), NA_real_, as.numeric(j_ally)),
        j_payoff = as.numeric(payoff[j]),
        i_win = i_win
      )
    
    
  } # end the for loop
  
  return(dat_simu = tibble(dat_bouts = list(dat_bouts),
                           strenth = list(stren),
                           payoff = list(payoff)))
}

# single simulation
simu_dat <- coalitional_aggres_single_gen(N = 100,
                                          pC = 0.10,
                                          round_bouts = 1000,
                                          random_cost = FALSE,
                                          reward = 1,
                                          cost_fight = 0.15,
                                          cost_lose = 0.1,
                                          strength_shape = 2,
                                          strength_rate = 2,
                                          seed = NULL)

dat_bout <- simu_dat$dat_bouts[[1]] %>%
  filter(i_type == "C" | j_type == "C")

#logit_reg <- glm(admit ~ gre + gpa + rank, data = dat_bout, family = "binomial")

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
# larger shape parameters indicate more balanced strength distributions
x <- seq(0, 10, length.out = 300)
y <- dgamma(x, shape = 1, rate = 1)
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
                          strength_shape = c(1, 2, 3, 4),
                          strength_rate = 1,
                          random_cost = c(TRUE, FALSE),
                          reward = 1,
                          cost_fight = c(0.02, 0.05, 0.1, 0.3),
                          cost_lose = c(0, 0.05, 0.10, 0.20))

# parallel computing
library(furrr)
plan(multisession, workers = parallel::detectCores() - 2) # 12 - 2 = 10

res_list <- future_pmap(.l = simu_condi,
                        .f = function(N, pC, round_bouts, strength_shape, strength_rate,
                                      random_cost, reward, cost_fight, cost_lose) {
                          coalitional_aggres_single_gen(
                            N = N, pC = pC, round_bouts = round_bouts,
                            strength_shape = strength_shape, strength_rate = strength_rate,
                            random_cost = random_cost, reward = reward, cost_fight = cost_fight,
                            seed = sample.int(.Machine$integer.max, 1))
                        },
                        .options = furrr_options(seed = TRUE),
                        .progress = TRUE)

simu_dat <- bind_cols(simu_condi, tibble(result = res_list))
#save(simu_dat, file = "./code/output/simu_dat_yield.RData")

# =========================== payoff analysis ==================================

load("./code/output/simu_dat_yield.RData")

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

lm1 <- lm(payoff_delta ~ round_bouts + strength_shape + cost_fight + cost_lose + random_cost,
          data = payoff_delta %>%
            mutate(random_cost = if_else(random_cost, 1, 0)))

summary(lm1)

# check cost_fight

payoff_delta %>%
  group_by(cost_fight) %>%
  summarise(delta_m = mean(payoff_delta),
            delta_sd = sd(payoff_delta))

# ========================== coalition formation frequency =====================

simu_payoff_unnest <- simu_payoff %>%
  unnest(result) %>%
  mutate(coal_res = map(dat_bouts, ~ {
    
    dat_C_i <- filter(.x, i_type == "C") %>%
      mutate(my_stren = i_strength,
             stren_delta = i_strength - j_strength,
             rival_type = j_type,
             coal_form = i_coal_formed) %>%
      select(coal_form, my_stren, rival_type, stren_delta)
    
    dat_C_j <- filter(.x, j_type == "C") %>%
      mutate(my_stren = j_strength,
             stren_delta = j_strength - i_strength,
             rival_type = i_type,
             coal_form = j_coal_formed) %>%
      select(coal_form, my_stren, rival_type, stren_delta)
    
    bind_rows(dat_C_i, dat_C_j)
  }))


dat_coal <- simu_payoff_unnest %>%
  select(-c(dat_bouts:payoff)) %>%
  unnest(coal_res) %>%
  mutate(coal_form = if_else(is.na(coal_form), 0, 1))

library(lme4)

# Create a unique ID for each simulation run
dat_coal <- dat_coal %>%
  group_by(N, pC, round_bouts, strength_shape, strength_rate, 
           random_cost, reward, cost_fight, cost_lose) %>%
  mutate(sim_id = cur_group_id()) %>%
  ungroup()

# random intercept 
model1 <- glmer(coal_form ~ my_stren + rival_type + stren_delta + 
                  cost_fight + cost_lose + random_cost + my_stren * random_cost +
                  (1 | sim_id),
                data = dat_coal,
                family = binomial(link = "logit"))

summary(model1)
#save(model1, file = "./code/output/glm1_coal_yield.RData")

# random slopes
model2 <- glmer(coal_form ~ my_stren + rival_type + stren_delta + 
                  cost_fight + cost_lose + 
                  (1 + my_stren | sim_id),
                data = dat_coal,
                family = binomial(link = "logit"))

summary(model2)

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

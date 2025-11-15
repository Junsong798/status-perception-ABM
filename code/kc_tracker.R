# Simple KC tracker

# Author: Junsong Lu
# Version: 2025-09-08

# Libraries
library(tidyverse)

# Sources

# Parameters

# ============================================================================

# ----------------------------
# Utilities
# ----------------------------
norm01 <- function(x) {
  if (!length(x)) return(x)
  if (all(!is.finite(x))) return(rep(0, length(x)))
  rng <- range(x[is.finite(x)], na.rm = TRUE)
  if (diff(rng) < 1e-12) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

clip01 <- function(x) pmin(1, pmax(0, x))

# Decayed alliance memory; add `pairs` that allied this step
update_alliance_matrix <- function(W, pairs, decay = 1, weight = 1) {
  W <- W * decay
  if (!is.null(pairs) && nrow(pairs) > 0) {
    for (k in seq_len(nrow(pairs))) {
      i <- pairs[k, 1]; j <- pairs[k, 2]
      if (i != j) {
        W[i, j] <- W[i, j] + weight
        W[j, i] <- W[j, i] + weight
      }
    }
  }
  diag(W) <- 0
  W
}

# Katz/Bonacich (classic Katz with exogenous ones vector)
katz_from_W <- function(W, beta = NULL) {
  n <- nrow(W); if (n == 0) return(numeric())
  W <- (W + t(W))/2; diag(W) <- 0
  ev  <- suppressWarnings(eigen(W, only.values = TRUE)$values)
  rho <- max(Re(ev), 0)
  if (is.null(beta)) {
    beta <- if (rho > 0) 0.9 / rho else 0
  } else if (rho > 0 && beta >= 1/rho) {
    beta <- 0.9 / rho
  }
  I <- diag(n); ones <- rep(1, n)
  x <- tryCatch(
    as.numeric(solve(I - beta * W, ones)),
    error = function(e) {
      # truncated power series fallback
      acc  <- ones
      term <- ones
      for (k in 1:20) {
        term <- beta * (W %*% term)
        acc  <- acc + term
      }
      as.numeric(acc)
    }
  )
  x
}

# Soft probability a side wins given strength S (logistic on strength diff)
p_win <- function(Sa, Sb, k = 2.0) {
  1 / (1 + exp(-k * (Sa - Sb)))
}

# Draw two distinct indices
two_distinct <- function(n) {
  i <- sample.int(n, 1)
  j <- sample(c(seq_len(n)[-i]), 1)
  c(i, j)
}

# ----------------------------
# Core simulation
# ----------------------------
run_sim1 <- function(N = 60, G = 100, init_p_status = 0.5,
                     strength_mean = 0.5, strength_sd = 0.25,
                     wtr_mean = 0, wtr_sd = 0.25,
                     allow_form_join = TRUE,
                     bw_weight = 1, fs_weight = 1
                     ) {
  
  # --- init population ---
  type <- ifelse(runif(N) < init_p_status, "status", "form")
  strength_raw <- norm01(rnorm(N, mean = strength_mean, sd = strength_sd))
  strength <- strength_raw
  
  # Dyadic WTR matrix (as others→me valuation; Θ[i,j] = how much i values j)
  WTR <- matrix(rnorm(N * N, wtr_mean, wtr_sd), N, N)
  diag(WTR) <- 0
  
  # Alliance memory + Katz
  WTR_est <- matrix(0, N, N)
  
  # Status compression from WTR: mean inbound valuation (others value me)
  compress_status_from_WTR <- function(WTR) {
    s <- colMeans(WTR, na.rm = TRUE)
    norm01(s)
  }
  
  for (g in seq_len(G)) {
    # pick two principals
    ij <- two_distinct(N)
    i <- ij[1]
    j <- ij[2]
    
    # candidate pool excludes principals
    candidates <- setdiff(seq_len(N), c(i, j))
    
    status_old <- compress_status_from_WTR(WTR)
    status_sig <- status_old
    
    # Strength totals (before the conflict)
    Si <- strength[i] + sum(WTR[, i] * strength)
    Sj <- strength[j] + sum(WTR[, j] * strength)
    
    # Resolve contest
    p_i <- p_win(Si, Sj)
    win_i <- runif(1) < p_i
    
    # Estimated Strength
    Si_est_status <- strength[i] + sum(WTR_est[, i] * strength)
    Sj_est_status <- strength[j] + sum(WTR_est[, j] * strength)
    p_i_est_status <- p_win(Si_est_status, Sj_est_status)
    
    Si_est_form <- strength[i] 
    Sj_est_form <- strength[j] 
    p_i_est_form <- p_win(Si_est_form, Sj_est_form)
    
    # Bandwagoning & social supports
    # an agent determine which side to support based on two processes:
    # 1) whether joining one of the two sides increases winning
    # 2) whether the target has a high WTR toward the agent
    
    pick_ally <- function(leader, opponent, can_prop) {
      if (!can_prop || length(candidates) == 0) return(NA_integer_)
      # Evaluate each candidate's net EV of joining this side vs the other
      # EV_k = bw_weight * (Δ p_win * benefit_share - cost_join)
      #     + fs_weight * (WTR[leader,k] - WTR[opponent,k])
      # Use current strengths + hypothetical joiner
      Sc_opp <- strength[opponent] + sum(WTR_est[opponent, ] * status_old)
      Sc_lea <- strength[leader] + sum(WTR_est[leader, ] * status_old)
      best_k <- NA_integer_
      best_u <- -Inf
      for (k in candidates) {
        # If "form" cannot join, skip non-status candidates (unless allow_form_join)
        k_can_join <- (type[k] == "status") || allow_form_join
        if (!k_can_join) next
        
        # bandwagon diff: joining leader raises p_win for that side
        p_lea_join <- p_win(Sc_lea + strength[k], Sc_opp)
        p_opp_join <- p_win(Sc_opp + strength[k], Sc_lea)
        # If candidate stayed neutral, we'll compare to not-join baseline.
        # For simplicity we compare "join leader" vs "join opponent"
        # (approx for a quick best side decision).
        delta_p <- p_lea_join - (1 - p_opp_join)
        
        ev_bw <- delta_p * (benefit_win * ally_share) - cost_join
        ev_fs <- (WTR[leader, k] - WTR[opponent, k])
        u     <- bw_weight * ev_bw + fs_weight * ev_fs + 1e-9 * status_sig[k] # tie-break by status
        
        if (u > best_u) { best_u <- u; best_k <- k }
      }
      best_k
    }
    
    
  }
}



# ----------------------------
# One-generation, all-agents-choose-sides sim
# ----------------------------
run_sim1 <- function(
    N = 60, G = 100,
    init_p_status = 0.5,
    strength_mean = 0.5, strength_sd = 0.25,
    wtr_mean = 0, wtr_sd = 0.25,
    allow_form_join = TRUE,
    bw_weight = 1, fs_weight = 1,
    cost_join = 0.05,
    benefit_win = 1.0,    # NEW
    cost_lose   = 0.5,    # NEW
    ally_share  = 0.5,    # NEW: fraction of winner benefit reserved for allies
    # learning rates (true WTR)
    wtr_lr_win = 0.10, wtr_lr_cost = 0.02, wtr_lr_betray = 0.05,
    # learning rates (estimated/common WTR)
    est_lr_win = 0.06, est_lr_cost = 0.01, est_lr_betray = 0.03,
    anticip_alpha = 0.7,
    signed_wtr = FALSE,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  # Types & strength
  type <- ifelse(runif(N) < init_p_status, "status", "form")
  strength <- norm01(pmax(0, rnorm(N, strength_mean, strength_sd)))
  
  # True WTR
  if (!signed_wtr) {
    WTR <- matrix(clip01(plogis(matrix(rnorm(N * N, wtr_mean, wtr_sd), N, N))),
                  nrow = N, ncol = N)
  } else {
    WTR <- matrix(clip11(tanh(matrix(rnorm(N * N, wtr_mean, wtr_sd), N, N))),
                  nrow = N, ncol = N)
  }
  diag(WTR) <- 0
  
  # Shared estimated WTR
  if (!signed_wtr) {
    WTR_est <- matrix(clip01(WTR + matrix(rnorm(N * N, 0, 0.05), N, N)),
                      nrow = N, ncol = N)
  } else {
    WTR_est <- matrix(clip11(WTR + matrix(rnorm(N * N, 0, 0.05), N, N)),
                      nrow = N, ncol = N)
  }
  diag(WTR_est) <- 0
  
  # Logs + payoffs
  hist <- data.frame(
    bout = integer(G), i = integer(G), j = integer(G),
    n_i = integer(G), n_j = integer(G),
    Si = numeric(G), Sj = numeric(G),
    p_i = numeric(G), win_i = logical(G)
  )
  payoff <- rep(0, N)  # <--- accumulates per-agent payoffs
  
  # Update helpers (respecting bounds)
  upd_win  <- function(x, lr) if (!signed_wtr) clip01(x + lr * (1 - x)) else clip11(x + lr * (1 - x))
  upd_cost <- function(x, lr) if (!signed_wtr) clip01(x - lr * x)         else clip11(x - lr * (1 + x))
  upd_bet  <- upd_cost
  
  for (g in seq_len(G)) {
    # principals
    ij <- two_distinct(N); i <- ij[1]; j <- ij[2]
    candidates <- setdiff(seq_len(N), c(i, j))
    
    # baseline estimated strengths
    S_i_base_est <- strength[i]
    S_j_base_est <- strength[j]
    
    # expected backing from others (estimated)
    contr_i <- strength * WTR_est[, i]
    contr_j <- strength * WTR_est[, j]
    EB_i_tot <- sum(contr_i[candidates])
    EB_j_tot <- sum(contr_j[candidates])
    
    support_i <- logical(N)
    support_j <- logical(N)
    
    for (k in candidates) {
      k_can_join <- (type[k] == "status") || allow_form_join
      if (!k_can_join) next
      
      # status anticipates others; form is myopic
      if (type[k] == "status") {
        Ai_excl_k <- EB_i_tot - contr_i[k]
        Aj_excl_k <- EB_j_tot - contr_j[k]
        S_i_est_k <- strength[i] + anticip_alpha * Ai_excl_k
        S_j_est_k <- strength[j] + anticip_alpha * Aj_excl_k
      } else {
        S_i_est_k <- S_i_base_est
        S_j_est_k <- S_j_base_est
      }
      
      p_i_base_est_k <- p_win(S_i_est_k, S_j_est_k)
      p_j_base_est_k <- 1 - p_i_base_est_k
      
      p_i_if_k_i <- p_win(S_i_est_k + strength[k], S_j_est_k)
      p_j_if_k_j <- p_win(S_j_est_k + strength[k], S_i_est_k)
      
      delta_i <- p_i_if_k_i - p_i_base_est_k
      delta_j <- p_j_if_k_j - p_j_base_est_k
      
      fs_i <-  WTR_est[i, k] - WTR_est[j, k]
      fs_j <- -fs_i
      
      u_i <- bw_weight * delta_i + fs_weight * fs_i - cost_join
      u_j <- bw_weight * delta_j + fs_weight * fs_j - cost_join
      u_0 <- 0
      
      um <- max(u_i, u_j, u_0)
      if (um > 0) {
        if (um == u_i)      support_i[k] <- TRUE
        else if (um == u_j) support_j[k] <- TRUE
      }
    }
    
    # realized true strengths (clip negatives if signed)
    if (!signed_wtr) {
      contrib_i_raw <- strength[support_i] * WTR[support_i, i]
      contrib_j_raw <- strength[support_j] * WTR[support_j, j]
    } else {
      contrib_i_raw <- strength[support_i] * pmax(WTR[support_i, i], 0)
      contrib_j_raw <- strength[support_j] * pmax(WTR[support_j, j], 0)
    }
    Si <- strength[i] + sum(contrib_i_raw)
    Sj <- strength[j] + sum(contrib_j_raw)
    
    p_i <- p_win(Si, Sj)
    win_i <- runif(1) < p_i
    
    # ---- Payoffs ----
    if (win_i) {
      # leader i reward
      payoff[i] <- payoff[i] + benefit_win * (1 - ifelse(length(contrib_i_raw) > 0, ally_share, 0))
      # supporters of i share ally pot proportionally, pay join cost
      if (length(contrib_i_raw) > 0) {
        pot <- benefit_win * ally_share
        w   <- contrib_i_raw
        s   <- sum(w)
        if (s > 0) {
          shares <- pot * (w / s)
        } else {
          # fallback: equal split if all contribs are zero
          shares <- rep(pot / length(w), length(w))
        }
        payoff[which(support_i)] <- payoff[which(support_i)] + shares - cost_join
      }
      # losing side j: leader pays lose cost; allies pay join cost
      payoff[j] <- payoff[j] - cost_lose
      if (any(support_j)) payoff[which(support_j)] <- payoff[which(support_j)] - cost_join
    } else {
      # leader j reward
      payoff[j] <- payoff[j] + benefit_win * (1 - ifelse(length(contrib_j_raw) > 0, ally_share, 0))
      if (length(contrib_j_raw) > 0) {
        pot <- benefit_win * ally_share
        w   <- contrib_j_raw
        s   <- sum(w)
        if (s > 0) {
          shares <- pot * (w / s)
        } else {
          shares <- rep(pot / length(w), length(w))
        }
        payoff[which(support_j)] <- payoff[which(support_j)] + shares - cost_join
      }
      payoff[i] <- payoff[i] - cost_lose
      if (any(support_i)) payoff[which(support_i)] <- payoff[which(support_i)] - cost_join
    }
    
    # ---- Learning (unchanged signs) ----
    if (win_i) {
      if (any(support_i)) {
        for (k in which(support_i)) {
          WTR[i, k]     <- upd_win(WTR[i, k], wtr_lr_win)
          WTR[k, i]     <- upd_win(WTR[k, i], wtr_lr_win)
          WTR_est[i, k] <- upd_win(WTR_est[i, k], est_lr_win)
          WTR_est[k, i] <- upd_win(WTR_est[k, i], est_lr_win)
        }
      }
      if (any(support_j)) {
        for (k in which(support_j)) {
          WTR[j, k]     <- upd_cost(WTR[j, k], wtr_lr_cost)
          WTR[k, j]     <- upd_cost(WTR[k, j], wtr_lr_cost)
          WTR_est[j, k] <- upd_cost(WTR_est[j, k], est_lr_cost)
          WTR_est[k, j] <- upd_cost(WTR_est[k, j], est_lr_cost)
        }
      }
      if (length(candidates)) {
        could_join <- candidates[(type[candidates] == "status") | allow_form_join]
        non_joiners_j <- setdiff(could_join, which(support_j))
        for (k in non_joiners_j) {
          WTR[j, k]     <- upd_bet(WTR[j, k], wtr_lr_betray)
          WTR_est[j, k] <- upd_bet(WTR_est[j, k], est_lr_betray)
        }
      }
    } else {
      if (any(support_j)) {
        for (k in which(support_j)) {
          WTR[j, k]     <- upd_win(WTR[j, k], wtr_lr_win)
          WTR[k, j]     <- upd_win(WTR[k, j], wtr_lr_win)
          WTR_est[j, k] <- upd_win(WTR_est[j, k], est_lr_win)
          WTR_est[k, j] <- upd_win(WTR_est[k, j], est_lr_win)
        }
      }
      if (any(support_i)) {
        for (k in which(support_i)) {
          WTR[i, k]     <- upd_cost(WTR[i, k], wtr_lr_cost)
          WTR[k, i]     <- upd_cost(WTR[k, i], wtr_lr_cost)
          WTR_est[i, k] <- upd_cost(WTR_est[i, k], est_lr_cost)
          WTR_est[k, i] <- upd_cost(WTR_est[k, i], est_lr_cost)
        }
      }
      if (length(candidates)) {
        could_join <- candidates[(type[candidates] == "status") | allow_form_join]
        non_joiners_i <- setdiff(could_join, which(support_i))
        for (k in non_joiners_i) {
          WTR[i, k]     <- upd_bet(WTR[i, k], wtr_lr_betray)
          WTR_est[i, k] <- upd_bet(WTR_est[i, k], est_lr_betray)
        }
      }
    }
    
    # log bout
    hist[g, ] <- list(g, i, j, sum(support_i), sum(support_j), Si, Sj, p_i, win_i)
  }
  
  # Type-level payoff summary
  df_pay <- data.frame(id = seq_len(N), type = type, payoff = payoff)
  type_summary <- aggregate(payoff ~ type, data = df_pay, FUN = mean)
  
  list(
    history       = hist,
    strength      = strength,
    type          = type,
    WTR_final     = WTR,
    WTR_est_final = WTR_est,
    payoff        = payoff,
    type_summary  = type_summary
  )
}


# =========================
# QUICK DEMOS (fast)
# =========================

# A: small-stakes, high yield -> fewer coalitions -> form can survive
set.seed(1)
ex_A <- run_sim1(N = 60, G = 100,
                 init_p_status = 0.5,
                 strength_mean = 0.5, strength_sd = 0.25,
                 wtr_mean = 0, wtr_sd = 0.25,
                 allow_form_join = TRUE,
                 bw_weight = 1, fs_weight = 1,
                 cost_join = 0.05,
                 # learning rates (true WTR)
                 wtr_lr_win = 0.05, wtr_lr_cost = 0.05, wtr_lr_betray = 0.05,
                 # learning rates (estimated/common WTR)
                 est_lr_win = 0.05, est_lr_cost = 0.05, est_lr_betray = 0.05,
                 # anticipation strength for status agents (0 = ignore, 1 = fully trust)
                 anticip_alpha = 1,
                 seed = NULL)

## ---- quick summaries
print_sim_summary <- function(sim) {
  cat("\n=== Type-level mean payoff ===\n")
  print(aggregate(payoff ~ type, data = data.frame(type = sim$type, payoff = sim$payoff), mean))
  cat("\n=== Mean coalition size (per bout) ===\n")
  H <- sim$history
  cat(mean(H$n_i + H$n_j), "\n")
}

## ---- plotting helper
plot_sim <- function(sim) {
  H  <- sim$history
  df <- data.frame(type = sim$type, payoff = sim$payoff)
  
  # off-diagonal extractor for matrix histograms
  offdiag_vals <- function(M) M[row(M) != col(M)]
  
  par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
  
  # (1) Coalition sizes over time
  tot_support <- H$n_i + H$n_j
  plot(H$bout, tot_support, type = "l", lwd = 2, xlab = "Bout", ylab = "Total supporters",
       main = "Coalition size per bout")
  lines(H$bout, H$n_i, lwd = 1, lty = 2)
  lines(H$bout, H$n_j, lwd = 1, lty = 3)
  legend("topleft", c("Total", "i-side", "j-side"), lwd = c(2,1,1), lty = c(1,2,3), bty = "n")
  
  # (2) Win probability for i
  plot(H$bout, H$p_i, type = "l", lwd = 2, xlab = "Bout", ylab = "P(i wins)",
       main = "Win probability")
  
  # (3) Effective strengths
  matplot(H$bout, cbind(H$Si, H$Sj), type = "l", lwd = 2, lty = 1,
          xlab = "Bout", ylab = "Effective strength", main = "Effective strengths")
  legend("topleft", c("Si", "Sj"), lwd = 2, lty = 1, col = c("black","red"), bty = "n")
  
  # (4) Per-agent payoff by type
  boxplot(payoff ~ type, data = df, main = "Per-agent payoff by type",
          ylab = "Payoff", xlab = "Type")
  
  # (5) Mean payoff by type
  type_mean <- aggregate(payoff ~ type, data = df, mean)
  barplot(height = type_mean$payoff, names.arg = type_mean$type,
          ylab = "Mean payoff", main = "Mean payoff (type)", las = 1)
  
  # (6) Final WTR vs Estimated WTR distributions
  v_true <- offdiag_vals(sim$WTR_final)
  v_est  <- offdiag_vals(sim$WTR_est_final)
  rng    <- range(c(v_true, v_est), na.rm = TRUE)
  hist(v_true, breaks = 30, xlim = rng, freq = FALSE,
       col = rgb(0,0,1,0.35), border = NA,
       main = "Final WTR vs est-WTR", xlab = "Value")
  hist(v_est,  breaks = 30, xlim = rng, freq = FALSE,
       col = rgb(1,0,0,0.35), border = NA, add = TRUE)
  legend("topright", c("WTR_final", "WTR_est_final"),
         fill = c(rgb(0,0,1,0.35), rgb(1,0,0,0.35)), bty = "n")
}

## ---- run on your result
print_sim_summary(ex_A)
plot_sim(ex_A)

# B: moderate stakes/bouts, moderate yield -> coalitions frequent -> status tends to win
set.seed(2)
ex_B <- run_sim1B_exclusive(N=60, G=100, F=100, gamma=10,
                            yield_factor=0.25,
                            c_status_once=0.01,
                            init_p_status=0.5, seed=2)
print(ex_B$final); print(ex_B$metrics); tail(ex_B$traj, 5)

# C: high stakes, many bouts, low yield -> coalitions common -> status wins strongly
set.seed(3)
ex_C <- run_sim1B_exclusive(N=60, G=100, F=100, gamma=20,
                            yield_factor=0.10,
                            c_status_once=0.02,
                            init_p_status=0.5, seed=3)
print(ex_C$final); print(ex_C$metrics); tail(ex_C$traj, 5)

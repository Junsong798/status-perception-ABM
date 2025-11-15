# Coevolution of war, orders of intentionality, and coalition formation

# Author: Junsong Lu
# Version: 2025-10-11

# Libraries
library(tidyverse)

# Sources

# Parameters

# ====================== dynamic updating of WTR ===============================

# normal-inverse-gamma model

# prior
mu0 <- 1.9 # prior sample mean
k0 <- 5 # prior samples 

s20 <- .5 # prior sample variance
nu0 <- 5 # prior samples

n_prior <- 5000
inv_s2 <- rgamma(n_prior, shape = nu0 / 2, rate = nu0 * s20 / 2)
s2_prior <- 1 / inv_s2
hist(s2_prior)

mu_prior <- map_dbl(s2_prior, ~ rnorm(1, mu0, ./k0))
hist(mu_prior)
sd(mu_prior)

# data
n <- 10
true_mu <- 2
true_sd <- 1
y <- rnorm(n, true_mu, true_sd)
n <- length(y)
ybar <- mean(y)
s2 <- var(y)

# posterior
kn <- k0 + n
nun <- nu0 + n

mun <- (k0 * mu0 + n * ybar) / kn # mean
s2n <- (nu0 * s20 + (n - 1) * s2 + k0 * n * (ybar - mu0)^2 / (kn))/ (nun) # variance

# ============================ reciprocal altruism =============================





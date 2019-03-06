#' Load libraries.
library(dplyr)
library(ggplot2)
# Rule of 3 is a 95% one-sided confidence limit. 

# Laplace's rule of succession. The rule of succession states that the estimated probability of failure is (F + 1)/(N + 2). Where F is the number of failures.
# Laplace's rule of succession.
F <- 0
N <- 138
p <- (F + 1) / (N + 2)
x <- 1:1000
pe <- 1 - pbinom(0, size = x, prob = p)
ggplot(tibble(x, pe)) +
    geom_line(aes(x, pe, group = 1)) +
    geom_hline(yintercept = 0.95, linetype = "dashed")
sum(near(0.95, pe, tol = 0.0001))
x[near(0.95, pe, tol = 0.0001)]

#' Bayesian estimator
#' if we use beta prior with parameters a and b we can use formula (see Razzaghi, 2002):
#' Razzaghi, M. (2002). On the estimation of binomial success probability with zero occurrence in sample. Journal of Modern Applied Statistical Methods, 1(2), 41. http://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=1673&context=jmasm

eb_estimate <- function(x, a, b) {
    a / (a + b + x)
}

#'
#' Blood donors
bd <- 2 / 1e6
#' Sweden
sw <- 0.2 / 1e5
#' Our data
we <- list(pos = 0, ntry = 138)
#' IVF patients
ivf <- 2.3 / 1e4
#' injecting drug users
idu <- 3.2 / 100

#' Create prior parameters
obs <- c(bd, sw, ivf, idu)
values <- sample(obs, 1000, replace = TRUE)
m <- MASS::fitdistr(values, dbeta, start = list(shape1 = 1, shape2 = 10))

alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

#' Plot prior distribution
set.seed(11)
x <- seq(0.001, 0.1, 0.001)
y <- dbeta(x, alpha0, beta0)
tibble(x, y) %>% 
    ggplot() +
    geom_line(aes(x, y, group = 1)) +
    labs(title = "Prior distribution")

#' Estimate upper bound
x <- seq(1, 1000, by = 1)
xub <- eb_estimate(x, alpha0, beta0)

#' Plot upper bound estimate
as_tibble(cbind(x, p = xub)) %>% 
    ggplot() +
    geom_line(aes(x, p, group = 1)) +
    geom_vline(xintercept = 138, linetype = "dashed") +
    labs(y = "Probability with zero occurrence",
         x = "Sample size")

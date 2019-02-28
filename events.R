#' Load libraries.
library(dplyr)
library(ggplot2)

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

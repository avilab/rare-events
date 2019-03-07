#' ---
#' title: "Modeling rare events"
#' author: "Taavi Päll"
#' output: github_document
#' ---
#' 

#' ## Load libraries.
#+
library(dplyr)
library(ggplot2)

#' ## Rule of three (is next to useless)

#' > Rule of 3 is a 95% one-sided confidence limit.   
#' 
#' ### Laplace's rule of succession  
#' 
#' The rule of succession states that the estimated probability of failure is (F + 1)/(N + 2). 
#' Where F is the number of failures.   
#+
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

#' ## Bayesian estimator   
#' 
#' If we use beta prior with parameters a and b we can use formula (see Razzaghi, 2002):
#' Razzaghi, M. (2002). On the estimation of binomial success probability with zero occurrence in sample. Journal of Modern Applied Statistical Methods, 1(2), 41. http://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=1673&context=jmasm
#+
a <- 0.5
b <- 0.5
hist(rbeta(1000, a, b))

#' Naive Bayes 
#+
nb_estimate <- function(x) {
    a / (a + b + x)
    }

xub <- nb_estimate(x)
ggplot(tibble(x, xub)) +
    geom_line(aes(x, xub, group = 1)) +
    geom_vline(xintercept = 138, linetype = "dashed")

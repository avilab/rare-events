#' ---
#' title: "Prevalence of HTLVI/II infection in Estonian population: modeling zero events"
#' author: "Taavi Päll"
#' output: github_document
#' ---
#' 

#' ## Load libraries.
#+
library(dplyr)
library(ggplot2)

#' ## Rule of three (is next to useless)
#' "... concluded by Ludbrook and Lew (2009) "rule of threes" is "next to useless" and "rule of 3.6" (and 3.7) "have serious limitations – they are grossly inaccurate if the initial sample size is less than 50" and they do not recommend methods (3)-(6), suggesting rather to use proper Bayesian estimators (see below)."
#' citation from https://stats.stackexchange.com/questions/134380/how-to-tell-the-probability-of-failure-if-there-were-no-failures

#' 
#' ### Laplace's rule of succession  
#' 
#' The rule of succession states that the estimated probability of event is (F + 1)/(N + 2). 
#' Where F is the number of events.   
#+ rule-three
F <- 0
N <- 138
p <- (F + 1) / (N + 2)
x <- 1:1000
pe <- 1 - pbinom(0, size = x, prob = p)
ggplot(tibble(x, pe)) +
    geom_line(aes(x, pe, group = 1)) +
    geom_hline(yintercept = 0.95, linetype = "dashed") +
    labs(caption = "Dashed line, 95% probability of an event.")


#' Sample size at 95% probability of an event.
#+
x[near(0.95, pe, tol = 0.0001)]

#' ## Empirical Bayesian estimator   
#' 
#' If we use beta prior with parameters a and b we can use formula (see Razzaghi, 2002):
#' Razzaghi, M. (2002). On the estimation of binomial success probability with zero occurrence in sample. Journal of Modern Applied Statistical Methods, 1(2), 41. http://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=1673&context=jmasm     

#' Empirical Bayes updating function. Basu et al (1996) recommends a / (a + b + n) with informative prior, when some a priori knowledge is available. 
#+
eb_estimate <- function(x, a, b) {
    a / (a + b + x)
}

#' ### Prior frequencies
#' 
#' Known prior frequencies of HTLVI/II positive subjects in different subpopulations.
#' Blood donors.
bd <- 2 / 1e6
#' Sweden general population.
sw <- 0.2 / 1e5
#' Our data.
we <- list(pos = 0, ntry = 138)
#' IVF patients.
ivf <- 2.3 / 1e4
#' Injecting drug users.
idu <- 3.2 / 100

#' ### Prior parameters
#' 
#' Estimate parameters for prior from beta distribution.
#+ warning=FALSE
obs <- c(bd, sw, ivf, idu)
values <- sample(obs, 1000, replace = TRUE)
m <- MASS::fitdistr(values, dbeta, start = list(shape1 = 1, shape2 = 10))

#' Estimated shape parameters.
#+
coef(m)
alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

#' Plot prior distribution.
#+ eb-prior
bfun <- function(x) dbeta(x, alpha0, beta0)
ggplot(data = tibble(x = 0), mapping = aes(x = x)) + 
    stat_function(fun = bfun) + 
    xlim(0.001, 0.1)

#' Estimate upper bound for range of sample sizes.
#+ eb-estimate
ggplot(data = tibble(x = 0), mapping = aes(x = x)) + 
    stat_function(fun = eb_estimate, args = list(a = alpha0, b = beta0)) + 
    xlim(1, 1000) +
    geom_vline(xintercept = 138, linetype = "dashed") +
    labs(y = "Upper bound for probability of event",
         x = "Sample size",
         caption = "Dashed line, sample size in current study.")

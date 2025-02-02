library(mlpwr)
library(mirt)

# Defining intercepts and slopes
a <- c(1.04, 1.2, 1.19, 0.61, 1.31, 0.83, 1.46, 1.27, 0.51, 0.81)
d <- c(0.06, -1.79, -1.15, 0.88, -0.2, -1.87, 1.23, -0.08, -0.71, 0.6)

# Setting number of observations 
N <- 100

# Itemtype 
itemtype <- "2PL"

# Simulate Data
sim_data <- simdata(a = a, d = d, N = N, itemtype = itemtype)

# First 5 rows if simulated data
sim_data[1:5,]

# Fit 2PL model
mod <- mirt(sim_data)

# Rasch contsraint for items 1-4
constrained <- "F = 1-4
          CONSTRAIN = (1-4, a1)"
# Fit constrained model
mod_constrained <- mirt(sim_data, constrained)  # Fit 2PL with equal item discrimination

# Compare model fit
res <- anova(mod_constrained, mod)  

res$p[2] < 0.01  # extract significance

simfun_irt1 <- function(N) {
  
  # generate data
  dat <- simdata(a = c(1.04, 1.2, 1.19, 0.61, 1.31,
                       0.83, 1.46, 1.27, 0.51, 0.81), d = c(0.06,
                                                            -1.79, -1.15, 0.88, -0.2, -1.87, 1.23, -0.08,
                                                            -0.71, 0.6), N = N, itemtype = "2PL")
  
  # test hypothesis
  mod <- mirt(dat)  # Fit 2PL Model
  constrained <- "F = 1-4
          CONSTRAIN = (1-4, a1)"
  mod_constrained <- mirt(dat, constrained)  # Fit 2PL with equal slopes
  
  res <- anova(mod_constrained, mod)  # perform model comparison
  res$p[2] < 0.01  # extract significance
}

example.simfun("irt1")
set.seed(123)
t <- proc.time()
res <- find.design(simfun = simfun_irt1, boundaries = c(40,
                                                        100), power = .95, evaluations = 2000)
proc.time()-t
summary(res)
plot(res)

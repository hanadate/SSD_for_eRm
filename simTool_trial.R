# https://cran.r-project.org/web/packages/simTool/vignettes/simTool.html
library(simTool)
# A first example
regData <- function(n, SD) {
  x <- seq(0, 1, length = n)
  y <- 10 + 2 * x + rnorm(n, sd = SD)
  tibble(x = x, y = y)
}

eval_tibbles(
  expand_tibble(fun = "regData", n = 5L, SD = 1:2),
  expand_tibble(proc = "lm", formula = c("y~x", "y~I(x^2)")),
  post_analyze = broom::tidy,
  summary_fun = list(mean = mean, sd = sd),
  group_for_summary = "term",
  replications = 3
)

# Defining the data.frames for data generation and analysis
# generation function
print(dg <- dplyr::bind_rows(
  expand_tibble(fun = "rexp", n = c(10L, 20L), rate = 1:2),
  expand_tibble(fun = "rnorm", n = c(10L, 20L), mean = 1:2)
))
# analyzing function
print(pg <- dplyr::bind_rows(
  expand_tibble(proc = "min"),
  expand_tibble(proc = "mean", trim = c(0.1, 0.2))
))

# The workhorse eval_tibbles
dg <- expand_tibble(fun = "rnorm", n = 10, mean = 1:2)
pg <- expand_tibble(proc = "min")
eg <- eval_tibbles(data_grid = dg, proc_grid = pg, replications = 2)
eg

# Parameter replications
eg <- eval_tibbles(data_grid = dg, proc_grid = pg, replications = 3)
eg

# Parameter discard_generated_data
eg <- eval_tibbles(data_grid = dg, proc_grid = pg, replications = 1)
eg$simulation

# Parameter summary_fun
dg <- expand_tibble(fun = "runif", n = c(10, 20, 30))
pg <- expand_tibble(proc = c("min", "max"))
eval_tibbles(
  data_grid = dg, proc_grid = pg, replications = 1000,
  summary_fun = list(mean = mean)
)
eval_tibbles(
  data_grid = dg, proc_grid = pg, replications = 1000,
  summary_fun = list(mean = mean, sd = sd)
)

# Parameter post_analyze
eval_tibbles(
  expand_tibble(fun = "regData", n = 5L, SD = 1:2),
  expand_tibble(proc = "lm", formula = c("y~x", "y~I(x^2)")),
  replications = 2
)
eval_tibbles(
  expand_tibble(fun = "regData", n = 5L, SD = 1:2),
  expand_tibble(proc = "lm", formula = c("y~x", "y~I(x^2)")),
  post_analyze = purrr::compose(function(mat) mat["(Intercept)", "Estimate"], coef, summary.lm),
  replications = 2
)

# Parameter group_for_summary
presever_rownames <- function(mat) {
  rn <- rownames(mat)
  ret <- tibble::as_tibble(mat)
  ret$term <- rn
  ret
}

eval_tibbles(
  expand_tibble(fun = "regData", n = 5L, SD = 1:2),
  expand_tibble(proc = "lm", formula = c("y~x", "y~I(x^2)")),
  post_analyze = purrr::compose(presever_rownames, coef, summary),
  replications = 3
)

eval_tibbles(
  expand_tibble(fun = "regData", n = 5L, SD = 1:2),
  expand_tibble(proc = "lm", formula = c("y~x", "y~I(x^2)")),
  post_analyze = purrr::compose(presever_rownames, coef, summary),
  summary_fun = list(mean = mean, sd = sd),
  group_for_summary = "term",
  replications = 3
)

# Parameter ncpus and cluste_seed
eval_tibbles(
  data_grid = dg, proc_grid = pg, replications = 10,
  ncpus = 2, summary_fun = list(mean = mean)
)

# Parameter cluster
library(parallel)
cl <- makeCluster(rep("localhost", 2), type = "PSOCK")
eval_tibbles(
  data_grid = dg, proc_grid = pg, replications = 10,
  cluster = cl, summary_fun = list(mean = mean)
)
stopCluster(cl)

# Parameter cluster_libraries and cluster_global_objects
library(boot)
ratio <- function(d, w) sum(d$x * w) / sum(d$u * w)
city.boot <- boot(city, ratio,
                  R = 999, stype = "w",
                  sim = "ordinary"
)
boot.ci(city.boot,
        conf = c(0.90, 0.95),
        type = c("norm", "basic", "perc", "bca")
)
returnCity <- function() {
  city
}
bootConfInt <- function(data) {
  city.boot <- boot(data, ratio,
                    R = 999, stype = "w",
                    sim = "ordinary"
  )
  boot.ci(city.boot,
          conf = c(0.90, 0.95),
          type = c("norm", "basic", "perc", "bca")
  )
}

dg <- expand_tibble(fun = "returnCity")
pg <- expand_tibble(proc = "bootConfInt")
eval_tibbles(dg, pg,
             replications = 10, ncpus = 2,
             cluster_libraries = c("boot"),
             cluster_global_objects = c("ratio")
)

# Parameter envir
# masking summary from the base package
summary <- function(x) tibble(sd = sd(x))
g <- function(x) tibble(q0.1 = quantile(x, 0.1))
someFunc <- function() {
  summary <- function(x) tibble(sd = sd(x), mean = mean(x))
  
  dg <- expand_tibble(fun = "runif", n = 100)
  pg <- expand_tibble(proc = c("summary", "g"))
  
  # the standard is to use the global
  # environment, hence summary defined outside
  # of someFunc() will be used
  print(eval_tibbles(dg, pg))
  cat("--------------------------------------------------\n")
  # will use the local defined summary, but g
  # from the global environment, because
  # g is not locally defined.
  print(eval_tibbles(dg, pg, envir = environment()))
}
someFunc()

# .truth-functionality
dg <- expand_tibble(fun = c("rnorm"), mean = c(1,1000), sd = c(1,10), n = c(10L, 100L))
pg <- expand_tibble(proc = "quantile", probs = 0.975)
post_ana <- function(q_est, .truth){
  tibble::tibble(bias = q_est - stats::qnorm(0.975, mean = .truth$mean, sd = .truth$sd))
}
eval_tibbles(dg, pg, replications = 10^3, discard_generated_data = TRUE,
             ncpus = 2,
             post_analyze = post_ana,
             summary_fun = list(mean = mean))



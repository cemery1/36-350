generate_data <- function(n, p) {
  covariates <- replicate(p, rnorm(n))
  response <- rnorm(n)
  return (list(covariates = covariates, response = response))
}

model_select <- function(covariates, response, cutoff) {
  model_full <- lm(response ~ covariates)
  inds <- which(summary(model_full)$coefficients[-1, "Pr(>|t|)"] <= cutoff)
  if (length(inds) == 0) return (vector())
  else {
    model_reduced <- lm(response ~ covariates[, inds])
    return (summary(model_reduced)$coefficients[-1, "Pr(>|t|)"])
  }
}

run_simulation <- function(n_trials, n, p, cutoff) {
  result <- vector()
  for (i in 1:n_trials) {
    data <- generate_data(n, p)
    result <- c(result, model_select(data$covariates, data$response, cutoff))
  }
  hist(result)
  return ()
}

par(mfrow = c(3,3))
for (n in c(100, 1000, 10000)) {
  for (p in c(10, 20, 50)) {
    run_simulation(100, n, p, 0.05)
  }
}

# It does not appear to be the case that the p-values are uniformly distributed
# between 0 and 1 (probably because we filtered out every p-value that was 
# greater than 0.05 in the original regression model)

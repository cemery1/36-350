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

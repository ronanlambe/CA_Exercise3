## Exercise 3

### part 1
```{r}
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))
df
```


### part 2
```{r}
nll_lm <- function(data, par) {
  # response and predictors
  y <- data$y
  X <- as.matrix(cbind(1, data[, -1]))  # intercept column (1s)
  
  # Parameters
  beta <- par[-length(par)]  # coefficients
  sigma <- par[length(par)]  # sd            
  
  # Residuals
  residuals <- y - X %*% beta           
  
  # Negative log-likelihood
  nll <- -sum(dnorm(residuals, mean = 0, sd = sigma, log = TRUE))
  
  return(nll)
}
```


### part 3
```{r}
# intercept and coeff
init_beta <- c(mean(df$y), rep(0, 3))
# sd
init_sigma <- sd(df$y)
# combine
start_par <- c(init_beta, init_sigma)   

# Bounds no beta, sigma > 0
lower_bounds <- c(rep(-Inf, length(init_beta)), 1e-6)
upper_bounds <- c(rep(Inf, length(init_beta)), Inf)

# optimise
result <- optim(par = start_par,
                fn = nll_lm,
                data = df,
                method = "L-BFGS-B",
                lower = lower_bounds,
                upper = upper_bounds,
                control = list(maxit = 1000)
)

# mles
mle_beta <- result$par[-length(result$par)]   # Coefficients
mle_sigma <- result$par[length(result$par)]  # Standard deviation

#results
cat("MLE Coefficients (Beta):", mle_beta, "\n")
cat("MLE Sigma:", mle_sigma, "\n")
```


### part 4
Usind the negative log-likelihood function is necessary for numerical stability and compatibility with optimization algorithms. It also gives us better control over the statistical model.


### part 5

${\beta}$ caluclated with matrix
```{r}

X <- as.matrix(cbind(1, df[, c("x1", "x2", "x3")]))  
y <- df$y

# beta_hat 
beta_hat_matrix <- solve(t(X) %*% X) %*% t(X) %*% y

# print
cat("Coefficients (Matrix Operations):", beta_hat_matrix, "\n")

#compare
cat("Coefficients (MLE using optim):", mle_beta, "\n")
```


### part 6
```{r}

# residual sum of squares 
residuals <- y - X %*% beta_hat_matrix  # matrix-based beta
rss <- sum(residuals^2)                # rss

# Estimate sigma 
n <- nrow(X)   # observations
p <- ncol(X)   # coefficients 
sigma_hat_matrix <- sqrt(rss / (n - p))  # sd of residuals

# results
cat("Sigma (Matrix Operations):", sigma_hat_matrix, "\n")
cat("Sigma (MLE optim):", sigma_mle, "\n")

```


### part 7
Differences are caused by the degrees of freedom adjustment in OLS, MLE doesnt apply this.

### part 8
```{r}
result <- optim(
  par = start_par,
  fn = nll_lm,
  data = df,
  method = "L-BFGS-B",
  lower = lower_bounds,
  upper = upper_bounds,
  hessian = TRUE,
  control = list(maxit = 1000)
)

# Hessian matrix
hessian_matrix <- result$hessian

# Compute the covariance matrix as the inverse of the Hessian
cov_matrix <- solve(hessian_matrix)

# Standard errors 
standard_errors <- sqrt(diag(cov_matrix))

# Results
cat("Standard Errors of Coefficients:", standard_errors[-length(standard_errors)], "\n")
cat("Standard Error of Sigma:", standard_errors[length(standard_errors)], "\n") 
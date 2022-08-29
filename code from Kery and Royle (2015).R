# Code from Kery and Royle (2015)
# Choose sample sizes and prepare obs. data array y
set.seed(1)                   # So we all get same data set
M <- 100                      # Number of sites
J <- 3                        # Number of repeated abundance measurements
C <- matrix(NA, nrow = M, ncol = J) # to contain the observed data

# Create a covariate called vegHt
vegHt <- sort(runif(M, -1, 1)) # sort for graphical convenience

# Choose parameter values for abundance model and compute lambda
beta0 <- 0                    # Log-scale intercept
beta1 <- 2                    # Log-scale slope for vegHt
lambda <- exp(beta0 + beta1 * vegHt) # Expected abundance

# Draw local abundance
N <- rpois(M, lambda)

# Create a covariate called wind
wind <- array(runif(M * J, -1, 1), dim = c(M, J))

# Choose parameter values for measurement error model and compute detectability
alpha0 <- -2                        # Logit-scale intercept
alpha1 <- -3                        # Logit-scale slope for wind
p <- plogis(alpha0 + alpha1 * wind) # Detection probability

# Take J = 3 abundance measurements at each site
for(j in 1:J) {
  C[,j] <- rbinom(M, N, p[,j])
}

# Create factors
time <- matrix(rep(as.character(1:J), M), ncol = J, byrow = TRUE)
hab <- c(rep("A", 33), rep("B", 33), rep("C", 34))  # assumes M = 100

# Bundle data
# NIMBLE: For full flexibility, we could separate this list
#         into constants and data lists.  For simplicity we will keep
#         it as one list to be provided as the "constants" argument.
#         See comments about how we would split it if desired.
win.data <- list(
  ## NIMBLE: C is the actual data
  C = C,
  ## NIMBLE: Covariates can be data or constants
  ##         If they are data, you could modify them after the model is built
  wind = wind,
  vegHt = vegHt,
  XvegHt = seq(-1, 1,, 100), # Used only for derived quantities
  Xwind = seq(-1, 1,,100),   # Used only for derived quantities
  ## NIMBLE: The rest of these are constants, needed for model definition
  ## We can provide them in the same list and NIMBLE will figure it out.
  M = nrow(C),
  J = ncol(C),
  hab = as.numeric(factor(hab))
)
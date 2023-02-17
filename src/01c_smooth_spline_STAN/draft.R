## Script to employ the method's presented in Sophie Lee's paper;
## "A Bayesian modelling framework to quantify multiple sources of spatial variation for disease mapping"
## https://doi.org/10.1098/rsif.2022.0440

##########################################################################

#Load the Cases data
load("Cases_Data.RData")

#Load the Boundaries data
load("Boundaries_Data.RData")

### Scale coordinates to lie between [0, 1] 
Case_Rates_Data <- Case_Rates_Data %>% 
  mutate(y_scale = (centroid_y - min(centroid_y))/(max(centroid_y) - min(centroid_y)),
         x_scale = (centroid_x - min(centroid_x))/(max(centroid_x) - min(centroid_x)))


#First, we are going to look at the spatial variation for just one Week. Let's say Week 50; (very low cases) Week 85 for very high!
Reduced_Data <- filter(Case_Rates_Data, Week == 50)


## Use the smooth_create function to create smooth surface over coordinates
#### Function to create a smooth surface (see ?te) ####
## x, z = scaled connectivity coordinates
smooth_create <- function(x, z, sx = 0.3, sz = 0.4) { 
  
  #x <- x*20
  
  (pi**sx*sz)*(1.2*exp(-(x - 0.2)^2/sx^2-(z - 0.3)^2/sz^2)+
                 0.8*exp(-(x - 0.7)^2/sx^2-(z - 0.8)^2/sz^2))
  
}
smooth_sim <- smooth_create(Reduced_Data$x_scale, Reduced_Data$y_scale)
# Centre estimates around 0 and scale
smooth_sim <- (smooth_sim - mean(smooth_sim)) * 10
## Plot smooth function on England map to check simulation
Boundaries %>% 
  mutate(smooth = smooth_sim) %>% 
  ggplot( ) +
  geom_sf(aes(fill = smooth), lwd =  .05) +
  scale_fill_viridis_c(name = expression(u[i])) +
  theme_void()
#That doesn't look particularly smooth... but I think it's okay.
plot(Reduced_Data$centroid_x, Reduced_Data$centroid_y, xlim = c(0, 1e+06), ylim = c(0, 1e+06))
plot(Reduced_Data$x_scale, Reduced_Data$y_scale)
Reduced_Data$smooth <- smooth_sim

plot_data <- Reduced_Data[,c(1,82)]
colnames(plot_data) <- c("CODE", "smooth")

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = smooth), lwd =  .05) +
  scale_fill_viridis_c(name = expression(u[i])) +
  theme_void()
#Now it's smooth!


## Plot E = offset (LTLA population)
offset_map <- Boundaries %>% 
  mutate(E = Reduced_Data$Population) %>% 
  ggplot( ) +
  geom_sf(aes(fill = E), lwd = .05) +
  scale_fill_viridis_c(name = "Offset", trans = "log1p",
                       breaks = c(0, 5, 15, 30)) +
  theme_void()

#############################
#NOW FOR ACTUAL MODELLING
###########################

#### Step 1: Fit the spatial smooth model ####
## Fit GAM to obtain sensible priors for lambdas (sp)

sp_check <- gam(Week_Cases ~  s(y_scale, x_scale, k = 10, bs = "tp"), offset = log(Population),
                family = "poisson", data = Reduced_Data, method = "REML")

summary(sp_check)
#Significant...
sp_check$sp
#~ 0.48, which is quite high, but not weird or anything.

#Let's just plot the cases first:
plot_data <- Reduced_Data[,c(1,4,8)]
colnames(plot_data) <- c("CODE", "Population", "Week_Cases")

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Week_Cases), lwd =  .05) +
  scale_fill_viridis_c(name = "Week 50 Cases") +
  theme_void()

#And plot divided by population
Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Week_Cases/Population), lwd =  .05) +
  scale_fill_viridis_c(name = "Week 50 Cases / Population") +
  theme_void()


#Now plot the fit out of the GAM
plot_data$GAM_fit <- sp_check$fitted.values

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = GAM_fit/Population), lwd =  .05) +
  scale_fill_viridis_c(name = "Week 50 GAM Cases / Population") +
  theme_void()

#What if I give the smoothing spline more basis functions to work with?
sp_check_20 <- gam(Week_Cases ~  s(y_scale, x_scale, k = 20, bs = "tp"), offset = log(Population),
                   family = "poisson", data = Reduced_Data, method = "REML")

summary(sp_check_20)
#Significant...
sp_check_20$sp
#~ 0.235, which makes sense, twice the penalty, with double the basis functions
plot_data$GAM_fit_20 <- sp_check_20$fitted.values

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = GAM_fit_20/Population), lwd =  .05) +
  scale_fill_viridis_c(name = "Week 50 GAM (20b) Cases / Population") +
  theme_void()
#This DOES look better, but is ofc a bit... rougher
#We can do an ANOVA to have a look at if this 20 version is significantly better:
anova(sp_check, sp_check_20, test = "Chisq")
#HUGELY so. What about 40 basis functions?

sp_check_40 <- gam(Week_Cases ~  s(y_scale, x_scale, k = 40, bs = "tp"), offset = log(Population),
                   family = "poisson", data = Reduced_Data, method = "REML")

summary(sp_check_40)
#Significant...
sp_check_40$sp
#~ 0.235, which makes sense, twice the penalty, with double the basis functions
plot_data$GAM_fit_40 <- sp_check_40$fitted.values

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = GAM_fit_40/Population), lwd =  .05) +
  scale_fill_viridis_c(name = "Week 50 GAM (40b) Cases / Population") +
  theme_void()
#This actually doesn't look too different, just a little tighter maybe
anova(sp_check_20, sp_check_40, test = "Chisq") #Still hugely better apparently.
#Warning message:
#using F test with a 'poisson' family is inappropriate 
#See: https://katrienantonio.github.io/Risk-modelling-in-insurance/glms.html
#instead:
anova(sp_check_20, sp_check_40, test = "Chisq")
#Still way more significant


#NOTE WE ONLY DO THIS TO obtain sensible priors for lambdas (sp)
#But also of course to get an idea of how well the smoothing spline will perform.

###################################################################################
# NOW FOR THE BAYESIAN MODELLING
# Start with just distance-based, and heterogeneous noise.

### Fit a model with a smooth spatial random effect and an IID random
## effect in NIMBLE to data 

# Use mgcv to extract basis functions for the smooth term
jd_smooth <- jagam(Week_Cases ~  s(y_scale, x_scale, k = 40, bs = "tp"),
                   offset = log(Population),
                   # Save JAGS code to create model
                   file = "jagam_ten.txt", 
                   # Set prior for smoothing function (in this case, the spatial smooth)
                   sp.prior = "gamma", 
                   family = poisson,
                   # If T, smooths are re-parameterised to have an iid Gaussian prior (not appropriate here)
                   # diagonalize cannot be used with >1 dimension
                   diagonalize = F, 
                   data = Reduced_Data)

# Extract basis functions to use as linear predictors in the model
X <- jd_smooth$jags.data$X

# Set constants for model (number obs & number of coefficients)
Consts <- list(n = length(Reduced_Data$Week_Cases), m = ncol(X))

### Write model formula
Model <- nimbleCode({ 
  
  # u = spatial smooth term (using basis funtions)
  u[1:n] <- X[1:n, 2:m] %*% b[2:m] #b is the coefficients, beta
  
  for (i in 1:n) { 
    # y = number of cases
    y[i] ~ dpois(mu[i]) 
    
    log(mu[i]) <- b[1] + u[i] + v[i] + log(e[i])
    
    # v = iid random effect
    v[i] ~ dnorm(0, sd = sig_re)
  } 
  
  # Priors
  # Random effect SD
  sig_re ~ dexp(.1)
  
  # Intercept
  b[1] ~ dnorm(0, sd = 5) 
  
  ## prior for sd(smooth function)
  K1[1:(m-1), 1:(m-1)] <- S1[1:(m-1), 1:(m-1)] * lambda[1] + 
    S1[1:(m-1), m:(2*(m-1))] * lambda[2]
  
  # Prior for smooth coefficients
  b[2:m] ~ dmnorm(zero[2:m], K1[1:(m-1), 1:(m-1)]) 
  
  ## smoothing parameter priors 
  for (i in 1:2) {
    # truncate lambdas to avoid simulations getting stuck
    lambda[i] ~ T(dgamma(.05, .005), 0, 5)
  }
} )

# Convert jagam data into data suitable for nimble
nimbleData <- list(y = Reduced_Data$Week_Cases, X = X, zero = jd_smooth$jags.data$zero, 
                   S1 = jd_smooth$jags.data$S1, e = Reduced_Data$Population)


# Set initial values for MCMC
#inits <- list(b = rnorm(ncol(X),sd = 1), lambda = c(3, 3), 
#              sig_re = runif(1), v = rnorm(nrow(Reduced_Data), 1))
#Maybe try starting from the b of the gam fit...
inits <- list(b = colMeans(X), lambda = c(3, 3), 
              sig_re = runif(1), v = rnorm(nrow(Reduced_Data), 1))
##Doing this did help a LOT actually


# Sets up model in nimble code
nimbleModel <- nimbleModel(code = Model, name = 'nimbleModel', 
                           constants = Consts, data = nimbleData, 
                           inits = inits)

# Tell model which parameter to estimate and return
MCMCconfig <- configureMCMC(nimbleModel,
                            monitors=c("b","lambda", "u", "v",
                                       "sig_re", "mu"),
                            # Return WAIC to compare models
                            enableWAIC = TRUE)


# Build the model
modelMCMC <- buildMCMC(MCMCconfig)

compiled_model <- compileNimble(nimbleModel)

compiled_model_MCMC <- compileNimble(modelMCMC, project = nimbleModel)

results <- runMCMC(compiled_model_MCMC, thin = 100, 
                   niter = 1250000, nburnin = 50000, 
                   nchains = 3, inits=inits, progressBar = T, 
                   samplesAsCodaMCMC = T, WAIC = TRUE)

#20min:40secs to run thin = 100, 
#niter = 1250000, nburnin = 250000,

## Check convergence using Gelman-Rubin diagnostics with coda 
GR.diag <- gelman.diag(results$samples, multivariate = F)
sum(GR.diag$psrf[, "Point est."] > 1.1)
# Our u definitely did NOT converge! or b, or lambda. Anything but mu basically.
#This was improved significantly by doing 10* the samples. Only 50 over 1.1 now, compared to 750
#Let's look at some plots
plot(results$samples[ , "b[2]"])

# Save the model object
write_rds(results, 
          file = "output/results.rds")


#### Extract predicted intercept from models ####
## Spatial smooth model

## Extract simulations of the intercept
b_sim <- do.call(rbind, results$samples)[ , "b[1]"]

# Return mean and 95% credible interval & format 
b_est <- data.table(b_est = mean(b_sim), 
                    b_lq = quantile(b_sim, .025),
                    b_uq = quantile(b_sim, .975),
                    b_format = paste0(round(mean(b_sim), 3), " (",
                                      round(quantile(b_sim, .025), 3), 
                                      ", ", round(quantile(b_sim, .975), 3), ")"))
b_est$b_format
#### Extract estimates for phi/mixing parameters ####
n <- nrow(Boundaries)

## Spatial smooth model
# Return column numbers with structured random effect simulations
umin <- which(colnames(results$samples[[1]]) == "u[1]")
umax <- which(colnames(results$samples[[1]]) == paste0("u[", n, "]"))

# Return column numbers with unstructured random effect simulations
vmin <- which(colnames(results$samples[[1]]) == "v[1]")
vmax <- which(colnames(results$samples[[1]]) == paste0("v[", n, "]"))


# Extract simulations of structured random effect
# A 28,500 x 356 matrix
u_mat <- do.call(rbind, results$samples)[, umin:umax]
# Estimate the variance of each simulation (OVER ROWS)
u_var <- apply(u_mat, 1, var)

# Extract simulations of unstructured random effect
v_mat <- do.call(rbind, results$samples)[,vmin:vmax]
# Estimate the variance of each simulation
v_var <- apply(v_mat, 1, var)

# Calculate the proportion of variance explained by the structured term
propn_spat <- u_var/(u_var + v_var)

re_vars <- data.table(spat_var = u_var, 
                      iid_var = v_var,
                      propn_var = propn_spat)

## Combine the estimated phi from each model and calculate mean and 95% CI
smooth_phi_est <- re_vars %>% 
  summarise(phi_est = mean(propn_var),
            phi_lq = quantile(propn_var, .025),
            phi_uq = quantile(propn_var, .975))

#Cool, so estimated that ~ 42.5% of the "noise" is spatial!
paste0(round(mean(smooth_phi_est$phi_est), 3), " (",
       round(smooth_phi_est$phi_lq, 3), 
       ", ", round(smooth_phi_est$phi_uq, 3), ")")
#### Calculate model comparison statistics ####
## WAIC
## Smooth model
waic_smooth <- results$WAIC$WAIC


## Mean absolute error
## Smooth model
# Extract predicted values from smooth model

# Return column numbers with lambda simulations
lammin <- which(colnames(results$samples[[1]]) == "mu[1]")
lammax <- which(colnames(results$samples[[1]]) == paste0("mu[", n, "]"))

## # Extract simulations of lambda
lam_mat <- do.call(rbind, results$samples)[ , lammin:lammax]
# Estimate mean from each simulation
lam_mean <- apply(lam_mat, 2, mean)

mu_est <- lam_mean

mae_smooth <- mean(abs(Reduced_Data$Week_Cases - mu_est))
#That's.... slightly outrageously good.
plot(log(Reduced_Data$Week_Cases), log(mu_est))

#Let's plot the spatial bit:

# Estimate mean from each simulation
u_mean <- apply(u_mat, 2, mean)
v_mean <- apply(v_mat, 2, mean)
exp_u_mean <- exp(u_mean)
exp_v_mean <- exp(v_mean)
plot_data <- Reduced_Data[,c(1,4,8)]
plot_data$u_mean <- exp_u_mean
plot_data$v_mean <- exp_v_mean
colnames(plot_data) <- c("CODE", "Population", "Week_Cases", "u_mean", "v_mean")

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = u_mean), lwd =  .05) +
  scale_fill_viridis_c(name = "u_mean") +
  theme_void()

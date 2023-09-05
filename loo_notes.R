test2 <- rstan::loo(stanfit)

loo::pareto_k_table(test)
loo::pareto_k_values(test)

loo::psis_n_eff_values(test)
plot(test)

#Higher elpd_loo suggests a better model fit.
#Smaller p_loo suggests a simpler model.
#Lower looic indicates a model that balances fit and complexity effectively.

#Comparing my first fits of 9i,j,k,l,m,n:

#i) Error: 10.06 (no zeta no x no theta)
#Some big uncertainty in the steps
#j) Error: 9.97 (no zeta no x)
# Theta is very badly mixed
#k) Error: 9.96 (no zeta no mobility)
# Everything badly mixed, only relevant betas were Delta

#l) Error: 10.16 (constant zeta, no x, no theta)
# zeta: ~ 0.005
#All looks well mixed but not really a change on i
#m) Error: 9.72 (constant zeta, no x)
# zeta: ~ 0.07 (much higher...)
#theta still very badly mixed.





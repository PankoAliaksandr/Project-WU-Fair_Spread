# Download package "Sim.DiffProc", version 2.5
# library(devtools)
# install_version("Sim.DiffProc", version = "2.5", repos = "http://cran.us.r-project.org")

# Libraries
library(Sim.DiffProc)

# Quarterly premium payments
paym_times = seq(from = 0.25, to = 5, by = 0.25)
paym_number = 20

# Parameters for CIR function that simulates the process
N = 1000
M = 100
t0 = 0
x0 = 0.03
theta = 1
r = 0.04
sigma = 0.2

# Other parameters
rate = 0.01
LGD = 0.6
delta_t = 0.25

# Vectors to store intermediate results
gamma_integrals = rep(NA, paym_number)
exponents_of_rate = rep(NA, paym_number)
exponents_of_gamma = rep(NA, paym_number)

# Loop over all premium payments
for (i in 1:paym_number) 
{
  t = paym_times[i]
  # Simulate CIR process
  CIR_result = CIR(N=N, M=M, t0=t0, T=t, x0=x0, theta=theta, r=r, sigma=sigma)
  # Average process trajectory
  gamma_average = mean(CIR_result$X.mean)
  # Monte Carlo integration
  gamma_integrals[i] = gamma_average*t
  # Apply exponent function
  exponents_of_gamma[i] = exp(-gamma_integrals[i])
  exponents_of_rate[i] = exp(-rate*t)
}

fair_spread = LGD*(exponents_of_rate[1]-exponents_of_rate[paym_number]*exponents_of_gamma[paym_number] + exponents_of_gamma[1:(paym_number-1)] %*% (exponents_of_rate[2:paym_number]-exponents_of_rate[1:(paym_number-1)])) / (delta_t*exponents_of_rate%*%exponents_of_gamma) 
print(fair_spread)


# Increase x0
x0_increased = 0.3

# Loop over all premium payments
for (i in 1:paym_number) 
{
  t = paym_times[i]
  # Simulate CIR process
  CIR_result = CIR(N=N, M=M, t0=t0, T=t, x0=x0_increased, theta=theta, r=r, sigma=sigma)
  # Average process trajectory
  gamma_average = mean(CIR_result$X.mean)
  # Monte Carlo integration
  gamma_integrals[i] = gamma_average*t
  # Apply exponent function
  exponents_of_gamma[i] = exp(-gamma_integrals[i])
  exponents_of_rate[i] = exp(-rate*t)
}

fair_spread = LGD*(exponents_of_rate[1]-exponents_of_rate[paym_number]*exponents_of_gamma[paym_number] + exponents_of_gamma[1:(paym_number-1)] %*% (exponents_of_rate[2:paym_number]-exponents_of_rate[1:(paym_number-1)])) / (delta_t*exponents_of_rate%*%exponents_of_gamma) 
print(fair_spread)


# Increase r
r_increased = 0.4

# Loop over all premium payments
for (i in 1:paym_number) 
{
  t = paym_times[i]
  # Simulate CIR process
  CIR_result = CIR(N=N, M=M, t0=t0, T=t, x0=x0, theta=theta, r=r_increased, sigma=sigma)
  # Average process trajectory
  gamma_average = mean(CIR_result$X.mean)
  # Monte Carlo integration
  gamma_integrals[i] = gamma_average*t
  # Apply exponent function
  exponents_of_gamma[i] = exp(-gamma_integrals[i])
  exponents_of_rate[i] = exp(-rate*t)
}

fair_spread = LGD*(exponents_of_rate[1]-exponents_of_rate[paym_number]*exponents_of_gamma[paym_number] + exponents_of_gamma[1:(paym_number-1)] %*% (exponents_of_rate[2:paym_number]-exponents_of_rate[1:(paym_number-1)])) / (delta_t*exponents_of_rate%*%exponents_of_gamma) 
print(fair_spread)



# Increase sigma
sigma_increased = 0.28

# Loop over all premium payments
for (i in 1:paym_number) 
{
  t = paym_times[i]
  # Simulate CIR process
  CIR_result = CIR(N=N, M=M, t0=t0, T=t, x0=x0, theta=theta, r=r, sigma=sigma_increased)
  # Average process trajectory
  gamma_average = mean(CIR_result$X.mean)
  # Monte Carlo integration
  gamma_integrals[i] = gamma_average*t
  # Apply exponent function
  exponents_of_gamma[i] = exp(-gamma_integrals[i])
  exponents_of_rate[i] = exp(-rate*t)
}

fair_spread = LGD*(exponents_of_rate[1]-exponents_of_rate[paym_number]*exponents_of_gamma[paym_number] + exponents_of_gamma[1:(paym_number-1)] %*% (exponents_of_rate[2:paym_number]-exponents_of_rate[1:(paym_number-1)])) / (delta_t*exponents_of_rate%*%exponents_of_gamma) 
print(fair_spread)





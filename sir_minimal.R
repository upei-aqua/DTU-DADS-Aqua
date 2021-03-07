# source:
# https://www.r-bloggers.com/sir-model-of-epidemics/

# The SIR model divides the population to three compartments: Susceptible,
# Infected and Recovered. If the disease dynamic fits the SIR model, then the
# flow of individuals is one direction from the susceptible group to infected
# group and then to the recovered group. All individuals are assumed to be
# identical in terms of their susceptibility to infection, infectiousness if
# infected and mixing behaviour associated with disease transmission.
#
# We defined:
#     (S_t) = the number of susceptible individuals at time t
#     (I_t) = the number of infected individuals at time t
#     (R_t) = the number of recovered individuals at time t
#
# Suppose on average every infected individual will contact (gamma) person, and
# (kappa) percent of these (gamma) person will be infected. Then on average
# there are (beta = gamma times kappa) person will be infected an infected
# individual.  So with infected number (I_t) , they will infected (beta I_t)
# individuals. Since not all people are susceptible, this number should multiple
# to the percentage of susceptible individuals. Therefore, (I_t) infected
# individuals will infect (beta frac{S_t}{N} I_t) individuals.  Another
# parameter (alpha) describes the percentage of infected individuals to recover
# in a time period. That is on average, it takes (1/alpha) periods for an
# infected person to recover.

# ---- Define function ---------------------------------------------------------
sim_sirmod <- function(N, I0, beta, alpha, t) {

  # initialize
  S <- numeric(t)
  I <- numeric(t)
  S[1] <- N - I0
  I[1] <- I0

  # iterate through time
  for (i in 2:t) {

      S[i] <- S[i - 1] - beta * S[i - 1] / N * I[i - 1]
      I[i] <- I[i - 1] + beta * S[i - 1] / N * I[i - 1] - alpha * I[i - 1]

      if (I[i] < 1 || S[i] < 1) break
  }

  # put everything into a dataframe
  df <- data.frame(time = 1:t,
                   S    = S,
                   I    = I,
                   R    = N - S - I)

  # get rid of empty time points
  df <- df[S > 1 & I > 1, ]

  # put additional calculations in df
  nr      <- nrow(df)
  rate    <- df$I[2:nr] / df$I[1:(nr - 1)]
  df$AR   <- (df$I + df$R) / N
  df$rate <- c(rate[1], rate)

  return(df)

}

# ---- Run simulation ----------------------------------------------------------
sir_simulation <- sim_sirmod(

                    N     = 50000,
                    I0    = 3,
                    beta  = 1,
                    alpha = 0.01,
                    t     = 300
                  )


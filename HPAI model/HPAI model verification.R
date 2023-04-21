##########
# Simulation code for HPAI Bangladesh Model for Verification
##########

Iterate <- function(status, DIST, Suscept, Transmiss, kern, I_to_R_time, pars, infections){
  
  # Function for evolving an infection process for an individual-based model 
  # over one time step.  
  #
  # SIRepC model: SUS(Susceptible) -> INF(Infected) -> REP(Reported) -> CUL(Culled)
  # 
  # INPUTS
  # 
  #   status : vector of infection times of each farm 
  #            (0 is susceptible, postive integers represent infected farms not yet reported (counts down to reporting day),
  #             1 represents reporting day, -1 is culled)
  
  #   DIST   : a matrix defining the distance between each farm
  # 
  #   Suscept: vector, farm-level susceptibility for each farm
  # 
  #   Transmiss: vector, farm-level transmissibility for each farm
  # 
  #   kern   : function defining the distance kernel (outputs the evaluated 
  #            kernel function for a given distance input)
  #
  #   I_to_R_time : time delay between infection and reporting
  #
  #   pars : parameter estimates from MCMC
  #
  #   infections: list of times between reporting and culling from outbreak data
  # 
  # OUTPUTS
  # 
  #   status : vector, the input status vector evolved one time step
  
  # Create an empty vector of events that occur this time step
  Event <- rep(0L, length(status)) 
  
  INF <- which(status > 0) # All holdings that are infectious (those infected and reported)
  SUS <- which(status == 0) # All holdings that are susceptible
  
  nINF <- length(INF)
  nSUS <- length(SUS)
  
  # Only calculate distance kernel when there are infected holdings
  if (nINF > 0) {
    
    # Evaluate the kernel function at the specified distances
    K <- matrix(kern(DIST[INF,SUS]),nINF)
    TRANS <- kronecker(matrix(1, 1, nSUS),Transmiss[INF]) 
    SUSCEPT <- kronecker(matrix(1, nINF, 1), t(Suscept[SUS]))
    
    # Find the probability of each susceptible holding becoming infected
    force <- as.matrix(TRANS * SUSCEPT * K)
    rate <- (colSums(force)) + pars$spark
    
  } else {
    
    # Find the probability of each susceptible holding becoming infected
    # by external infection - spark term
    rate <- matrix(pars$spark, 1, nSUS)
  }
  
  # Find which premises become infected by comparing if P (probability of infection) 
  # is >= a random number between 0 and 1
  P <- 1 - exp(-rate)
  INFECTION_EVENT <- P >= runif(nSUS)
  
  # Find the index of susceptible holdings which become infected
  INFECTION_ROWS <- SUS[(INFECTION_EVENT) > 0]
  
  # Calculate time to culling for each infection event - randomly sampled from array of actual times
  TIME_TO_CULL <- sapply(INFECTION_ROWS, function(x) 1 + I_to_R_time + sample(infections$time_to_cull, 1))
  
  # Update the event vector with culling times
  if (length(INFECTION_ROWS) > 0) {Event[INFECTION_ROWS] <- TIME_TO_CULL}
  
 # Reduce timer for existing IPs and add new IPs to status vector
  status[status > 0] <- status[status > 0] - 1
  status <- status + Event
  
  return(status)
}

##########
# Distance kernel function
##########

kernel <- function(d, alpha = pars$alpha){
  return((100 / d)^(alpha + 1))
}


##########
# Code for running HPAI model
##########

setwd("")

#####
# Choose settings
#####

num_outbreaks <- 1000 # Number of outbreaks to generate

#####
# Main code section
#####

for (wave in c(2,5)) { 

  set.seed(2019)
  
  # Load files
  holdings <- readRDS("R files/dhaka_div_data.rds") # Farm data
  distance_matrix <- readRDS("R files/dhaka_div_distance_matrix.rds") # Distance between farms
  
  if (wave == 2) {
    infections <- readRDS("R files/dhaka_div_w2_inf.rds") # Reporting to culling times
    max_time <- 176 # Outbreak duration
    initial_infected <- 10390 # Initial infected farm
    #which(holdings$Digital_North == 23.7891 & holdings$Digital_East == 90.6519)
    parameters <- readRDS("R files/w2_C_div_parameters.rds") # Thinned parameter estimates
  } else if (wave == 5) {
    infections <- readRDS("R files/dhaka_div_w5_inf.rds") # Reporting to culling times
    max_time <- 129 # Outbreak duration
    initial_infected <- 3107  # Initial infected farm
    #which(holdings$Digital_North == 23.5648 & holdings$Digital_East == 90.1679)
    parameters <- readRDS("R files/w5_C_div_parameters.rds") # Thinned parameter estimates
  }

  # Create parameters for model
  N <- nrow(holdings) # Number of premises
  Chickens <- holdings$Chickenno # Number of chickens
  I_to_R_time <- 7 # Time between infection and reporting
  
  # # Create empty list to store detailed results for each simulation
  individual_outbreak <- list()
  
  # Set up two empty vectors to store results
  outbreak_size <- outbreak_duration <- rep(NA, num_outbreaks)
  
  # Run several outbreaks
  for (run in 1:num_outbreaks){
    
    # Reset the starting infection conditions on each farm
    status <- rep(0, N) # Holding vector for farm status
    status[initial_infected] <- 1 + I_to_R_time + sample(infections$time_to_cull, 1) # Infect the index case
    inf_day <- rep(0,N) # Holding vector for infection day
    inf_day[initial_infected] <- 1 # Record day of index case
    cull_day <- rep(0,N) # Holding vector for culling day
    
    # Randomly sample a row of parameters
    pars <- parameters[sample(nrow(parameters), 1), ]
    
    # Set farm level susceptibility and transmissibility
    Suscept <- Chickens ^ pars$pc
    Transmiss <- pars$tc * (Chickens ^ pars$qc)
    
    # Reset the time counter
    current_t <- 2
    
    # Loop through each time step. Stop the loop if the time step goes past
    # the 'max_time' parameter or if there are no more susceptible premises.  
    while((current_t <= max_time) & length(which(status == 0)) > 0) {
      
      # Iterate the infection process each time step
      status <- Iterate(status, DIST = distance_matrix, Suscept, Transmiss, kern = kernel, I_to_R_time, pars, infections)
      
      # Identify new IPs and record the day
      new_inf <- which(status > 0 & inf_day == 0)
      inf_day[new_inf] <- current_t

       # Identify culled premises, change their status and record the day
      culled <- (status == 1)
      status[culled] <- -1
      cull_day[culled] <- current_t
      
      # Increment the time counter
      current_t <- current_t + 1
      
    }
    # Save the culling days and the chicken numbers to the list
    df <- data.frame(status, inf_day, cull_day, Chickens)
    individual_outbreak[[run]] <- df
    
    outbreak_size[run] <- sum(status != 0) # Any premises that are no longer susceptibles were infected
    outbreak_duration[run] <- current_t
  }
  
  results <- data.frame(outbreak_size, outbreak_duration)
  outfile1 <- paste("Results/Verification/outbreak_size_", wave, ".rds", sep = "")
  saveRDS(results, outfile1)
  
  outfile2 <- paste("Results/Verification/detailed_results_", wave, ".rds", sep = "")
  saveRDS(individual_outbreak, outfile2)
}

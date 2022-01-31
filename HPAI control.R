##########
# Bangladesh HPAI simulation code
# Single or multiphase control
##########

setwd("")
source("HPAI_functions.R")
library(gtools)

#####
# Choose settings 
#####

management_list <- c(1:87) # Subset of the management options list to use
#Low - 1,2,4,8,12,14,18,24
#High - 59,60,62,66,70,72,76,84
# All - 1:87

wave <- 2 # Options: 2 or 5
model <- "C" # Options: B or C
area <- "division" # Options: district or division

num_outbreaks <- 1000 # Number of outbreaks to generate
random_seeder <- c(2001:3000) # Allows for reproducibility of 'randomness'. Must have minimum length = num_outbreaks

switch_type <- 0 # Options: 0 - No switch point, 1 - Outbreak duration, 2 - Outbreak size, 3 - Time since last IP
switch_points <- 0 # Number of potential switch points
trigger <- NA

transmiss_change <- 1 # Change in transmissibility
flock_size <- 1 # Change in chicken flock sizes
spark <- "off" # Can be on or off

# Generally these do not need changing
I_to_R_time <- 7 # Time between infection and reporting
infection_free <- 28 # Days with no new reported infections before control stops
delay <- 7 # Time between vaccination and immunity
efficacy <- 0.7 # Proportion of birds that are successfully vaccinated
surveillance_time <- 4 # Days between infection and reporting for premises under active surveillance

#####
# Main code section
#####

# Load files
# Management options and appropriate holdings, distance matrix and parameter set files

management_options <- readRDS("R files/management_options.rds")

if (area == "district") {
  holdings <- readRDS("R files/dhaka_dist_data.rds") # Farm data
  distance_matrix <- readRDS("R files/dhaka_dist_distance_matrix.rds") # Distance between premises
  if (wave == 2) {
    if (model == "B") {
      parameters <- readRDS("R files/w2_B_dist_parameters.rds") # Thinned parameter estimates
    } else if (model == "C") {
      parameters <- readRDS("R files/w2_C_dist_parameters.rds") # Thinned parameter estimates
    }
  } else if (wave == 5) {
    if (model == "B") {
      parameters <- readRDS("R files/w5_B_dist_parameters.rds") # Thinned parameter estimates
    } else if (model == "C") {
      parameters <- readRDS("R files/w5_C_dist_parameters.rds") # Thinned parameter estimates
    }
  }
} else if (area == "division") {
  holdings <- readRDS("R files/dhaka_div_data.rds") # Farm data
  distance_matrix <- readRDS("R files/dhaka_div_distance_matrix.rds") # Distance between farms
  if (wave == 2) {
    if (model == "B") {
      parameters <- readRDS("R files/w2_B_div_parameters.rds") # Thinned parameter estimates
    } else if (model == "C") {
      parameters <- readRDS("R files/w2_C_div_parameters.rds") # Thinned parameter estimates
    }
  } else if (wave == 5) {
    if (model == "B") {
      parameters <- readRDS("R files/w5_B_div_parameters.rds") # Thinned parameter estimates
    } else if (model == "C") {
      parameters <- readRDS("R files/w5_C_div_parameters.rds") # Thinned parameter estimates
    }
  }
}

# Calculate density for proactive surveillance
density <- apply(distance_matrix, 1, function(x) sum(x < 500))

# Recalculate chicken populations
holdings$Chickenno <- holdings$Chickenno * flock_size

# Create empty list to store results
summary_results <- list()

# Create list of all management permutations for switching strategies
if (switch_type != 0) {
  management_permutations <- permutations(n = length(management_list), r = (switch_points + 1), v = management_list)
  saveRDS(management_permutations, "management_permutations.rds") # Save management permutations
  loop <- (1:nrow(management_permutations)) # Objects to loop over

} else {
  loop <- management_list # Objects to loop over
}

# Loop over each of the chosen management options
for (m in loop) {
  
  # Choose settings for single control scenarios
  if (switch_type == 0) {settings <- Settings(m, management_options)}
  
  # Set up empty vectors to store summary results
  outbreak_size <- outbreak_duration <- culled_premises <- culled_chickens <- vaccinated_premises <-
    vaccinated_chickens <- unreported_infections <- switch_day <- rep(NA, num_outbreaks)
  
  # Create empty list to store detailed results for each simulation
  individual_outbreak <- list()
  
  # Loop over each individual outbreak
  for (run in 1:num_outbreaks){
    
    set.seed(random_seeder[run])
    
    # Reset the starting infection conditions on each farm
    status <- IP_day <- rep_day <- control_day <- cull_day <- vax_day <- t_to_rep <- 
      t_to_immune <- rep(0, nrow(holdings))
    proportion <- rep(1, nrow(holdings)) 
    # status: 0 = susceptible, 1 = infected, -1 = culled
    # Records day of respective event:
    # IP_day (infected), rep_day (reported), control_day (listed for ring culling/vaccination/active surveillance),
    # cull_day (IP cull or ring cull day), vax_day (day vaccinated) 
    # t_to_rep and t_to_immune counters: 0 inactive, count down daily and change triggered when = 1
    # Proportion of total transmissibility/susceptibility retained by vaccinated premises - used for iterate function
    
    # Reset counters
    infection_free_counter <- 0 # Infection free time
    current_t <- 1 # Outbreak duration
    switch <- 0 # Number of times strategy has been switched
    
    # Set/reset starting management options for switching simulations
    if (switch_type != 0) {
      selector <- management_permutations[m,1]
      settings <- Settings(selector, management_options)
    }
  
    # Randomly infect the index case
    initial_infected <- sample(nrow(holdings), 1)
    
    status[initial_infected] <- 1 
    t_to_rep[initial_infected] <- 1 + I_to_R_time
    IP_day[initial_infected] <- 1
    
    # Randomly sample a row of parameters
    pars <- parameters[sample(nrow(parameters), 1), ]
    
    if (spark == "off") {parameters$spark <- 0}

    # Create empty lists to store premises needing culling/vaccination/surveillance
    IP_cull_list <- ring_cull_list <- vax_list <- surveillance_list <- integer()

    # Set farm level susceptibility and transmissibility
    if (model == "B") {
      Suscept <- holdings$Chickenno 
      Transmiss <- pars$tc * holdings$Chickenno * transmiss_change
    } else if (model == "C") {
      Suscept <- (holdings$Chickenno ^ pars$pc)
      Transmiss <- pars$tc * (holdings$Chickenno ^ pars$qc) * transmiss_change
    }
    
    # Create new data frame
    results_df <- data.frame(holdings, Suscept, Transmiss, status, IP_day, rep_day, control_day, cull_day, vax_day, t_to_rep, t_to_immune, proportion)
    rm(Suscept, Transmiss, status, IP_day, rep_day, control_day, cull_day, vax_day, t_to_rep, t_to_immune, proportion)
    
    # Identify farms for proactive surveillance
    if (settings$control == 4) {
      if (settings$type == "proactive" & settings$priority == "population") {
        surveillance_list <- order(results_df$Chickenno, decreasing = TRUE)[1:round(nrow(results_df) * settings$coverage * 0.01)]
      } else if (settings$type == "proactive" & settings$priority == "density") {
        surveillance_list <- order(density, decreasing = TRUE)[1:round(nrow(results_df) * settings$coverage * 0.01)]
      }
      results_df$control_day[surveillance_list] <- current_t # Record surveillance day
    }
    
    # Loop over each time step. 
    # Stop the loop if the counter goes past the 'infection free' parameter.  
    while(infection_free_counter < infection_free) {
    
      # Iterate the infection process each time step
      new_infections <- Iterate(results_df, DIST = distance_matrix, kern = kernel, pars, settings)
      
      # Only run this block if there are new infections
      if (length(new_infections) > 0) {
        
        # Change new infections status 
        results_df$status[new_infections] <- 1 
        
        if(settings$control == 4) {
          for (j in new_infections) {
            if(results_df$control_day[j] > 0) {
              results_df$t_to_rep[j] <- 1 + surveillance_time
            }
            else {
              results_df$t_to_rep[j] <- 1 + I_to_R_time
            }
          }
        } else {
          results_df$t_to_rep[new_infections] <- 1 + I_to_R_time
        }
        
        results_df$IP_day[new_infections] <- current_t
      }
      
      # Identify any new reported premises 
      new_reported <- which(results_df$t_to_rep == 1)
      
      if (length(new_reported) > 1) {new_reported <- sample(new_reported)}  # Randomly sort vector
      if (length(new_reported) > 0) {
        IP_cull_list <- c(IP_cull_list, new_reported) # Add new reported infections to cull list
        results_df$rep_day[new_reported] <- current_t # Change reported status
      } 
      
      # After time delay, reduce any vaccinated premises susceptibility and transmissibility
      # due to immunity conferred by vaccination
      if (settings$control == 3) {
        results_df$Suscept[results_df$t_to_immune == 1] <- results_df$Suscept[results_df$t_to_immune == 1] * (1 - efficacy)
        results_df$Transmiss[results_df$t_to_immune == 1] <- results_df$Transmiss[results_df$t_to_immune == 1] * (1 - efficacy)
        results_df$proportion[results_df$t_to_immune == 1] <- results_df$proportion[results_df$t_to_immune == 1] * (1 - efficacy)
      }
      
      # Reduce any positive time to reporting or immunity timers by 1
      results_df$t_to_rep[results_df$t_to_rep > 0] <- results_df$t_to_rep[results_df$t_to_rep > 0] - 1
      results_df$t_to_immune[results_df$t_to_immune > 0] <- results_df$t_to_immune[results_df$t_to_immune > 0] - 1
      
      # Update infection free counter
      # Infection free if no new reported IPs and no premises in IP cull list
      if ((length(IP_cull_list) == 0) & (length(new_reported) == 0)) {
        infection_free_counter <- infection_free_counter + 1
      } else {
        infection_free_counter <- 0
      }
      
      # Do control
      if (settings$control == 1) {
        control_results <- IP_culling(results_df, IP_cull_list, settings$limit, current_t)
      } else if (settings$control == 2) {
        control_results <- Ring_culling(results_df, new_reported, IP_cull_list, ring_cull_list, settings$radius, settings$limit, distance_matrix, current_t)
        ring_cull_list <- control_results$ring_cull_list
      } else if (settings$control == 3) {
        control_results <- Ring_vaccination(results_df, new_reported, IP_cull_list, vax_list, settings$radius, settings$limit, distance_matrix, current_t)
        vax_list <- control_results$vax_list
      } else if (settings$control == 4) {
        if (settings$type == "proactive") {
          control_results <- IP_culling(results_df, IP_cull_list, settings$limit, current_t)
        } else if (settings$type == "reactive") {
          control_results <- Reactive_surveillance(results_df, new_reported, IP_cull_list, surveillance_list, settings$limit, settings$priority, distance_matrix, current_t)
          surveillance_list <- control_results$surveillance_list
        }
      }
      
      results_df <- control_results$df
      IP_cull_list <- control_results$IP_cull_list
      
      # Increment the time counter
      current_t <- current_t + 1
      
      # If maximum number of strategy switch points has not yet occurred
      if (switch < switch_points) {
        
        # Trigger check - has outbreak reached criteria to switch control strategy
        if (switch_type == 1) {
          trigger_check <- current_t # Outbreak duration
        } else if (switch_type == 2) {
          trigger_check <- sum(results_df$rep_day > 0) # Number of reported IPs
        } else if (switch_type == 3) {
          trigger_check <- current_t - max(results_df$rep_day) # Day since last IP
        } else {
          trigger_check <- NA # No switching
        }
        
        # If it is a switch simulation and the switch trigger criteria has been
        if ((!is.na(trigger)) & (trigger_check >= trigger))  {
          
          switch <- switch + 1 # Increase switch counter
          switch_day[run] <- current_t # Record switch day
          
          # Change to switched management options
          selector <- management_permutations[m,(switch + 1)]
          settings <- Settings(selector, management_options)
          
          # Clear appropriate lists
          ring_cull_list <- vax_list <- surveillance_list <- integer()
          
          # Identify farms for proactive surveillance
          if (settings$control == 4) {
            if (settings$type == "proactive" & settings$priority == "population") {
              surveillance_list <- order(results_df$Chickenno, decreasing = TRUE)[1:round(nrow(results_df) * settings$coverage * 0.01)]
            } else if (settings$type == "proactive" & settings$priority == "density") {
              surveillance_list <- order(density, decreasing = TRUE)[1:round(nrow(results_df) * settings$coverage * 0.01)]
            }
            
            # Record surveillance day
            results_df$control_day[surveillance_list] <- current_t
          }
        }
      }
    }
    
    # Save the summary outbreak data
    outbreak_size[run] <- sum(results_df$rep_day > 0) # Records only premises that have been reported as IPs
    outbreak_duration[run] <- current_t - 1
    culled_premises[run] <- sum(results_df$status == -1) # Includes IP and ring culled premises
    culled_chickens[run] <- sum(results_df$Chickenno[which(results_df$status == -1)])
    vaccinated_premises[run] <- sum(results_df$vax_day > 0)
    vaccinated_chickens[run] <- sum(results_df$Chickenno[which(results_df$vax_day > 0)])
    unreported_infections[run] <- sum(results_df$status == 1)
    
    # Save the individual outbreak data
    # Only keep rows of data that have been subject to the outbreak in some way
    results_df <- results_df[which(results_df$status != 0 | results_df$IP_day != 0 | results_df$rep_day != 0 | results_df$control_day != 0 | results_df$cull_day != 0 | results_df$vax_day != 0),]
    individual_outbreak[[run]] <- results_df
  }
  
  # Create data frame of summary results and store in list
  df <- data.frame(outbreak_size, outbreak_duration, culled_premises, culled_chickens, vaccinated_premises, vaccinated_chickens, unreported_infections, switch_day)
  summary_results[[m]] <- df
  
  # Save detailed results
  outfile <- paste("detailed_", m, ".rds", sep = "")
  saveRDS(individual_outbreak, outfile)
}

# Save summary results
saveRDS(summary_results, "summary_results.rds")






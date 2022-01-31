####################################
# Functions used by the HPAI model #
####################################

# IP_culling: IP culling function
# Iterate: Simulation function for HPAI Bangladesh Model
# Kernel: Distance kernel function
# Reactive_surveillance: Reactive surveillance function
# Ring_culling: Ring culling function
# Ring_vaccination: Ring vaccination function
# Settings: Selects the correct management settings

##########
# IP_culling
# IP culling function
##########

# Function to implement IP culling
#
# INPUTS
#
#   results_df : data frame containing key information about all premises including
#     Chickenno: number of chickens present
#     status: current infection status (0 is susceptible, 1 is infected, - 1 is culled)
#     cull_day: day culling took place (0 is unculled)
#
#   IP_cull_list: vector, list of premises waiting to be culled. In order of priority.
#
#   limit: integer, culling daily resource limit
#
#   current_t: integer, current outbreak day
#
# OUTPUTS
#
# IP_cull_results: list containing updated results_df and IP_cull_list

IP_culling <- function(results_df, IP_cull_list, limit, current_t) { 
  
  # Only run this block if there are premises in the IP cull list
  if (length(IP_cull_list) > 0) {
    
    # Counters to ensure limits are not exceeded
    premise_limit <- limit
    chicken_limit <- limit * 1000
    
    # If there is enough capacity to cull all IPs
    if (length(IP_cull_list) <= premise_limit & sum(results_df$Chickenno[IP_cull_list]) <= chicken_limit) {
      
      results_df$status[IP_cull_list] <- -1 # Update status to culled
      results_df$cull_day[IP_cull_list] <- current_t # Record cull day
      IP_cull_list <- numeric() # Clear IP cull list
      
      # If there is not enough capacity to cull all IPs
    } else {
      
      # Find which IPs are to be culled
      cumulative_total <- cumsum(results_df$Chickenno[IP_cull_list])
      to_cull <- IP_cull_list[1:min(sum(cumulative_total <= chicken_limit) + 1, premise_limit)]
      
      results_df$status[to_cull] <- -1 # Update status to culled
      results_df$cull_day[to_cull] <- current_t # Record cull day
      IP_cull_list <- IP_cull_list[!IP_cull_list %in% to_cull] # Update IP cull list
    }
  }
  
  IP_cull_results <- list("df" = results_df, "IP_cull_list" = IP_cull_list)
  return(IP_cull_results)
}

##########
# Iterate
# Simulation function for HPAI Bangladesh Model
##########

# Function for evolving an infection process for an individual-based model 
# over one time step.  
#
# SIRepC model: SUS(Susceptible) -> INF(Infected) -> REP(Reported) -> CUL(Culled)
# 
# INPUTS
# 
#   results_df: data frame containing key information about all premises including
#     Suscept: premises-level susceptibility 
#     Transmiss: premises-level transmissibility 
#     status: current infection status (0 is susceptible, 1 is infected, - 1 is culled)
#
#   DIST: matrix defining the distance between each premises
# 
#   kern: function defining the distance kernel (outputs the evaluated 
#            kernel function for a given distance input)
#
#   pars: list containing parameter estimates from MCMC
#           pars$spark: the spark term (external force of infection)
# 
# OUTPUTS
# 
#   INFECTION_INDEX : vector, the index of susceptible premises that have become infected in this
#                      execution of the function

Iterate <- function(results_df, DIST, kern, pars, settings){
  
  INF <- which(results_df$status == 1) # All premises that are infectious
  SUS <- which(results_df$status == 0) # All premises that are susceptible
  
  nINF <- length(INF)
  nSUS <- length(SUS)
  
  # Only calculate distance kernel when there are infected premises
  if (nINF > 0) {
    
    # Evaluate the kernel function at the specified distances
    K <- matrix(kern(DIST[INF,SUS]),nINF)
    TRANS <- kronecker(matrix(1, 1, nSUS),results_df$Transmiss[INF])
    SUSCEPT <- kronecker(matrix(1, nINF, 1), t(results_df$Suscept[SUS]))
    
    # Find the probability of each susceptible premises becoming infected
    force <- as.matrix(TRANS * SUSCEPT * K)
    rate <- (colSums(force)) + pars$spark
  }
  
  else {

    # Find the probability of each susceptible premises becoming infected 
    # by external infection (spark term)
    if (settings$control == 3) {
      # Reduce susceptibility for vaccinated premises
      rate <- matrix(results_df$proportion[SUS] * pars$spark, 1, nSUS)
    }
    
    else {rate <- matrix(pars$spark, 1, nSUS)}
  }
  
  # Find which premises become infected by comparing if P (probability of infection) 
  # is >= a random number between 0 and 1
  P <- 1 - exp(-rate)
  INFECTION_EVENT <- P >= runif(nSUS)
  
  # Find the index of susceptible holdings which become infected
  INFECTION_INDEX <- SUS[INFECTION_EVENT > 0]
  
  return(INFECTION_INDEX)
}

##########
# Kernel
# Distance kernel function
##########

kernel <- function(d, alpha = pars$alpha){
  return((100 / d)^(alpha + 1))
}

###########
# Reactive_surveillance
# Reactive surveillance function
##########

# Function to implement reactive surveillance
#
# INPUTS
#   results_df : data frame containing key information on all premises including
#     Chickenno: number of chickens present
#     status: current infection status (0 is susceptible, 1 is infected, - 1 is culled)
#     control_day: day listed for control (in this case ring culling)
#
#   new_reported: vector, list of premises that are newly reported IPs
#
#   IP_cull_list: vector, list of premises waiting to be culled. In order of priority
#
#   surveillance_list: vector, list of premises currently on the active surveillance list
#
#   limit: integer, culling daily resource limit
#
#   distance_matrix: matrix, distance in metres between each premises
#
#   current_t: integer, current outbreak day 
#
# OUTPUTS
# surveillance_results: list containing updated results_df, IP_cull_list and surveillance_list

Reactive_surveillance <- function(results_df, new_reported, IP_cull_list, surveillance_list, limit, priority, distance_matrix, current_t) {
  
  # Enact IP culling
  control_results <- IP_culling(results_df, IP_cull_list, limit, current_t)
  results_df <- control_results$df
  IP_cull_list <- control_results$IP_cull_list
  
  # Identify new premises to add to surveillance list
  # Only run this block if there are new reported premises and the surveillance list is not full
  if (length(new_reported) > 0 & length(surveillance_list) < limit) {
    
    new_surveillance <- numeric() # Create empty new surveillance list
    distance_to_IP <- numeric() # Create empty distance list
    
    # For each newly reported premises
    for (i in 1:(length(new_reported))) {
      
      # Find premises within surveillance radius (500m) of new IP
      premises <- which(distance_matrix[new_reported[i],] <= 500 & distance_matrix[new_reported[i],] > 0)
      
      # Remove any that have already been culled or are already in surveillance list or new surveillance list
      premises <- premises[! (premises %in% which(results_df$status == -1) | premises %in% surveillance_list | premises %in% new_surveillance)]
      
      # Record distance from IP
      distance <- distance_matrix[new_reported[i],premises]
      
      # Add to new surveillance and distance list
      new_surveillance <- c(new_surveillance, premises)
      distance_to_IP <- c(distance_to_IP, distance)
    }
    
    # Only do this block if there are new surveillance premises identified
    if (length(new_surveillance) > 0) {
      
      # Add all identified premises to the surveillance list if there is capacity
      if ((length(new_surveillance) + length(surveillance_list)) <= limit) {
        
        surveillance_list <- c(surveillance_list, new_surveillance)
        results_df$control_day[new_surveillance] <- current_t # Record surveillance day
      
      # Decide which premises to add based on their priority  
      } else {
        
        # Find premises nearest to an IP
        if (priority == "distance") {
          new_premises <- new_surveillance[order(distance_to_IP)[1:(limit - length(surveillance_list))]]
          
        # Find highest chicken populations within space limit
        } else if (priority == "population") {
          new_premises <- new_surveillance[order(results_df$Chickenno[new_surveillance], decreasing = TRUE)[1:(limit - length(surveillance_list))]]
        }
        
        surveillance_list <- c(surveillance_list, new_premises) # Add to surveillance list
        results_df$control_day[new_premises] <- current_t # Record surveillance day
      }
    }
  }
  
  surveillance_results <- list("df" = results_df, "IP_cull_list" = IP_cull_list, "surveillance_list" = surveillance_list)
  return(surveillance_results)
}

##########
# Ring_culling
# Ring culling function
##########

# Function to implement ring culling
#
# INPUTS
#   results_df : data frame containing key information on all premises including
#     Chickenno: number of chickens present
#     status: current infection status (0 is susceptible, 1 is infected, - 1 is culled)
#     control_day: day listed for control (in this case ring culling)
#     cull_day: day culling took place (0 is unculled)
#
#   new_reported: vector, list of premises that are newly reported IPs
#
#   IP_cull_list: vector, list of premises waiting to be culled. In order of priority
#
#   ring_cull_list: vector, list of premises waiting to be ring culled. In order of priority
#
#   radius: integer, ring cull radius
#
#   limit: integer, culling daily resource limit
#
#   distance_matrix: matrix, distance in metres between each premises
#
#   current_t: integer, current outbreak day 
#
# OUTPUTS
# ring_cull_results: list containing updated results_df, IP_cull_list and ring_cull_list

Ring_culling <- function(results_df, new_reported, IP_cull_list, ring_cull_list, radius, limit, distance_matrix, current_t) {
  
  # Only run this block if there are new reported premises
  if (length(new_reported > 0)) {
    
    # Remove any reported IPs from ring cull list
    ring_cull_list <- ring_cull_list[!ring_cull_list %in% IP_cull_list]
    
    # Identify new premises to add to ring cull list for each newly reported IP
    for (i in 1:(length(new_reported))) {
      
      # Find premises within ring cull radius of new IP
      premises <- which(distance_matrix[new_reported[i],] <= radius*1000 & distance_matrix[new_reported[i],] > 0)
      
      # Remove any that have already been culled or are already in IP or cull list
      premises <- premises[! (premises %in% which(results_df$status == -1) | premises %in% ring_cull_list | premises %in% IP_cull_list)]
      
      # Sort from largest to smallest distance to IP
      premises <- premises[order(distance_matrix[new_reported[i],premises], decreasing = TRUE)]
      
      # Add to ring cull list
      ring_cull_list <- c(ring_cull_list, premises)
      
      # Record day added to ring cull list
      results_df$control_day[premises] <- current_t
    }
  }
  
  # Only run this block if there are premises in the IP or ring cull list
  if (length(IP_cull_list > 0) | length(ring_cull_list > 0)) {
    
    # Counters to ensure limits are not exceeded
    premise_limit <- limit
    chicken_limit <- limit * 1000
    
    # If there is enough capacity to cull all IPs
    if (length(IP_cull_list) <= premise_limit & sum(results_df$Chickenno[IP_cull_list]) <= chicken_limit) {
      
      # Reduce counters
      premise_limit <- premise_limit - length(IP_cull_list) 
      chicken_limit <- chicken_limit - sum(results_df$Chickenno[IP_cull_list])
      
      results_df$status[IP_cull_list] <- -1 # Update status to culled
      results_df$cull_day[IP_cull_list] <- current_t # Record cull day
      IP_cull_list <- numeric() # Clear IP cull list
      
      # If there is enough capacity to cull all in ring cull zone
      if (length(ring_cull_list) <= premise_limit & sum(results_df$Chickenno[ring_cull_list]) <= chicken_limit) {
        
        results_df$status[ring_cull_list] <- -1 # Update status to culled
        results_df$cull_day[ring_cull_list] <- current_t # Record cull day
        ring_cull_list <- numeric() # Clear ring cull list
      }
      
      # If there is not enough capacity to cull all in ring cull zone
      else {
        
        # Identify the premises are to be ring culled
        cumulative_total <- cumsum(results_df$Chickenno[ring_cull_list])
        to_cull <- ring_cull_list[1:min(sum(cumulative_total <= chicken_limit) + 1, premise_limit)]
        
        results_df$status[to_cull] <- -1 # Update status to culled
        results_df$cull_day[to_cull] <- current_t # Record cull day
        ring_cull_list <- ring_cull_list[!ring_cull_list %in% to_cull] # Update ring cull list
      }
    }
    
    # If there is not enough capacity to cull all IPs
    else {
      
      # Identify the premises to be IP culled
      cumulative_total <- cumsum(results_df$Chickenno[IP_cull_list])
      to_cull <- IP_cull_list[1:min(sum(cumulative_total <= chicken_limit) + 1, premise_limit)]
      
      results_df$status[to_cull] <- -1 # Update status to culled
      results_df$cull_day[to_cull] <- current_t # Record cull day
      IP_cull_list <- IP_cull_list[!IP_cull_list %in% to_cull]  # Update IP cull list
    }
  }
  
  ring_cull_results <- list("df" = results_df, "IP_cull_list" = IP_cull_list, "ring_cull_list" = ring_cull_list)
  return(ring_cull_results)
}

##########
# Ring_vaccination
# Ring vaccination function
##########

# Function to implement ring vaccination
#
# INPUTS
#   results_df : data frame containing key information on all premises including
#     Chickenno: number of chickens present
#     status: current infection status (0 is susceptible, 1 is infected, - 1 is culled)
#     control_day: day listed for control (in this case ring vaccination)
#     vax_day: day vaccination took place (0 is unvaccinated)
#     t_to_immune: days until vaccinated premises gain immunity conferred by the vaccine
#
#   new_reported: vector, list of premises that are newly reported IPs
#
#   IP_cull_list: vector, list of premises waiting to be culled. In order of priority
#
#   vax_list: vector, list of premises waiting to be vaccinated. In order of priority
#
#   radius: integer, ring cull radius
#
#   limit: integer, culling daily resource limit
#
#   distance_matrix: matrix, distance in metres between each premises
#
#   current_t: integer, current outbreak day
#
# OUTPUTS
# vaccination_results: list containing updated results_df, IP_cull_list and vax_list

Ring_vaccination <- function(results_df, new_reported, IP_cull_list, vax_list, radius, limit, distance_matrix, current_t) {

  # Enact IP culling
  control_results <- IP_culling(results_df, IP_cull_list, limit, current_t)
  results_df <- control_results$df
  IP_cull_list <- control_results$IP_cull_list

  # Only run this block if there are new reported premises
  if (length(new_reported) > 0) {

    # Identify new premises to add to vaccinate list
    for (i in 1:(length(new_reported))) {

      # Find premises within ring cull radius of new IP
      premises <- which(distance_matrix[new_reported[i],] <= radius*1000 & distance_matrix[new_reported[i],] > 0)

      # Remove any that have already been culled or vaccinated or are already in vaccinate list
      premises <- premises[! (premises %in% which(results_df$status == -1) | premises %in% which(results_df$vax_day > 0) | premises %in% vax_list)]

      # Sort from largest to smallest distance to IP
      premises <- premises[order(distance_matrix[new_reported[i],premises], decreasing = TRUE)]

      # Add to vax list
      vax_list <- c(vax_list, premises)

      # Record day added to ring vaccination list
      results_df$control_day[premises] <- current_t
    }
  }

  # Only run this block if there are premises in the vaccinate list

  if (length(vax_list) > 0) {

    # Counters to ensure limits are not exceeded
    premise_limit <- limit
    chicken_limit <- limit * 1000
    
    # Remove any premises that have already been reported or culled
    vax_list <- vax_list[! (vax_list %in% which(results_df$status == -1) | vax_list %in% which (results_df$rep_day > 0))]

    # If there is enough capacity to vaccinate all on vaccinate list
    if (length(vax_list) <= premise_limit & sum(results_df$Chickenno[vax_list]) <= chicken_limit) {

      results_df$vax_day[vax_list] <- current_t # Record vaccination day
      results_df$t_to_immune[vax_list] <- delay # Counter until immune
      vax_list <- numeric() # Clear vaccination list
    }

    # If there is not enough capacity to vaccinate all on list
    else {
      # Find which can be vaccinated
      cumulative_total <- cumsum(results_df$Chickenno[vax_list])
      to_vax <- vax_list[1:min(sum(cumulative_total <= chicken_limit) + 1, premise_limit)]

      results_df$vax_day[to_vax] <- current_t # Record vaccination day
      results_df$t_to_immune[to_vax] <- 1 + delay # Counter until immune
      vax_list <- vax_list[!vax_list %in% to_vax] # Update vaccination list
    }
  }
  vaccination_results <- list("df" = results_df, "IP_cull_list" = IP_cull_list, "vax_list" = vax_list)
  return(vaccination_results)
}

##########
# Settings
# Selects the correct management settings
##########

# Function for selecting the correct management settings to be used as the control measure
#
# INPUTS
#
#   selector: variable defining which of the management options is to be selected
#   management_options: list containing all possible management options
#   
# OUTPUTS
# 
#   settings: list containing the current limit, control, radius, type, priority and coverage settings

Settings <- function(selector, management_options){
  
  limit <- management_options$limit[selector] # Daily premises limit
  control <- management_options$control[selector] # 1 = IP culling, 2 = ring culling, 3 = ring vaccination, 4 = active surveillance
  radius <- management_options$radius[selector] # Culling/vaccination radius
  type <- management_options$type[selector] # Active surveillance only - reactive or proactive
  priority <- management_options$priority[selector] # Active surveillance only - distance, population or density
  coverage <- management_options$coverage[selector] # Proactive surveillance only - 5/10/25
  
  settings <- list("limit" = limit, "control" = control, "radius" = radius, 
                   "type" = type, "priority" = priority, "coverage" = coverage)
  return(settings)
}

###################
# Plots chapter 4 #
###################

setwd("")

library(dplyr)
library(ggplot2)
library(plyr)
library(xlsx) 

##########
# Figure 4.2 
# Box plots of chickens culls for ring culling simulations
##########

results <- readRDS("Apps/HPAI App/fixed_results.rds")
results <- filter(results, control %in% c(1:11,30:40,59:69)) # Keep IP and ring culling

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = culled_chickens/1000000), width = 0.5, fill = "#e28743") +
  facet_grid(wave ~ capacity) +
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  xlab("Ring culling radius (km)") +
  ylab("Chickens culled (millions)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45))

##########
# Figure 4.3
# Log transformed box plots of number of premises culled for ring culling simulations 
##########

results <- readRDS("Apps/HPAI App/fixed_results.rds")
results <- filter(results, control %in% c(1:11,30:40,59:69)) # Keep IP and ring culling

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = culled_premises), width = 0.5, fill = "#e28743") +
  facet_grid(wave ~ capacity) +
  scale_y_log10() +
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  xlab("Ring culling radius (km)") +
  ylab("Premises culled") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45))

##########
# Figure 4.4
# Box plots of chickens vaccinated for ring vaccination simulations
##########

results <- readRDS("Apps/HPAI App/fixed_results.rds")
results <- filter(results, control %in% c(1,12:21,30,41:50,59,70:79)) # Keep IP and ring vaccination

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = vaccinated_chickens/1000000), width = 0.5, fill = "#e28743") +
  facet_grid(wave ~ capacity) +
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  xlab("Ring vaccination radius (km)") +
  ylab("Chickens vaccinated (millions)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45))

##########
# Figure 4.5
# Box plots of premises vaccinated for ring vaccination simulations 
##########

results <- readRDS("Apps/HPAI App/fixed_results.rds")
results <- filter(results, control %in% c(1,12:21,30,41:50,59,70:79)) # Keep IP and ring vaccination

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = vaccinated_premises), width = 0.5, fill = "#e28743") +
  facet_grid(wave ~ capacity) +
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  xlab("Ring vaccination radius (km)") +
  ylab("Premises vaccinated") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45))

##########
# Figure 4.6
# Log transformed box plots of outbreak durations for active surveillance strategies 
##########

results <- readRDS("Apps/HPAI App/fixed_results.rds")
results <- filter(results, control %in% c(1,22:24,27,30,51,52,54,57,59,80,81,84,87)) # Keep IP and active surveillance

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = outbreak_duration), width = 0.5, fill = "#e28743") +
  facet_grid(wave ~ capacity) +
  scale_y_log10() +
  scale_x_discrete(labels=c("IP culling", "Reactive-distance", "Reactive-popn", "Proactive-popn", "Proactive-density")) +
  xlab("Active surveillance control strategy") +
  ylab("Outbreak duration (days)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

####################################################################################################

#####
# Calculate premises under reactive surveillance for single phase incremental analysis
#####

# Load files
management_options <- readRDS("R files/management_options.rds") # Management options file
w2 <- readRDS("Results/Fixed single control/w2_div_fixed/summary_results.rds") # Wave 2
w5 <- readRDS("Results/Fixed single control/w5_div_fixed/summary_results.rds") # Wave 5

for (i in c(22,23,51,52,80,81)) {
  # Wave 2
  current <- paste("Results/Fixed single control/w2_div_fixed/detailed_", i, ".rds", sep = "")
  detailed <- readRDS(current)
  
  # Create new vector
  surveillance <- rep(NA, length(detailed))
  
  # Calculated premises under surveillance for each simulation
  for (j in 1:length(detailed)) {surveillance[j] <- sum(detailed[[j]]$control_day > 0)}
  
  # Append to dataframe
  w2[[i]] <- cbind(w2[[i]], surveillance)
  
  # Wave 5
  current <- paste("Results/Fixed single control/w5_div_fixed/detailed_", i, ".rds", sep = "")
  detailed <- readRDS(current)
  
  # Create new vector
  surveillance <- rep(NA, length(detailed))
  
  # Calculated premises under surveillance for each simulation
  for (j in 1:length(detailed)) {surveillance[j] <- sum(detailed[[j]]$control_day > 0)}
  
  # Append to dataframe
  w5[[i]] <- cbind(w5[[i]], surveillance)
}

# Save appended results
saveRDS(w2, "Results/Fixed single control/w2_div_fixed/summary_results.rds") 
saveRDS(w5, "Results/Fixed single control/w5_div_fixed/summary_results.rds")

####################################################################################################

##########
# Data management for Fig 4.7 and 4.8
##########

# Load files
management_options <- readRDS("R files/management_options.rds") # Management options file
w2 <- readRDS("Results/Fixed single control/w2_div_fixed/summary_results.rds") # Wave 2
w5 <- readRDS("Results/Fixed single control/w5_div_fixed/summary_results.rds") # Wave 5

# Change limit and control to factors
management_options$limit = factor(management_options$limit, levels = c("100", "50", "20"))
management_options$control = factor(management_options$control, levels = c("2", "3", "4"))

# Create results analysis data frame
w2_culled_ch_median <- w2_culled_ch_low <- w2_culled_ch_high <- 
  w5_culled_ch_median <- w5_culled_ch_low <- w5_culled_ch_high <-
  rep(NA, nrow(management_options))

# To calculate 95% range
low <- 0.025 
high <- 0.975 

for (i in 1:nrow(management_options)) {
  
  # Wave 2
  current <- w2[[i]] # Select results for this management option
  
  # Select comparator results
  if (management_options$limit[i] == 20) {comparator <- w2[[1]] # Low capacity
  } else if (management_options$limit[i] == 50) {comparator <- w2[[30]] # Medium capacity
  } else if (management_options$limit[i] == 100) {comparator <- w2[[59]]} # High capacity
  
  # Calculate averted chickens culled for each simulation run
  difference <- comparator$culled_chickens - current$culled_chickens
  
  w2_culled_ch_median[i] <- median(difference) # Median chickens culled
  w2_culled_ch_low[i] <- quantile(difference, low) # Low % chickens culled
  w2_culled_ch_high[i] <- quantile(difference, high)  # High % chickens culled
  
  # Wave 5
  current <- w5[[i]] # Select results for this management option
  
  # Select comparator results
  if (management_options$limit[i] == 20) {comparator <- w5[[1]] # Low capacity
  } else if (management_options$limit[i] == 50) {comparator <- w5[[30]] # Medium capacity
  } else if (management_options$limit[i] == 100) {comparator <- w5[[59]]} # High capacity
  
  # Calculate averted chickens culled for each simulation run
  difference <- comparator$culled_chickens - current$culled_chickens
  
  w5_culled_ch_median[i] <- median(difference)  # Median chickens culled
  w5_culled_ch_low[i] <- quantile(difference, low) # Low % chickens culled
  w5_culled_ch_high[i] <- quantile(difference, high) # High % chickens culled
}

averted <- data.frame(management_options, w2_culled_ch_median, w2_culled_ch_low, w2_culled_ch_high,
                      w5_culled_ch_median, w5_culled_ch_low, w5_culled_ch_high)

# Remove unused rows including comparator
averted <- averted[c(2:24,27,31:52,54,57,60:81,84,87),]

# Add management option label to data frame
averted['management_ID'] <- rep(c("1km cull","2km cull","3km cull","4km cull","5km cull","6km cull","7km cull","8km cull","9km cull","10km cull",
                                  "1km vacc","2km vacc","3km vacc","4km vacc","5km vacc","6km vacc","7km vacc","8km vacc","9km vacc","10km vacc",
                                  "Reactive-distance", "Reactive-popn", "Proactive-popn", "Proactive-density"), 3)


##########
# Figure 4.7
# Dot and whisker plot
# Wave 2 Averted Chickens Culled
##########

# Facet label names 
facet.labs <- c("Low capacity", "Medium capacity", "High capacity")
names(facet.labs) <- c("20", "50", "100")

# Plot
ggplot(averted) +
  geom_point(aes(x = management_ID, y = w2_culled_ch_median/1000000, colour = control)) +
  geom_errorbar(aes(x = management_ID, ymin = w2_culled_ch_low/1000000, ymax = w2_culled_ch_high/1000000, colour = control)) +
  facet_grid(limit ~ ., labeller = labeller(limit = facet.labs)) +
  scale_x_discrete(limits = c("1km cull","2km cull","3km cull","4km cull","5km cull","6km cull","7km cull","8km cull","9km cull","10km cull",
                              "1km vacc","2km vacc","3km vacc","4km vacc","5km vacc","6km vacc","7km vacc","8km vacc","9km vacc","10km vacc",
                              "Reactive-distance", "Reactive-popn", "Proactive-popn", "Proactive-density")) +
  scale_color_manual(labels = c("Ring culling","Ring vaccination","Active surveillance"), values = c("#1b9e77","#d95f02","#7570b3")) +
  xlab("Management objective") +
  ylab("Averted chickens culled (millions)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "bottom", legend.title=element_blank())

##########
# Figure 4.8
# Dot and whisker plot
# Wave 2 Averted Chickens Culled
##########

# Facet label names 
facet.labs <- c("Low capacity", "Medium capacity", "High capacity")
names(facet.labs) <- c("20", "50", "100")

# Plot
ggplot(averted) +
  geom_point(aes(x = management_ID, y = w5_culled_ch_median/1000000, colour = control)) +
  geom_errorbar(aes(x = management_ID, ymin = w5_culled_ch_low/1000000, ymax = w5_culled_ch_high/1000000, colour = control)) +
  facet_grid(limit ~ ., labeller = labeller(limit = facet.labs)) +
  scale_x_discrete(limits = c("1km cull","2km cull","3km cull","4km cull","5km cull","6km cull","7km cull","8km cull","9km cull","10km cull",
                              "1km vacc","2km vacc","3km vacc","4km vacc","5km vacc","6km vacc","7km vacc","8km vacc","9km vacc","10km vacc",
                              "Reactive-distance", "Reactive-popn", "Proactive-popn", "Proactive-density")) +
  scale_color_manual(labels = c("Ring culling","Ring vaccination","Active surveillance"), values = c("#1b9e77","#d95f02","#7570b3")) +
  xlab("Management objective") +
  ylab("Averted chickens culled (millions)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "bottom", legend.title=element_blank())

####################################################################################################

##########
# Data management for Fig 4.9 and 4.10
##########

# Load files
management_options <- readRDS("R files/management_options.rds") # Management options file
w2 <- readRDS("Results/Fixed single control/w2_div_fixed/summary_results.rds") # Wave 2
w5 <- readRDS("Results/Fixed single control/w5_div_fixed/summary_results.rds") # Wave 5

# Change limit and control to factors
management_options$limit = factor(management_options$limit, levels = c("100", "50", "20"))
management_options$control = factor(management_options$control, levels = c("1", "2", "3", "4"))

# Create results analysis data frame
w2_culled_ch_median <- w2_culled_ch_low <- w2_culled_ch_high <- 
  w5_culled_ch_median <- w5_culled_ch_low <- w5_culled_ch_high <-
  rep(NA, nrow(management_options))

# To calculate 95% range
low <- 0.025 
high <- 0.975 

for (i in 1:nrow(management_options)) {
  
  # Wave 2
  current <- w2[[i]] # Select results for this management option
  
  # Select comparator results
  if (management_options$limit[i] == 20) {comparator <- w2[[1]] # Low capacity
  } else if (management_options$limit[i] == 50) {comparator <- w2[[30]] # Medium capacity
  } else if (management_options$limit[i] == 100) {comparator <- w2[[59]]} # High capacity
  
  # Calculate averted chickens culled for each simulation run
  if (management_options$control[i] == 2) {
    # Averted chickens culled per ring culling visit
    difference <- (comparator$culled_chickens - current$culled_chickens)/(current$culled_premises - current$outbreak_size)
  } else if (management_options$control[i] == 3) {
    # Averted chickens culled per chicken vaccinated
    difference <- (comparator$culled_chickens - current$culled_chickens)/current$vaccinated_chickens
  } else if (management_options$control[i] == 4) {
    # Averted chickens culled per premises under surveillance
    if (management_options$type[i] == "reactive") {
      difference <- (comparator$culled_chickens - current$culled_chickens)/current$surveillance
      # difference <- (comparator$culled_chickens - current$culled_chickens)/as.numeric(management_options$limit[i])
    } else {
      difference <- (comparator$culled_chickens - current$culled_chickens)/(as.numeric(management_options$coverage[i])/ 100 * 13340)
    }
  } else {
    difference <- 0 # Comparator
  } 
  
  w2_culled_ch_median[i] <- median(difference, na.rm = TRUE) # Median chickens culled
  w2_culled_ch_low[i] <- quantile(difference, low, na.rm = TRUE) # Low % chickens culled
  w2_culled_ch_high[i] <- quantile(difference, high, na.rm = TRUE)  # High % chickens culled
  
  # Wave 5
  current <- w5[[i]] # Select results for this management option
  
  # Select comparator results
  if (management_options$limit[i] == 20) {comparator <- w5[[1]] # Low capacity
  } else if (management_options$limit[i] == 50) {comparator <- w5[[30]] # Medium capacity
  } else if (management_options$limit[i] == 100) {comparator <- w5[[59]]} # High capacity
  
  # Calculate averted chickens culled for each simulation run
  if (management_options$control[i] == 2) {
    # Averted chickens culled per ring culling visit
    difference <- (comparator$culled_chickens - current$culled_chickens)/(current$culled_premises - current$outbreak_size)
  } else if (management_options$control[i] == 3) {
    # Averted chickens culled per chicken vaccinated
    difference <- (comparator$culled_chickens - current$culled_chickens)/current$vaccinated_chickens
  } else if (management_options$control[i] == 4) {
    # Averted chickens culled per premises under surveillance
    if (management_options$type[i] == "reactive") {
      difference <- (comparator$culled_chickens - current$culled_chickens)/current$surveillance
      # difference <- (comparator$culled_chickens - current$culled_chickens)/as.numeric(management_options$limit[i])
    } else {
      difference <- (comparator$culled_chickens - current$culled_chickens)/(as.numeric(management_options$coverage[i])/ 100 * 13340)
    }
  } else {
    difference <- 0 # Comparator
  } 
  
  w5_culled_ch_median[i] <- median(difference, na.rm = TRUE)  # Median chickens culled
  w5_culled_ch_low[i] <- quantile(difference, low, na.rm = TRUE) # Low % chickens culled
  w5_culled_ch_high[i] <- quantile(difference, high, na.rm = TRUE) # High % chickens culled
}

averted <- data.frame(management_options, w2_culled_ch_median, w2_culled_ch_low, w2_culled_ch_high,
                      w5_culled_ch_median, w5_culled_ch_low, w5_culled_ch_high)

# Remove unused rows including comparator
averted <- averted[c(2:24,27,31:52,54,57,60:81,84,87),]

# Add management option label to data frame
averted['management_ID'] <- rep(c("1km cull","2km cull","3km cull","4km cull","5km cull","6km cull","7km cull","8km cull","9km cull","10km cull",
                                  "1km vacc","2km vacc","3km vacc","4km vacc","5km vacc","6km vacc","7km vacc","8km vacc","9km vacc","10km vacc",
                                  "Reactive-distance", "Reactive-popn", "Proactive-popn", "Proactive-density"), 3)

##########
# Figure 4.9
# Dot and whisker plot
# Wave 2 Averted Chickens Culled Per Control Intervention
##########
# Facet label names 
facet.labs <- c("Low capacity", "Medium capacity", "High capacity")
names(facet.labs) <- c("20", "50", "100")

ggplot(averted) +
  geom_point(aes(x = management_ID, y = w2_culled_ch_median/1000, colour = control)) +
  geom_errorbar(aes(x = management_ID, ymin = w2_culled_ch_low/1000, ymax = w2_culled_ch_high/1000, colour = control)) +
  facet_grid(limit ~ ., labeller = labeller(limit = facet.labs)) +
  scale_x_discrete(limits = c("1km cull","2km cull","3km cull","4km cull","5km cull","6km cull","7km cull","8km cull","9km cull","10km cull",
                              "1km vacc","2km vacc","3km vacc","4km vacc","5km vacc","6km vacc","7km vacc","8km vacc","9km vacc","10km vacc",
                              "Reactive-distance", "Reactive-popn", "Proactive-popn", "Proactive-density")) +
  scale_color_manual(labels = c("Ring culling","Ring vaccination","Active surveillance"), values = c("#1b9e77","#d95f02","#7570b3")) +
  xlab("Management objective") +
  ylab("Averted chickens culled (thousands) per control intervention") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "bottom", legend.title=element_blank())

##########
# Figure 4.10
# Dot and whisker plot
# Wave 5 Averted Chickens Culled Per Control Intervention
##########

# Facet label names 
facet.labs <- c("Low capacity", "Medium capacity", "High capacity")
names(facet.labs) <- c("20", "50", "100")

ggplot(averted) +
  geom_point(aes(x = management_ID, y = w5_culled_ch_median/1000, colour = control)) +
  geom_errorbar(aes(x = management_ID, ymin = w5_culled_ch_low/1000, ymax = w5_culled_ch_high/1000, colour = control)) +
  facet_grid(limit ~ ., labeller = labeller(limit = facet.labs)) +
  scale_x_discrete(limits = c("1km cull","2km cull","3km cull","4km cull","5km cull","6km cull","7km cull","8km cull","9km cull","10km cull",
                              "1km vacc","2km vacc","3km vacc","4km vacc","5km vacc","6km vacc","7km vacc","8km vacc","9km vacc","10km vacc",
                              "Reactive-distance", "Reactive-popn", "Proactive-popn", "Proactive-density")) +
  scale_color_manual(labels = c("Ring culling","Ring vaccination","Active surveillance"), values = c("#1b9e77","#d95f02","#7570b3")) +
  xlab("Management objective") +
  ylab("Averted chickens culled (thousands) per control intervention") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "bottom", legend.title=element_blank())

####################################################################################################

##########
# Data management for Table 4.2 - 4.7
##########

# Load files
management_options <- readRDS("R files/management_options.rds") # Management options file
w2 <- readRDS("Results/Fixed single control/w2_div_fixed/summary_results.rds") # Wave 2
w5 <- readRDS("Results/Fixed single control/w5_div_fixed/summary_results.rds") # Wave 5

# Change limit and control to factors
management_options$limit = factor(management_options$limit, levels = c("100", "50", "20"))
management_options$control = factor(management_options$control, levels = c("1", "2", "3", "4"))

# Create results analysis data frame
w2_cost_median <- w2_cost_low <- w2_cost_high <- 
  w2_cull_median <- w2_cull_low <- w2_cull_high <-
  w2_delta_cull_median <- w2_delta_cull_low <- w2_delta_cull_high <- 
  w5_cost_median <- w5_cost_low <- w5_cost_high <-
  w5_cull_median <- w5_cull_low <- w5_cull_high <-
  w5_delta_cull_median <- w5_delta_cull_low <- w5_delta_cull_high <-
  rep(NA, nrow(management_options))

# To calculate 95% range
low <- 0.025 
high <- 0.975

for (i in 1:nrow(management_options)) {
  
  # Wave 2
  current <- w2[[i]] # Select results for this management option
  
  # Select comparator results
  if (management_options$limit[i] == 20) {comparator <- w2[[1]] # Low capacity
  } else if (management_options$limit[i] == 50) {comparator <- w2[[30]] # Medium capacity
  } else if (management_options$limit[i] == 100) {comparator <- w2[[59]]} # High capacity
  
  # Calculate chicken culls 
  cull <- current$culled_chickens
  delta_cull <- comparator$culled_chickens - current$culled_chickens
  
  # Calculate cost (effort) for each simulation run
  if (management_options$control[i] == 2) {
    # Ring culling visits
    cost <- current$culled_premises - current$outbreak_size
  } else if (management_options$control[i] == 3) {
    # Chickens vaccinated
    cost <- current$vaccinated_chickens
  } else if (management_options$control[i] == 4) {
    # Premises under surveillance
    if (management_options$type[i] == "reactive") {
      cost <- current$surveillance
    } else {
      cost <- as.numeric(management_options$coverage[i])/ 100 * 13340
    }
  } else {
    cost <- rep(0, nrow(current)) # Comparator
  } 
  
  w2_cost_median[i] <- signif(median(cost, na.rm = TRUE), 3) # Median cost
  w2_cost_low[i] <- signif(quantile(cost, low, na.rm = TRUE), 3) # Low % cost
  w2_cost_high[i] <- signif(quantile(cost, high, na.rm = TRUE), 3)  # High % cost
  w2_cull_median[i] <- signif(median(cull, na.rm = TRUE), 3) # Median culls
  w2_cull_low[i] <- signif(quantile(cull, low, na.rm = TRUE), 3) # Low % culls
  w2_cull_high[i] <- signif(quantile(cull, high, na.rm = TRUE), 3)  # High % culls
  w2_delta_cull_median[i] <- signif(median(delta_cull, na.rm = TRUE), 3) # Median cull change
  w2_delta_cull_low[i] <- signif(quantile(delta_cull, low, na.rm = TRUE), 3) # Low % cull change
  w2_delta_cull_high[i] <- signif(quantile(delta_cull, high, na.rm = TRUE), 3)  # High % cull change
  
  # Wave 5
  current <- w5[[i]] # Select results for this management option
  
  # Select comparator results
  if (management_options$limit[i] == 20) {comparator <- w5[[1]] # Low capacity
  } else if (management_options$limit[i] == 50) {comparator <- w5[[30]] # Medium capacity
  } else if (management_options$limit[i] == 100) {comparator <- w5[[59]]} # High capacity
  
  # Calculate chicken culls 
  cull <- current$culled_chickens 
  delta_cull <- comparator$culled_chickens - current$culled_chickens
  
  # Calculate cost for each simulation run
  if (management_options$control[i] == 2) {
    # Ring culling visits
    cost <- current$culled_premises - current$outbreak_size
  } else if (management_options$control[i] == 3) {
    # Chickens vaccinated
    cost <- current$vaccinated_chickens
  } else if (management_options$control[i] == 4) {
    # Premises under surveillance
    if (management_options$type[i] == "reactive") {
      cost <- current$surveillance
    } else {
      cost <- as.numeric(management_options$coverage[i])/ 100 * 13340
    }
  } else {
    # Comparator
    cost <- rep(0, nrow(current)) # Comparator
  }
  
  w5_cost_median[i] <- signif(median(cost, na.rm = TRUE), 3) # Median cost
  w5_cost_low[i] <- signif(quantile(cost, low, na.rm = TRUE), 3) # Low % cost
  w5_cost_high[i] <- signif(quantile(cost, high, na.rm = TRUE), 3)  # High % cost
  w5_cull_median[i] <- signif(median(cull, na.rm = TRUE), 3) # Median culls
  w5_cull_low[i] <- signif(quantile(cull, low, na.rm = TRUE), 3) # Low % culls
  w5_cull_high[i] <- signif(quantile(cull, high, na.rm = TRUE), 3)  # High % culls
  w5_delta_cull_median[i] <- signif(median(delta_cull, na.rm = TRUE), 3) # Median cull change
  w5_delta_cull_low[i] <- signif(quantile(delta_cull, low, na.rm = TRUE), 3) # Low % cull change
  w5_delta_cull_high[i] <- signif(quantile(delta_cull, high, na.rm = TRUE), 3)  # High % cull change
}

cost_effect <- data.frame(management_options, 
                          w2_cost_median, w2_cost_low, w2_cost_high,
                          w2_cull_median, w2_cull_low, w2_cull_high,
                          w2_delta_cull_median, w2_delta_cull_low, w2_delta_cull_high,
                          w5_cost_median, w5_cost_low, w5_cost_high,
                          w5_cull_median, w5_cull_low, w5_cull_high,
                          w2_delta_cull_median, w2_delta_cull_low, w2_delta_cull_high)

# Remove unused rows
cost_effect <- cost_effect[c(1:24,27,30:52,54,57,59:81,84,87),]

# Add management option label to data frame
cost_effect['management_ID'] <- rep(c("IP cull", "1km cull","2km cull","3km cull","4km cull","5km cull","6km cull","7km cull","8km cull","9km cull","10km cull",
                                      "1km vacc","2km vacc","3km vacc","4km vacc","5km vacc","6km vacc","7km vacc","8km vacc","9km vacc","10km vacc",
                                      "Reactive-distance", "Reactive-popn", "Proactive-popn", "Proactive-density"), 3)

##########
# Table 4.2
# ICER wave 2 ring culling
##########

# Select ring culling and comparator strategies medium capacity
w2_cull <- cost_effect[((cost_effect$control == 1| cost_effect$control == 2) & cost_effect$limit == 50),]
w2_cull <- w2_cull[,c(25,7:9,13:15)]

# Sort by cost 
w2_cull <- w2_cull[order(w2_cull$w2_cost_median), ]

##########
# Table 4.3
# ICER wave 2 ring vaccination
##########

# Select ring vaccination and comparator strategies medium capacity
w2_vax <- cost_effect[((cost_effect$control == 1| cost_effect$control == 3) & cost_effect$limit == 50),]
w2_vax <- w2_vax[,c(25,7:9,13:15)]

# Sort by cost 
w2_vax <- w2_vax[order(w2_vax$w2_cost_median), ]

##########
# Table 4.4
# ICER wave 2 active surveillance
##########

# Select surveillance and comparator strategies medium capacity
w2_sur <- cost_effect[((cost_effect$control == 1| cost_effect$control == 4) & cost_effect$limit == 50),]
w2_sur <- w2_sur[,c(25,7:9,13:15)]

# Sort by cost 
w2_sur <- w2_sur[order(w2_sur$w2_cost_median), ]

##########
# Table 4.5
# ICER wave 5 ring culling
##########

# Select ring culling and comparator strategies medium capacity
w5_cull <- cost_effect[((cost_effect$control == 1| cost_effect$control == 2) & cost_effect$limit == 50),]
w5_cull <- w5_cull[,c(25,16:18,22:24)]

# Sort by cost 
w5_cull <- w5_cull[order(w5_cull$w5_cost_median), ]

##########
# Table 4.6
# ICER wave 5 ring vaccination
##########

# Select ring vaccination and comparator strategies medium capacity
w5_vax <- cost_effect[((cost_effect$control == 1| cost_effect$control == 3) & cost_effect$limit == 50),]
w5_vax <- w5_vax[,c(25,16:18,22:24)]

# Sort by cost 
w5_vax <- w5_vax[order(w5_vax$w5_cost_median), ]

##########
# Table 4.7
# ICER wave 5 active surveillance
##########

# Select surveillance and comparator strategies medium capacity
w5_sur <- cost_effect[((cost_effect$control == 1| cost_effect$control == 4) & cost_effect$limit == 50),]
w5_sur <- w5_sur[,c(25,16:18,22:24)]

# Sort by cost 
w5_sur <- w5_sur[order(w5_sur$w5_cost_median), ]

##########
# Table A.1
# Number of infected premises, outbreak duration, number of chickens culled and number 
# of chickens vaccinated for each single phase control measure under wave 2 parameterisation
##########

# Load files
management_options <- readRDS("R files/management_options.rds") # Management options file

results <- readRDS("Results/Fixed single control/w2_div_fixed/summary_results.rds") # Wave 2

# Create results analysis data frame 
premises_median <- premises_2.5 <- premises_97.5 <-
  duration_median <- duration_2.5 <- duration_97.5 <-
  culled_ch_median <- culled_ch_2.5 <- culled_ch_97.5 <-
  vax_ch_median <- vax_ch_2.5 <- vax_ch_97.5 <- rep(NA, nrow(management_options))

for (i in 1:nrow(management_options)) {
  premises_median[i] <- median(results[[i]]$outbreak_size) # Median outbreak size
  premises_2.5[i] <- quantile(results[[i]]$outbreak_size, 0.0025) # 2.5th % outbreak size
  premises_97.5[i] <- quantile(results[[i]]$outbreak_size, 0.975) # 97.5th % outbreak size
  duration_median[i] <- median(results[[i]]$outbreak_duration) # Median outbreak duration
  duration_2.5[i] <- quantile(results[[i]]$outbreak_duration, 0.0025) # 2.5th % outbreak duration
  duration_97.5[i] <- quantile(results[[i]]$outbreak_duration, 0.975) # 97.5th % outbreak duration
  culled_ch_median[i] <- median(results[[i]]$culled_chickens) # Median chickens culled
  culled_ch_2.5[i] <- quantile(results[[i]]$culled_chickens, 0.0025) # 2.5th % chickens culled
  culled_ch_97.5[i] <- quantile(results[[i]]$culled_chickens, 0.975) # 97.5th % chickens culled
  vax_ch_median[i] <- median(results[[i]]$vaccinated_chickens) # Median chickens vaccinated
  vax_ch_2.5[i] <- quantile(results[[i]]$vaccinated_chickens, 0.0025) # 2.5th % chickens vaccinated
  vax_ch_97.5[i] <- quantile(results[[i]]$vaccinated_chickens, 0.975) # 97.5th % chickens vaccinated
}

w2 <- data.frame(management_options, premises_median, premises_2.5, premises_97.5, duration_median, 
                 duration_2.5, duration_97.5, culled_ch_median, culled_ch_2.5, culled_ch_97.5, vax_ch_median,
                 vax_ch_2.5, vax_ch_97.5)

# Remove unused rows
w2 <- w2[c(1:24,27,30:52,54,57,59:81,84,87),]

##########
# Table A.2
# Number of infected premises, outbreak duration, number of chickens culled and number of 
# chickens vaccinated for each single phase control measure under wave 5 parameterisation
##########

# Load files
management_options <- readRDS("R files/management_options.rds") # Management options file

results <- readRDS("Results/Fixed single control/w5_div_fixed/summary_results.rds") # Wave 5

# Create results analysis data frame
premises_median <- premises_2.5 <- premises_97.5 <-
  duration_median <- duration_2.5 <- duration_97.5 <-
  culled_ch_median <- culled_ch_2.5 <- culled_ch_97.5 <-
  vax_ch_median <- vax_ch_2.5 <- vax_ch_97.5 <- rep(NA, nrow(management_options))

for (i in 1:nrow(management_options)) {
  premises_median[i] <- median(results[[i]]$outbreak_size) # Median outbreak size
  premises_2.5[i] <- quantile(results[[i]]$outbreak_size, 0.0025) # 2.5th % outbreak size
  premises_97.5[i] <- quantile(results[[i]]$outbreak_size, 0.975) # 97.5th % outbreak size
  duration_median[i] <- median(results[[i]]$outbreak_duration) # Median outbreak duration
  duration_2.5[i] <- quantile(results[[i]]$outbreak_duration, 0.0025) # 2.5th % outbreak duration
  duration_97.5[i] <- quantile(results[[i]]$outbreak_duration, 0.975) # 97.5th % outbreak duration
  culled_ch_median[i] <- median(results[[i]]$culled_chickens) # Median chickens culled
  culled_ch_2.5[i] <- quantile(results[[i]]$culled_chickens, 0.0025) # 2.5th % chickens culled
  culled_ch_97.5[i] <- quantile(results[[i]]$culled_chickens, 0.975) # 97.5th % chickens culled
  vax_ch_median[i] <- median(results[[i]]$vaccinated_chickens) # Median chickens vaccinated
  vax_ch_2.5[i] <- quantile(results[[i]]$vaccinated_chickens, 0.0025) # 2.5th % chickens vaccinated
  vax_ch_97.5[i] <- quantile(results[[i]]$vaccinated_chickens, 0.975) # 97.5th % chickens vaccinated
}

w5 <- data.frame(management_options, premises_median, premises_2.5, premises_97.5, duration_median,
                 duration_2.5, duration_97.5, culled_ch_median, culled_ch_2.5, culled_ch_97.5, vax_ch_median,
                 vax_ch_2.5, vax_ch_97.5)

# Remove unused rows
w5 <- w5[c(1:24,27,30:52,54,57,59:81,84,87),]








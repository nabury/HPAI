###################
# Plots chapter 3 #
###################

setwd("")

library(cowplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(maps)
library(readxl)
library(tidyr)

##########
# Figure 3.1
# Map of premises and sizes
##########

# All premises data
poultrydata <- readRDS("bangladeshpoultrydata_clean.rds")

# Create categorical poultry size variable
poultrydata <- poultrydata %>% 
  mutate(premises_size = cut(Chickenno, breaks = c(-Inf, 500, 1000, 1500, 2000, 2500, Inf), 
                             labels=c("1-500","501-1000","1001-1500","1501-2000","2001-2500",">2500")))

# Bangladesh data from the world map
Bangladesh <- map_data("world")
Bangladesh <- Bangladesh[Bangladesh$region == "Bangladesh",]

ggplot() +
  geom_point(data = poultrydata, aes(x = Digital_East, y = Digital_North, col = premises_size), alpha = 0.7) +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.0) +
  scale_color_discrete("Chicken population") +
  theme_void() 

##########
# Figure 3.2
# Histogram of flock sizes
##########

# All premises data
poultrydata <- readRDS("bangladeshpoultrydata_clean.rds")

# Skewed - find 95% percentile (4,000)
quantile(poultrydata$Chickenno, probs = 0.95)
poultrydata$Chickenno[poultrydata$Chickenno > 5000] <- 5000

# Histogram
ggplot() +
  geom_histogram(data = poultrydata, aes(Chickenno), 
                 fill = I("#b5c5dd"), col = I("black")) + 
  scale_x_continuous(breaks = c(1000, 2000, 3000, 4000, 5000), labels = c("1000", "2000", "3000", "4000", ">=5000")) +
  labs(x = "Flock size", y = "Frequency") +
  theme_bw() +
  theme(panel.border = element_blank())

##########
# Figure 3.4
# Map of w2 locations
##########

data <- read_excel("HPAI_Outbreaks_2012_All.xlsx")

# Bangladesh data from the world map
Bangladesh <- map_data("world")
Bangladesh <- Bangladesh[Bangladesh$region == "Bangladesh",]

# Start (21st Sep 2007)
wave_2_start <- data %>%
  filter((Date_of_inf >= "2007-09-21 00:00:00 UTC") & (Date_of_inf <= "2007-09-22 00:00:00 UTC")) %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Y_Digital_North, X_Digital_East) # Keep only needed columns

p1 <- ggplot() +
  geom_point(data = wave_2_start, aes(x = X_Digital_East, y = Y_Digital_North), col = "#b23542") +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.0) +
  theme_void() +
  labs(title = "  21st September 2007")

# Start of December
wave_2_Dec <- data %>%
  filter((Date_of_inf >= "2007-09-21 00:00:00 UTC") & (Date_of_inf <= "2007-12-02 00:00:00 UTC")) %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Y_Digital_North, X_Digital_East) # Keep only needed columns

p2 <- ggplot() +
  geom_point(data = wave_2_Dec, aes(x = X_Digital_East, y = Y_Digital_North), col = "#b23542") +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.0) +
  theme_void() +
  labs(title = "1st December 2007")

# Start of February
wave_2_Feb <- data %>%
  filter((Date_of_inf >= "2007-09-21 00:00:00 UTC") & (Date_of_inf <= "2008-02-02 00:00:00 UTC")) %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Y_Digital_North, X_Digital_East) # Keep only needed columns

p3 <- ggplot() +
  geom_point(data = wave_2_Feb, aes(x = X_Digital_East, y = Y_Digital_North), col = "#b23542") +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.0) +
  theme_void() +
  labs(title = "1st February 2008")

# Start of April
wave_2_Apr <- data %>%
  filter((Date_of_inf >= "2007-09-21 00:00:00 UTC") & (Date_of_inf <= "2008-04-02 00:00:00 UTC")) %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Y_Digital_North, X_Digital_East) # Keep only needed columns

p4 <- ggplot() +
  geom_point(data = wave_2_Apr, aes(x = X_Digital_East, y = Y_Digital_North), col = "#b23542") +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.0) +
  theme_void() +
  labs(title = "1st April 2008")


# End (19th May 2007)
wave_2_end <- data %>%
  filter((Date_of_inf >= "2007-09-21 00:00:00 UTC") & (Date_of_inf <= "2008-05-20 00:00:00 UTC")) %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Y_Digital_North, X_Digital_East) # Keep only needed columns

p5 <- ggplot() +
  geom_point(data = wave_2_end, aes(x = X_Digital_East, y = Y_Digital_North), col = "#b23542") +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.0) +
  theme_void() +
  labs(title = "19th May 2008")

plot <- plot_grid(p1, p2, p3, p4, p5, nrow = 1)

plot


##########
# Figure 3.5
# Map of w5 locations
##########

data <- read_excel("HPAI_Outbreaks_2012_All.xlsx")

# Subset only the Bangladesh data from the world map
Bangladesh <- map_data("world")
Bangladesh <- Bangladesh[Bangladesh$region == "Bangladesh",]

# Start (1st January 2011)
wave_5_start <- data %>%
  filter((Date_of_inf >= "2011-01-01 00:00:00 UTC") & (Date_of_inf <= "2011-01-02 00:00:00 UTC")) %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Y_Digital_North, X_Digital_East) # Keep only needed columns

p1 <- ggplot() +
  geom_point(data = wave_5_start, aes(x = X_Digital_East, y = Y_Digital_North), col = "#b23542") +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.0) +
  theme_void() +
  labs(title = "  1st January 2011")

# Start of February
wave_5_Feb <- data %>%
  filter((Date_of_inf >= "2011-01-01 00:00:00 UTC") & (Date_of_inf <= "2011-02-02 00:00:00 UTC")) %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Y_Digital_North, X_Digital_East) # Keep only needed columns

p2 <- ggplot() +
  geom_point(data = wave_5_Feb, aes(x = X_Digital_East, y = Y_Digital_North), col = "#b23542") +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.0) +
  theme_void() +
  labs(title = "  1st February 2011")

# Start of March
wave_5_Mar <- data %>%
  filter((Date_of_inf >= "2011-01-01 00:00:00 UTC") & (Date_of_inf <= "2011-03-02 00:00:00 UTC")) %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Y_Digital_North, X_Digital_East) # Keep only needed columns

p3 <- ggplot() +
  geom_point(data = wave_5_Mar, aes(x = X_Digital_East, y = Y_Digital_North), col = "#b23542") +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.0) +
  theme_void() +
  labs(title = "  1st March 2011")

# Start of April
wave_5_Apr <- data %>%
  filter((Date_of_inf >= "2011-01-01 00:00:00 UTC") & (Date_of_inf <= "2011-04-02 00:00:00 UTC")) %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Y_Digital_North, X_Digital_East) # Keep only needed columns

p4 <- ggplot() +
  geom_point(data = wave_5_Apr, aes(x = X_Digital_East, y = Y_Digital_North), col = "#b23542") +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.0) +
  theme_void() +
  labs(title = "  1st April 2011")


# End (9th May 2011)
wave_5_end <- data %>%
  filter((Date_of_inf >= "2011-01-01 00:00:00 UTC") & (Date_of_inf <= "2011-05-10 00:00:00 UTC")) %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Y_Digital_North, X_Digital_East) # Keep only needed columns

p5 <- ggplot() +
  geom_point(data = wave_5_end, aes(x = X_Digital_East, y = Y_Digital_North), col = "#b23542") +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.0) +
  theme_void() +
  labs(title = "  9th May 2011")

plot <- plot_grid(p1, p2, p3, p4, p5, nrow = 1)

plot

##########
# Figure 3.12
# Wave 2 simulated outbreak sizes, time series of IPs and chickens culled
##########

# Outbreak data
real_world <- read_excel("HPAI_Outbreaks_2012_All.xlsx")

# Wave 2 Dhaka division cases
wave_2 <- real_world %>%
  filter((Date_of_inf >= "2007-09-21 00:00:00 UTC") & (Date_of_inf <= "2008-05-20 00:00:00 UTC") & Division == "Dhaka") %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Tot_chickens, Date_culling) # Keep only needed columns

# Simulation results
w2_sim <- readRDS("Results/Verification/detailed_results4.rds")
results <- readRDS("Results/Verification/outbreak_size_4.rds")

#####
# Histogram of outbreak sizes
#####

results$outbreak_size[results$outbreak_size > 1000] <- 1000 
observed <- data.frame(x = 109, y = 250)

p1 <- ggplot() +
  geom_col(data = observed, aes(x, y), fill = I("#9d9e9c"), width = 10) +
  geom_histogram(data = results, aes(outbreak_size), 
                 fill = I("#5aaa66"), col = I("black")) +
  scale_x_continuous(breaks = c(250, 500, 750, 1000), labels = c("250", "500", "750", ">=1000")) +
  scale_y_continuous(breaks = c(50, 100, 150,200,250), labels = c("0.05", "0.10", "0.15", "0.20", "0.25")) +
  labs(#title = "Simulated premises outbreak sizes in the Dhaka division across 1,000 simulations using wave 2 parameterisation", 
    #subtitle = "The observed outbreak size of 109 infected premises is represented by the grey bar", 
    x = "Premises outbreak size", 
    y = "Frequency") +
  theme_bw() +
  theme(panel.border = element_blank())

#####
# Wave 2 cases
#####

# Wave 2 real world cases by day including dates with no cases
wave_2_cases <- wave_2 %>% 
  mutate(Date_of_inf = as.Date(Date_of_inf)) %>% # Convert to date format
  count(Date_of_inf) %>% # Count by date
  complete(Date_of_inf = seq.Date(min(Date_of_inf)-1, max(Date_of_inf)+1, by="day")) # Add days with no cases

wave_2_cases$n[is.na(wave_2_cases$n)] <- 0 # Change days with no cases from NA to 0

# Wave 2 simulation cases by day

w2_sim_cases <- list()

for (i in 1:1000) {
  w2_sim_cases[[i]] <- w2_sim[[i]] %>% 
    count(inf_day) %>% # Cases by day
    complete(inf_day = 1:176) # Add days with no cases - max time 176 days
  
  w2_sim_cases[[i]]$n[is.na(w2_sim_cases[[i]]$n)] <- 0 # Change days with no cases from NA to 0
  w2_sim_cases[[i]] <- subset(w2_sim_cases[[i]], inf_day!=0) # Remove farms not infected
} 

w2_sim_cases <- rbindlist(w2_sim_cases) # Convert to long format data frame

# Calculate summary results
date <- seq(as.Date("2007-11-26"), as.Date("2008-05-19"), by="days")
mean <- rep(NA,176)
high <- rep(NA,176)

for (i in 1:176) {
  sub <- w2_sim_cases[w2_sim_cases$inf_day==i]
  mean[i] <- mean(sub$n) # Calculate mean
  high[i] <-  quantile(sub$n, 0.95) # Calculate 95th percentile
}

w2_sim_cases <- data.frame(date, mean, high) # Convert to data frame

# Plot
p2 <- ggplot(wave_2_cases) +
  geom_line(aes(x=Date_of_inf, y = n, colour = "Real world")) +
  geom_line(data = w2_sim_cases, aes(x = date, y = mean, colour = "Mean")) +
  geom_line(data = w2_sim_cases, aes(x = date, y = high, colour = "95th percentile")) +
  xlab("Date") +
  ylab("Infected premises") +
  scale_color_manual(name = "Time series",
                     breaks = c("Real world", "Mean", "95th percentile"),
                     values = c("Real world"="black", "Mean"="#167ee0", "95th percentile"="#db9723")) +
  theme(legend.position = "top")

#####
# Chickens culled
#####

# Wave 2 real world chickens culled by day
wave_2_culled <- wave_2 %>% 
  mutate(Date_culling = as.Date(Date_culling)) %>% # Convert to date format
  group_by(Date_culling) %>% 
  summarise(Chickens_culled = sum(Tot_chickens)) %>% 
  complete(Date_culling = seq.Date(min(Date_culling)-1, max(Date_culling)+1, by="day")) # Add days with no cases

wave_2_culled$Chickens_culled[is.na(wave_2_culled$Chickens_culled)] <- 0 # Change days with no cases from NA to 0

# Wave 2 simulation chickens culled by day

w2_sim_chickens <- list()

for (i in 1:1000) {
  w2_sim_chickens[[i]] <- w2_sim[[i]] %>% 
    group_by(cull_day) %>% 
    summarise(Culled_chickens = sum(Chickens)) %>% # Calculate numbers of chickens culled by day
    complete(cull_day = 1:176) # Add days with no culling - max time 176 days
  
  w2_sim_chickens[[i]]$Culled_chickens[is.na(w2_sim_chickens[[i]]$Culled_chickens)] <- 0 # Change days with no cases from NA to 0
  w2_sim_chickens[[i]] <- subset(w2_sim_chickens[[i]], cull_day!=0) # Remove farms not culled
} 

w2_sim_chickens <- rbindlist(w2_sim_chickens) # Convert to long format data frame

# Calculate summary results
date <- seq(as.Date("2007-11-26"), as.Date("2008-05-19"), by="days")
mean <- rep(NA,176)
high <- rep(NA,176)

for (i in 1:176) {
  sub <- w2_sim_chickens[w2_sim_chickens$cull_day==i]
  mean[i] <- mean(sub$Culled_chickens) # Calculate mean
  high[i] <-  quantile(sub$Culled_chickens, 0.95) # Calculate 95th percentile
}

w2_sim_chickens <- data.frame(date, mean, high) # Convert to data frame

# Plot
p3 <- ggplot(wave_2_culled) +
  geom_line(aes(x = Date_culling, y = Chickens_culled, colour = "Real world")) +
  geom_line(data = w2_sim_chickens, aes(x = date, y = mean, colour = "Mean")) +
  geom_line(data = w2_sim_chickens, aes(x = date, y = high, colour = "95th percentile")) +
  xlab("Date") +
  ylab("Chickens culled") +
  scale_color_manual(name = "Time series",
                     breaks = c("Real world", "Mean", "95th percentile"),
                     values = c("Real world"="black", "Mean"="#167ee0", "95th percentile"="#db9723")) +
  theme(legend.position = "top")

#####
# Grid plot
#####

plot <- plot_grid(p1, p2, p3, nrow = 1, labels = c("A", "B", "C"))

plot

##########
# Figure 3.13
# Wave 5 simulated outbreak sizes, time series of IPs and chickens culled
##########

# Outbreak data
real_world <- read_excel("HPAI_Outbreaks_2012_All.xlsx")

# Wave 5 Dhaka division cases
wave_5 <- real_world %>%
  filter((Date_of_inf >= "2011-01-01 00:00:00 UTC") & (Date_of_inf <= "2011-05-10 00:00:00 UTC") & Division == "Dhaka") %>% # Keep only dates in range
  dplyr::select(Date_of_inf, Tot_chickens, Date_culling) # Keep only needed columns

# Simulation results
w5_sim <- readRDS("Results/Verification/detailed_results8.rds")
results <- readRDS("Results/Verification/outbreak_size_8.rds")

#####
# Histogram of outbreak sizes
#####

results$outbreak_size[results$outbreak_size > 2500] <- 2500
observed <- data.frame(x = 75, y = 250)

p1 <- ggplot() +
  geom_col(data = observed, aes(x, y), fill = I("#9d9e9c"), width = 10) +
  geom_histogram(data = results, aes(outbreak_size), 
                 fill = I("#5aaa66"), col = I("black")) +
  scale_x_continuous(breaks = c(500, 1000, 1500, 2000, 2500), labels = c("500", "1000", "1500", "2000", ">=2500")) +
  scale_y_continuous(breaks = c(50, 100, 150,200,250), labels = c("0.05", "0.10", "0.15", "0.20", "0.25")) +
  labs(x = "Premises outbreak size", y = "Frequency") +
  theme_bw() +
  theme(panel.border = element_blank())


#####
# Cases
#####

# Wave 5 cases by day including dates with no cases
wave_5_cases <- wave_5 %>% 
  mutate(Date_of_inf = as.Date(Date_of_inf)) %>% # Convert to date format
  count(Date_of_inf) %>% # Count by date
  complete(Date_of_inf = seq.Date(min(Date_of_inf)-1, max(Date_of_inf)+1, by="day")) # Add days with no cases

wave_5_cases$n[is.na(wave_5_cases$n)] <- 0 # Change days with no cases from NA to 0

# Wave 5 simulation cases by day

w5_sim_cases <- list()

for (i in 1:1000) {
  w5_sim_cases[[i]] <- w5_sim[[i]] %>% 
    count(inf_day) %>% # Cases by day
    complete(inf_day = 1:129) # Add days with no cases - max time 129 days
  
  w5_sim_cases[[i]]$n[is.na(w5_sim_cases[[i]]$n)] <- 0 # Change days with no cases from NA to 0
  w5_sim_cases[[i]] <- subset(w5_sim_cases[[i]], inf_day!=0) # Remove farms not infected
} 

w5_sim_cases <- rbindlist(w5_sim_cases) # Convert to long format data frame

# Calculate summary results
date <- seq(as.Date("2011-01-01"), as.Date("2011-05-09"), by="days")
mean <- rep(NA,129)
high <- rep(NA,129)

for (i in 1:129) {
  sub <- w5_sim_cases[w5_sim_cases$inf_day==i]
  mean[i] <- mean(sub$n) # Calculate mean
  high[i] <-  quantile(sub$n, 0.95) # Calculate 95th percentile
}

w5_sim_cases <- data.frame(date, mean, high) # Convert to data frame

# Plot
p2 <- ggplot(wave_5_cases) +
  geom_line(aes(x=Date_of_inf, y = n, colour = "Real world")) +
  geom_line(data = w5_sim_cases, aes(x = date, y = mean, colour = "Mean")) +
  geom_line(data = w5_sim_cases, aes(x = date, y = high, colour = "95th percentile")) +
  xlab("Date") +
  ylab("Infected premises") +
  scale_color_manual(name = "Time series",
                     breaks = c("Real world", "Mean", "95th percentile"),
                     values = c("Real world"="black", "Mean"="#167ee0", "95th percentile"="#db9723")) +
  theme(legend.position = "top")

  
#####
# Wave 5 chickens
#####

# Wave 5 chickens culled by day
wave_5_culled <- wave_5 %>% 
  mutate(Date_culling = as.Date(Date_culling)) %>% # Convert to date format
  group_by(Date_culling) %>% 
  summarise(Chickens_culled = sum(Tot_chickens)) %>% 
  complete(Date_culling = seq.Date(min(Date_culling)-1, max(Date_culling)+1, by="day")) # Add days with no cases

wave_5_culled$Chickens_culled[is.na(wave_5_culled$Chickens_culled)] <- 0 # Change days with no cases from NA to 0

# Wave 5 simulation chickens culled by day

w5_sim_chickens <- list()

for (i in 1:1000) {
  w5_sim_chickens[[i]] <- w5_sim[[i]] %>% 
    group_by(cull_day) %>% 
    summarise(Culled_chickens = sum(Chickens)) %>% # Calculate numbers of chickens culled by day
    complete(cull_day = 1:129) # Add days with no culling - max time 129 days
  
  w5_sim_chickens[[i]]$Culled_chickens[is.na(w5_sim_chickens[[i]]$Culled_chickens)] <- 0 # Change days with no cases from NA to 0
  w5_sim_chickens[[i]] <- subset(w5_sim_chickens[[i]], cull_day!=0) # Remove farms not culled
} 

w5_sim_chickens <- rbindlist(w5_sim_chickens) # Convert to long format data frame

# Calculate summary results
date <- seq(as.Date("2011-01-01"), as.Date("2011-05-09"), by="days")
mean <- rep(NA,129)
high <- rep(NA,129)

for (i in 1:129) {
  sub <- w5_sim_chickens[w5_sim_chickens$cull_day==i]
  mean[i] <- mean(sub$Culled_chickens) # Calculate mean
  high[i] <-  quantile(sub$Culled_chickens, 0.95) # Calculate 95th percentile
}

w5_sim_chickens <- data.frame(date, mean, high) # Convert to data frame

# Plot
p3 <- ggplot(wave_5_culled) +
  geom_line(aes(x = Date_culling, y = Chickens_culled, colour = "Real world")) +
  geom_line(data = w5_sim_chickens, aes(x = date, y = mean, colour = "Mean")) +
  geom_line(data = w5_sim_chickens, aes(x = date, y = high, colour = "95th percentile")) +
  scale_y_continuous(breaks = c(0, 100000, 200000, 300000), labels = c("0", "100,000", "200,000", "300,000")) +
  xlab("Date") +
  ylab("Chickens culled") +
  scale_color_manual(name = "Time series",
                     breaks = c("Real world", "Mean", "95th percentile"),
                     values = c("Real world"="black", "Mean"="#167ee0", "95th percentile"="#db9723")) +
  theme(axis.text.y = element_text(angle=45), legend.position = "top")


#####
# Grid plot
#####

plot <- plot_grid(p1, p2, p3, nrow = 1, labels = c("A", "B", "C"))

plot





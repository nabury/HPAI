###################
# Plots chapter 5 #
###################

# Set to wd containing plot code and associated files
setwd("")

library(cowplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

##########
# Table 5.2 
# Summary statistics
###########

# Load files
low <- readRDS("low_trans.rds")
med <- readRDS("med_trans.rds")
high <- readRDS("high_trans.rds")

# Subset into w2 and wave 5
low_w2 <- low[low$wave == "Wave 2",]
low_w5 <- low[low$wave == "Wave 5",]
med_w2 <- med[med$wave == "Wave 2",]
med_w5 <- med[med$wave == "Wave 5",]
high_w2 <- high[high$wave == "Wave 2",]
high_w5 <- high[high$wave == "Wave 5",]

# Compare mean outbreak size
mean(low_w2$outbreak_size)
mean(low_w5$outbreak_size)
mean(med_w2$outbreak_size)
mean(med_w5$outbreak_size)
mean(high_w2$outbreak_size)
mean(high_w5$outbreak_size)

# Compare median outbreak size
median(low_w2$outbreak_size)
median(low_w5$outbreak_size)
median(med_w2$outbreak_size)
median(med_w5$outbreak_size)
median(high_w2$outbreak_size)
median(high_w5$outbreak_size)

# Compare 95th percentile outbreak size
quantile(low_w2$outbreak_size, 0.95)
quantile(low_w5$outbreak_size, 0.95)
quantile(med_w2$outbreak_size, 0.95)
quantile(med_w5$outbreak_size, 0.95)
quantile(high_w2$outbreak_size, 0.95)
quantile(high_w5$outbreak_size, 0.95)

# Compare mean outbreak duration
mean(low_w2$outbreak_duration)
mean(low_w5$outbreak_duration)
mean(med_w2$outbreak_duration)
mean(med_w5$outbreak_duration)
mean(high_w2$outbreak_duration)
mean(high_w5$outbreak_duration)

# Compare median outbreak duration
median(low_w2$outbreak_duration)
median(low_w5$outbreak_duration)
median(med_w2$outbreak_duration)
median(med_w5$outbreak_duration)
median(high_w2$outbreak_duration)
median(high_w5$outbreak_duration)

# Compare 95th percentile outbreak duration
quantile(low_w2$outbreak_duration, 0.95)
quantile(low_w5$outbreak_duration, 0.95)
quantile(med_w2$outbreak_duration, 0.95)
quantile(med_w5$outbreak_duration, 0.95)
quantile(high_w2$outbreak_duration, 0.95)
quantile(high_w5$outbreak_duration, 0.95)

# Compare mean culled premises
mean(low_w2$culled_premises)
mean(low_w5$culled_premises)
mean(med_w2$culled_premises)
mean(med_w5$culled_premises)
mean(high_w2$culled_premises)
mean(high_w5$culled_premises)

# Compare median culled premises
median(low_w2$culled_premises)
median(low_w5$culled_premises)
median(med_w2$culled_premises)
median(med_w5$culled_premises)
median(high_w2$culled_premises)
median(high_w5$culled_premises)

# Compare 95th percentile culled premises
quantile(low_w2$culled_premises, 0.95)
quantile(low_w5$culled_premises, 0.95)
quantile(med_w2$culled_premises, 0.95)
quantile(med_w5$culled_premises, 0.95)
quantile(high_w2$culled_premises, 0.95)
quantile(high_w5$culled_premises, 0.95)

###################################################################################################

#########
# Data management for single phase increased transmission results
# Fig 5.1, 5.2, 5.5, 5.6, 5.7, 5.8
#########

# Load files and add transmissibility column
low <- readRDS("low_trans.rds")
low$trans <- rep("100%", nrow(low))

med <- readRDS("med_trans.rds")
med$trans <- rep("150%", nrow(med))

high <- readRDS("high_trans.rds")
high$trans <- rep("200%", nrow(high))

# Merge dataframes 
results_df <- rbind(low, med, high)

# Change transmissibility to a factor
results_df$trans <- factor(results_df$trans, levels = c("100%", "150%", "200%"))

##########
# Figure 5.1 
# Box plot
# Outbreak size multiphase scenarios wave 2 increased transmission
# Data management see below 
###########

# Filter - multiphase scenarios, wave 2, low and high capacity
results <- filter(results_df, control_text_long %in% c("IP cull", "1km cull", "3km cull", "7km cull",
                                                       "1km vacc", "3km vacc", "7km vacc", "Proactive-popn") & 
                    wave %in% "Wave 2" &
                    capacity %in% c("Low capacity", "High capacity")) 

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = outbreak_size), width = 0.5, fill = "#e28743") +
  facet_grid(trans ~ capacity) +
  xlab("Control measure") +
  ylab("Premises outbreak size") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))



##########
# Figure 5.2
# Box plot
# Outbreak size multiphase scenarios wave 5 increased transmission
# Data management see below
###########

# Filter - multiphase scenarios, wave 5, low and high capacity
results <- filter(results_df, control_text_long %in% c("IP cull", "1km cull", "3km cull", "7km cull",
                                                       "1km vacc", "3km vacc", "7km vacc", "Proactive-popn") & 
                    wave %in% "Wave 5" &
                    capacity %in% c("Low capacity", "High capacity"))

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = outbreak_size), width = 0.5, fill = "#e28743") +
  facet_grid(trans ~ capacity) +
  xlab("Control measure") +
  ylab("Premises outbreak size") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

##########
# Figure 5.5
# Log transformed box plot
# Outbreak size all scenarios wave 2 increased transmission
# Data management see below
###########

results <- filter(results_df, wave %in% "Wave 2")

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = outbreak_size), width = 0.5, fill = "#e28743") +
  facet_grid(trans ~ capacity) +
  scale_y_log10() +
  xlab("Control measure") +
  ylab("Premises outbreak size") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

##########
# Figure 5.6
# Box plot
# Outbreak size all scenarios wave 5 increased transmission
###########

results <- filter(results_df, wave %in% "Wave 5")

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = outbreak_size), width = 0.5, fill = "#e28743") +
  facet_grid(trans ~ capacity) +
  xlab("Control measure") +
  ylab("Premises outbreak size") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

##########
# Figure 5.7
# Log transformed box plot
# Outbreak duration ring culling wave 2 increased transmission
###########

results <- filter(results_df, control_text_long %in% c("1km cull", "2km cull", "3km cull", "4km cull", "5km cull",
                                                       "6km cull", "7km cull", "8km cull", "9km cull", "10km cull") & 
                    wave %in% "Wave 2" )

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = outbreak_duration), width = 0.5, fill = "#e28743") +
  facet_grid(trans ~ capacity) +
  scale_y_log10() +
  xlab("Ring culling radius (km)") +
  ylab("Outbreak duration (days") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

##########
# Figure 5.8
# Log transformed box plot
# Outbreak duration ring culling wave 5 increased transmission
###########

results <- filter(results_df, control_text_long %in% c("1km cull", "2km cull", "3km cull", "4km cull", "5km cull",
                                                       "6km cull", "7km cull", "8km cull", "9km cull", "10km cull") & 
                    wave %in% "Wave 5" )

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = outbreak_duration), width = 0.5, fill = "#e28743") +
  facet_grid(trans ~ capacity) +
  scale_y_log10() +
  xlab("Ring culling radius (km)") +
  ylab("Outbreak duration (days") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

##############################################################################################################

#####
# Fig 5.3
# Outbreak size histograms increased transmission
# IP culling medium capacity
#####

# Wave 2 all premises data
low <- readRDS("w2_div_100_summary_results.rds")
med <- readRDS("w2_div_150_summary_results.rds")
high <- readRDS("w2_div_200_summary_results.rds")

obj <- 30

p1 <- ggplot() +
  geom_histogram(data = low[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(100, 200, 300, 400, 500), 
                     labels = c("0.1", "0.2", "0.3", "0.4", "0.5")) +
  labs(x = "", y = "Frequency") +
  theme_bw() +
  theme(panel.border = element_blank()) 

p2 <- ggplot() +
  geom_histogram(data = med[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(100, 200, 300, 400, 500), 
                     labels = c("0.1", "0.2", "0.3", "0.4", "0.5"), 
                     limits = c(0, 500)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.border = element_blank()) 

p3 <- ggplot() +
  geom_histogram(data = high[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(100, 200, 300, 400, 500), 
                     labels = c("0.1", "0.2", "0.3", "0.4", "0.5"),
                     limits = c(0, 500)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.border = element_blank()) 

# Wave 5 all premises data
low <- readRDS("w5_div_100_summary_results.rds")
med <- readRDS("w5_div_150_summary_results.rds")
high <- readRDS("w5_div_200_summary_results.rds")

obj <- 30

p4 <- ggplot() +
  geom_histogram(data = low[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(200, 400, 600, 800),
                     labels = c("0.2", "0.4", "0.6", "0.8")) +
  labs(x = "", y = "Frequency") +
  theme_bw() +
  theme(panel.border = element_blank()) 

p5 <- ggplot() +
  geom_histogram(data = med[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(200, 400, 600, 800),
                     labels = c("0.2", "0.4", "0.6", "0.8"),
                     limits = c(0, 800)) +
  labs(x = "Premises outbreak size", y = "") +
  theme_bw() +
  theme(panel.border = element_blank()) 

p6 <- ggplot() +
  geom_histogram(data = high[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(200, 400, 600, 800),
                     labels = c("0.2", "0.4", "0.6", "0.8"),
                     limits = c(0, 800)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.border = element_blank()) 

plot <- plot_grid(p1, p2, p3,p4, p5, p6, nrow = 2)
plot

##########
# Fig 5.4 Time series plot
# Increased transmission wave 2 and 5 - IP culling, medium capacity (30)
##########

results <- readRDS("Fig5-4.rds")

# W2 Infected premises subplot
w2_df <- results[[1]]
results_df <- w2_df[1:300,] # Trim high values

p1 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,200) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Transmissibility",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "none", axis.title = element_text(size = 12,face = "bold")) 

# W5 Infected premises subplot
w5_df <- results[[2]]
results_df <- w5_df[1:500,] # Trim high values

p2 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  xlab("Outbreak duration (days)") +
  ylab("") +
  scale_color_manual(name = "Transmissibility",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "right", 
        legend.text=element_text(size=12),
        axis.title = element_text(size = 12,face = "bold")) 

# Grid plot
plot <- plot_grid(p1, p2, nrow = 1, labels = c("Wave 2", "Wave 5"), hjust = -1)
plot

##########
# Table 5.4
###########

# Load files
med <- readRDS("med_popn.rds")
high <- readRDS("high_popn.rds")

# Subset into w2 and wave 5
med_w2 <- med[med$wave == "Wave 2",]
med_w5 <- med[med$wave == "Wave 5",]
high_w2 <- high[high$wave == "Wave 2",]
high_w5 <- high[high$wave == "Wave 5",]

# Compare mean outbreak size
mean(med_w2$outbreak_size)
mean(med_w5$outbreak_size)
mean(high_w2$outbreak_size)
mean(high_w5$outbreak_size)

# Compare median outbreak size
median(med_w2$outbreak_size)
median(med_w5$outbreak_size)
median(high_w2$outbreak_size)
median(high_w5$outbreak_size)

# Compare 95th percentile outbreak size
quantile(med_w2$outbreak_size, 0.95)
quantile(med_w5$outbreak_size, 0.95)
quantile(high_w2$outbreak_size, 0.95)
quantile(high_w5$outbreak_size, 0.95)

# Compare mean outbreak duration
mean(med_w2$outbreak_duration)
mean(med_w5$outbreak_duration)
mean(high_w2$outbreak_duration)
mean(high_w5$outbreak_duration)

# Compare median outbreak duration
median(med_w2$outbreak_duration)
median(med_w5$outbreak_duration)
median(high_w2$outbreak_duration)
median(high_w5$outbreak_duration)

# Compare 95th percentile outbreak duration
quantile(med_w2$outbreak_duration, 0.95)
quantile(med_w5$outbreak_duration, 0.95)
quantile(high_w2$outbreak_duration, 0.95)
quantile(high_w5$outbreak_duration, 0.95)

# Compare mean culled premises
mean(med_w2$culled_premises)
mean(med_w5$culled_premises)
mean(high_w2$culled_premises)
mean(high_w5$culled_premises)

# Compare median culled premises
median(med_w2$culled_premises)
median(med_w5$culled_premises)
median(high_w2$culled_premises)
median(high_w5$culled_premises)

# Compare 95th percentile culled premises
quantile(med_w2$culled_premises, 0.95)
quantile(med_w5$culled_premises, 0.95)
quantile(high_w2$culled_premises, 0.95)
quantile(high_w5$culled_premises, 0.95)

##############################################################################################################

#########
# Data management for single phase intensification results used for Fig 5.11 and 5.12
#########

# Load files and add population column
low <- readRDS("low_trans.rds")
low$popn <- rep("100%", nrow(low))

med <- readRDS("med_popn.rds")
med$popn <- rep("150%", nrow(med))

high <- readRDS("high_popn.rds")
high$popn <- rep("200%", nrow(high))

# Merge dataframes 
results_df <- rbind(low, med, high)

# Change population to a factor
results_df$popn <- factor(results_df$popn, levels = c("100%", "150%", "200%")) 

##########
# Figure 5.11
# Log transformed box plot
# Outbreak size all scenarios wave 2 intensification
###########

results <- filter(results_df, wave %in% "Wave 2")

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = outbreak_size), width = 0.5, fill = "#e28743") +
  facet_grid(popn ~ capacity) +
  scale_y_log10() +
  xlab("Control measure") +
  ylab("Premises outbreak size") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

##########
# Figure 5.12
# Box plot
# Outbreak size all scenarios wave 5 intensification
# Data management see below
###########

results <- filter(results_df, wave %in% "Wave 5")

ggplot(results) +
  geom_boxplot(aes(x = control_text_long, y = outbreak_size), width = 0.5, fill = "#e28743") +
  facet_grid(popn ~ capacity) +
  xlab("Control measure") +
  ylab("Premises outbreak size") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

##############################################################################################################

#####
# Fig 5.13
# Outbreak size histograms increased intensification
# IP culling medium capacity
#####
# W2 all premises data
low <- readRDS("w2_div_100_summary_results.rds")
med <- readRDS("popn_w2_1.5_summary_results.rds")
high <- readRDS("popn_w2_2_summary_results.rds")

obj <- 30

p1 <- ggplot() +
  geom_histogram(data = low[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(200, 400, 600, 800),
                     labels = c("0.2", "0.4", "0.6", "0.8")) +
  labs(x = "", y = "Frequency") +
  theme_bw() +
  theme(panel.border = element_blank()) 

p2 <- ggplot() +
  geom_histogram(data = med[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(50, 100, 150, 200),
                     labels = c("0.2", "0.4", "0.6", "0.8"),
                     limits = c(0, 200)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.border = element_blank()) 

p3 <- ggplot() +
  geom_histogram(data = high[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(50, 100, 150, 200),
                     labels = c("0.2", "0.4", "0.6", "0.8")) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.border = element_blank()) 

# W5 all premises data
low <- readRDS("w5_div_100_summary_results.rds")
med <- readRDS("popn_w5_1.5_summary_results.rds")
high <- readRDS("popn_w5_2_summary_results.rds")

obj <- 30

p4 <- ggplot() +
  geom_histogram(data = low[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(200, 400, 600, 800, 1000),
                     labels = c("0.2", "0.4", "0.6", "0.8", "1"),
                     limits = c(0,1000)) +
  labs(x = "", y = "Frequency") +
  theme_bw() +
  theme(panel.border = element_blank()) 

p5 <- ggplot() +
  geom_histogram(data = med[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(50,100,150,200,250),
                     labels = c("0.2", "0.4", "0.6", "0.8", "1"),
                     limits = c(0, 250)) +
  labs(x = "Premises outbreak size", y = "") +
  theme_bw() +
  theme(panel.border = element_blank()) 

p6 <- ggplot() +
  geom_histogram(data = high[[obj]], aes(outbreak_size), fill = I("#5aaa66"), col = I("black")) +
  scale_x_log10() +
  scale_y_continuous(breaks = c(50,100,150,200,250),
                     labels = c("0.2", "0.4", "0.6", "0.8", "1"),
                     limits = c(0, 250)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.border = element_blank()) 

plot <- plot_grid(p1, p2, p3, p4, p5, p6, nrow = 2)
plot

##########
# Fig 5.14 Time series plot
# Increased intensification wave 2 and 5 - IP culling, medium capacity (30).
##########

results <- readRDS("Fig5-14.rds")

# Wave 2 Infected premises subplot
w2_df <- results[[1]]

results_df <- w2_df[1:400,] # Trim high values

p1 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,350) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Intensification",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "none") 

# Wave 5 Infected premises subplot
w5_df <- results[[2]]
results_df <- w5_df[1:400,] # Trim high values

p2 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  xlab("Outbreak duration (days)") +
  ylab("") +
  scale_color_manual(name = "Intensification",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "right")

# Grid plot
plot <- plot_grid(p1, p2, nrow = 1, labels = c("Wave 2", "Wave 5"), hjust = -1)
plot

##########
# Fig A 13 Time series plots
# Increased transmission wave 2  
# Ring cull 3km low (4), ring vaccination 7km medium(47), proactive surveillance high (84)
##########

results <- readRDS("FigA13.rds")

# 3km ring cull low subplot

ring_cull <- results[[1]]
results_df <- ring_cull[1:500,] # Trim high values

p1 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Transmissibility",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "none")

# 7km ring vaccination medium subplot 

ring_vaxx <- results[[2]]
results_df <- ring_vaxx[1:500,] # Trim high values

p2 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,150) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Transmissibility",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "none")

## Proactive surveillance by population high subplot

proactive <- results[[3]]
results_df <- proactive[1:500,] # Trim high values

p3 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,150) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Transmissibility",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "right")

# Grid plot
plot <- plot_grid(p1, p2, p3, nrow = 1, labels = c("A", "B", "C"))
plot

##########
# Fig A 14 Time series plots
# Increased transmission wave 5  
# Ring cull 3km low (4), ring vaccination 7km medium (47), proactive surveillance high (84)
##########

results <- readRDS("FigA14.rds")

# 3km ring cull low subplot

ring_cull <- results[[1]]

results_df <- ring_cull[1:400,] # Trim high values

p1 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Transmissibility",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "none")

# 7km ring vaccination medium subplot

ring_vaxx <- results[[2]]

results_df <- ring_vaxx[1:400,] # Trim high values

p2 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,200) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Transmissibility",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "none")

# Proactive surveillance by population high subplot

proactive <- results[[3]]

results_df <- proactive[1:400,] # Trim high values

p3 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,200) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Transmissibility",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "right")

# Grid plot
plot <- plot_grid(p1, p2, p3, nrow = 1, labels = c("A", "B", "C"))
plot

##########
# Fig A 15 Time series plots
# Increased intensification wave 2  
# Ring cull 3km low (4), ring vaccination 7km medium (47), proactive surveillance high (84)
##########

results <- readRDS("FigA15.rds")

# 3km ring cull low subplot

ring_cull <- results[[1]]

results_df <- ring_cull[1:500,] # Trim high values

p1 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,200) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Intensification",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "none")

# 7km ring vaccination medium subplot

ring_vaxx <- results[[2]]

results_df <- ring_vaxx[1:500,] # Trim high values

p2 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,200) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Intensification",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "none")

# Proactive surveillance by population high subplot

proactive <- results[[3]]

results_df <- proactive[1:500,] # Trim high values

p3 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,200) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Intensification",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "right")

# Grid plot
plot <- plot_grid(p1, p2, p3, nrow = 1, labels = c("A", "B", "C"))
plot

##########
# Fig A 16 Time series plots
# Increased intensification wave 5  
# Ring cull 3km low (4), ring vaccination 7km medium (47), proactive surveillance high (84)
##########

results <- readRDS("FigA16.rds")

# 3km ring cull low subplot

ring_cull <- results[[1]]

results_df <- ring_cull[1:750,] # Trim high values

p1 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,350) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Intensification",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "none")

# 7km ring vaccination medium subplot

ring_vaxx <- results[[2]]

results_df <- ring_vaxx[1:750,] # Trim high values

p2 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,350) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Intensification",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "none")

# Proactive surveillance by population high subplot

proactive <- results[[3]]

results_df <- proactive[1:750,] # Trim high values

p3 <- ggplot(results_df) +
  geom_line(aes(x = time, y = baseline_mean_premises, colour = "Baseline")) +
  geom_line(aes(x = time, y = med_mean_premises, colour = "Medium")) +
  geom_line(aes(x = time, y = high_mean_premises, colour = "High")) +
  ylim(0,350) +
  xlab("Outbreak duration (days)") +
  ylab("Infected premises (mean)") +
  scale_color_manual(name = "Intensification",
                     breaks = c("Baseline", "Medium", "High"),
                     values = c("Baseline"="#86a040", "Medium"="#5f4ebf", "High"="#cc8d30")) +
  theme_minimal() +
  theme(legend.position = "right")

# Grid plot
plot <- plot_grid(p1, p2, p3, nrow = 1, labels = c("A", "B", "C"))
plot




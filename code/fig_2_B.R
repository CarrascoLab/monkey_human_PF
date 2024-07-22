library(ggplot2)
library(tidyr)
library(dplyr)
library(plotrix)
# library(export) # UNCOMMENT IF SAVING THE PRODUCED PLOT! 
library(superb)
library(rstatix)

directory = "/Volumes/purplab/EXPERIMENTS/1_Current_Experiments/Ekin/monkeyPF"
setwd(directory)

# comparing Asymmetry ratio or dprime data:
file_name = "data"
location_info = "cardinals"

# Load the data
data_tbl = read.csv((sprintf("%s/data_to_analyze/%s_botheyes_%s.csv", directory, file_name, location_info)), header = T)

# Convert to long format
data_tbl <- data_tbl %>%
  gather(key = "location", value = "sensitivity", UVM, LVM, LHM, RHM) %>%
  mutate(id = as.factor(id))  # Ensure id is treated as a factor

# Calculate the mean sensitivity for each observer (id)
observer_means <- data_tbl %>%
  group_by(group, id) %>%
  summarise(mean_sensitivity = mean(sensitivity, na.rm = TRUE))

# get grand mean
grand_means <- data_tbl %>%
  group_by(group) %>%
  summarise(mean_sensitivity = mean(sensitivity, na.rm = TRUE))

# Step 3: Join the observer means with the original data
data_corrected <- left_join(data_tbl, observer_means, by = c("group", "id"))

# Join the grand means with the corrected data
data_corrected <- data_tbl %>%
  left_join(observer_means, by = c("id", "group")) %>%
  left_join(grand_means, by = "group")

colnames(data_corrected)[colnames(data_corrected) == "mean_sensitivity.x"] <- "observer_means"
colnames(data_corrected)[colnames(data_corrected) == "mean_sensitivity.y"] <- "grand_means"

data_corrected <- data_corrected %>% mutate(corrected_sensitivity = sensitivity - observer_means + grand_means)

# Calculate the number of subjects in each group
nSubj <- data_corrected %>%
  group_by(group) %>%
  summarise(n = n_distinct(id))

# Calculate the standard deviation for each group and location
std_dev <- data_corrected %>%
  group_by(group, location) %>%
  summarise(std = sd(corrected_sensitivity, na.rm = TRUE))

# Merge to get the number of subjects for each group
std_dev <- std_dev %>%
  left_join(nSubj, by = "group")

# Calculate the standard error of the mean (SEM)
std_dev <- std_dev %>%
  mutate(sem = std / sqrt(n))

# Apply Morey's correction
numConditions <- 4
M <- sqrt(numConditions / (numConditions - 1))
std_dev <- std_dev %>%
  mutate(corrected_sem = sem * M)

# Select the relevant columns for the final output
sem_data <- std_dev %>%
  select(group, location, corrected_sem)

# now load the normalized data that will be plotted:
file_name = "normalized_data"

# Load the data
data_tbl = read.csv((sprintf("%s/data_to_analyze/%s_botheyes_%s.csv", directory, file_name, location_info)), header = T)
data_tbl <- data_tbl %>%
  gather(key = "location", value = "sensitivity", UVM, LVM, LHM, RHM)
data_tbl$location <- factor(data_tbl$location, levels = c('UVM', 'LVM', 'LHM', 'RHM'))

library(plyr)

data_summary <- function(data, varname, groupnames) {
  summary_func <- function(x, col) {
    c(mean = mean(x[[col]], na.rm = TRUE),
      sem = std.error(x[[col]], na.rm = TRUE))
  }
  data_sum <- ddply(data, groupnames, .fun = summary_func, varname)
  colnames(data_sum)[which(colnames(data_sum) == "mean")] <- varname
  return(data_sum)
}

df2 <- data_summary(data_tbl, varname = "sensitivity", groupnames = c("group", "location"))
df2 <- as_tibble(df2)


df2$location <- as.factor(df2$location)
df2$group <- as.factor(df2$group)
head(df2)

sem_data$group <- as.factor(sem_data$group)
sem_data$location <- as.factor(sem_data$location)

# Perform the join to update the sem values with corrected_sem
df2_updated <- df2 %>%
  left_join(sem_data, by = c("group", "location")) %>%
  mutate(sem = corrected_sem) %>%
  select(-corrected_sem)

# Default bar plot
p <- ggplot(df2_updated, aes(x = location, y = sensitivity, fill = group, color = as.factor(group), shape = as.factor(group))) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = sensitivity - sem, ymax = sensitivity + sem), width = 0.3, position = position_dodge(1))

p + labs(x = "location", y = "sensitivity") +
  theme_classic() +
  scale_fill_manual(values = c('black', 'red'))


print(p)

# Save the plot to a PowerPoint file
#graph2office(file = "fig_1_B.pptx", width = 5, height = 3)

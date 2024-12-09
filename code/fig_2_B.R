fig_2_B <- function(directory) {

# File paths for new data including the third group
file_name = "data_wbinocularhuman"
location_info = "cardinals"

if (comp_cond == 1) {
  add_label = 'monocularParticipants'
} else if (comp_cond == 0) {
  add_label = 'allBinocularParticipants'
}

# Load the data with three groups
data_tbl = read.csv((sprintf("%s/data_to_analyze/%s_%s_%s.csv", directory, file_name, location_info, add_label)), header = T)

# Convert to long format
data_tbl <- data_tbl %>%
  gather(key = "location", value = "sensitivity", UVM, LVM, LHM, RHM) %>%
  mutate(id = as.factor(id))  # Ensure id is treated as a factor

if ("package:plyr" %in% search()) {
  detach("package:plyr", unload = TRUE)
}

# Calculate the mean sensitivity for each observer (id) including the third group
observer_means <- data_tbl %>%
  group_by(group, id) %>%
  summarise(mean_sensitivity = mean(sensitivity, na.rm = TRUE))

# Calculate grand mean per group (including the third group)
grand_means <- data_tbl %>%
  group_by(group) %>%
  summarise(mean_sensitivity = mean(sensitivity, na.rm = TRUE))

# Join the observer means and grand means with the original data
data_corrected <- data_tbl %>%
  left_join(observer_means, by = c("group", "id")) %>%
  left_join(grand_means, by = "group")

# Rename columns to avoid confusion
colnames(data_corrected)[colnames(data_corrected) == "mean_sensitivity.x"] <- "observer_means"
colnames(data_corrected)[colnames(data_corrected) == "mean_sensitivity.y"] <- "grand_means"

# Compute corrected sensitivity
data_corrected <- data_corrected %>%
  mutate(corrected_sensitivity = sensitivity - observer_means + grand_means)

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

# Select relevant columns for the final output
sem_data <- std_dev %>%
  select(group, location, corrected_sem)

# Load normalized data for plotting
file_name = "normalized_data_wbinocularhuman"
data_tbl = read.csv((sprintf("%s/data_to_analyze/%s_%s_%s.csv", directory, file_name, location_info, add_label)), header = T)

# Convert normalized data to long format
data_tbl <- data_tbl %>%
  gather(key = "location", value = "sensitivity", UVM, LVM, LHM, RHM)

# Ensure locations are in the correct order
data_tbl$location <- factor(data_tbl$location, levels = c('UVM', 'LVM', 'LHM', 'RHM'))

data_summary <- function(data, varname, groupnames) {
  data %>%
    group_by(across(all_of(groupnames))) %>%
    summarise(
      mean = mean(.data[[varname]], na.rm = TRUE),
      sem = std.error(.data[[varname]], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(!!varname := mean)
}

df2 <- data_summary(data_tbl, varname = "sensitivity", groupnames = c("group", "location"))
df2 <- as_tibble(df2)

# Update SEM values using corrected SEM from sem_data
df2$location <- as.factor(df2$location)
df2$group <- as.factor(df2$group)
sem_data$group <- as.factor(sem_data$group)
sem_data$location <- as.factor(sem_data$location)

# Join to update SEM values
df2_updated <- df2 %>%
  left_join(sem_data, by = c("group", "location")) %>%
  mutate(sem = corrected_sem) %>%
  select(-corrected_sem)

# Ensure group order: group 1, group 3, then group 2
df2_updated$group <- factor(df2_updated$group, levels = c("1", "3", "2"))

# Updated plot with custom colors
p <- ggplot(df2_updated, aes(x = location, y = sensitivity, fill = group)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = sensitivity - sem, ymax = sensitivity + sem), width = 0.2, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c('black', 'gray', 'red')) +  # Custom colors: black for group 1, gray for group 2, red for group 3
  labs(x = "Location", y = "Normalized sensitivity (d')") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top"
  ) +
  facet_wrap(~ group, scales = "free_x", labeller = as_labeller(c(`1` = "Group 1", `2` = "Group 2", `3` = "Group 3"))) +
  theme(strip.text = element_text(size = 14, face = "bold"))

print(p)

# Save the plot if needed
#graph2office(file = "templatePlot.pptx", width = 5, height = 3)

}

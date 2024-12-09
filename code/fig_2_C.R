fig_2_C <- function(human, human_mono, monkey) {  

# Extract the asymmetry data from each group and convert it to a data frame
# Each group's asymmetries have columns "HVA" and "VMA"
human_data <- as.data.frame(human$asymmetries) %>% mutate(group = "Human")
colnames(human_data) <- c("HVA", "VMA", "group")

human_mono_data <- as.data.frame(human_mono$asymmetries) %>% mutate(group = "Human Binocular")
colnames(human_mono_data) <- c("HVA", "VMA", "group")

monkey_data <- as.data.frame(monkey$asymmetries) %>% mutate(group = "Monkey")
colnames(monkey_data) <- c("HVA", "VMA", "group")

# Combine all data into one data frame
combined_asymmetry_data <- bind_rows(human_data, human_mono_data, monkey_data)

# Reshape the data to long format for easier plotting
long_asymmetry_data <- combined_asymmetry_data %>%
  pivot_longer(cols = c("HVA", "VMA"), names_to = "Asymmetry", values_to = "Ratio") %>%
  mutate(group = factor(group, levels = c("Human", "Monkey", "Human Binocular")),
         Asymmetry = factor(Asymmetry, levels = c("HVA", "VMA")))

# Calculate means and standard errors for each group and asymmetry
summary_asymmetry_data <- long_asymmetry_data %>%
  group_by(group, Asymmetry) %>%
  dplyr::summarise(
    mean_ratio = mean(Ratio, na.rm = TRUE),
    se = sd(Ratio, na.rm = TRUE) / sqrt(length(Ratio)),
    .groups = 'drop'
  )

# Set color mapping for groups as requested
color_mapping <- c("Human" = "black", "Monkey" = "red", "Human Binocular" = "gray")

# Create the plot
plot_asymmetry <- ggplot(summary_asymmetry_data, aes(x = Asymmetry, y = mean_ratio, color = group)) +
  geom_point(aes(shape = group), size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean_ratio - se, ymax = mean_ratio + se),
                width = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = color_mapping) +
  labs(x = "", y = "Ratio of Perceptual Asymmetry") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(. ~ Asymmetry)

# Print the plot
print(plot_asymmetry)
}
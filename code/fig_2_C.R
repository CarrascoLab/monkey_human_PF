# Load necessary libraries
library(ggplot2)

# Prepare individual data frames for each group, updating the label to "Human Monocular"
human_data <- as.data.frame(human$dprime_normalized) %>%
  mutate(group = "Human", color = "black")
human_monocular_data <- as.data.frame(human_mono$dprime_normalized) %>%
  mutate(group = "Human Monocular", color = "gray")
monkey_data <- as.data.frame(monkey$dprime_normalized) %>%
  mutate(group = "Monkey", color = "red")  # Set color to red for Monkey group

# Combine data into one data frame
combined_data <- bind_rows(human_data, human_monocular_data, monkey_data)

# Rename columns for clarity
colnames(combined_data)[c(3, 7)] <- c("Upper_VM", "Lower_VM")

# Convert `group` to a factor with specified levels to ensure legend includes all groups
combined_data$group <- factor(combined_data$group, levels = c("Human", "Monkey", "Human Monocular"))

# Calculate group means for each group
group_means <- combined_data %>%
  group_by(group) %>%
  summarise(
    mean_upper_vm = mean(Upper_VM, na.rm = TRUE),
    mean_lower_vm = mean(Lower_VM, na.rm = TRUE)
  ) %>%
  mutate(color = c("black", "red", "gray"))  # Ensure color for group means is consistent

# Determine common axis range across all points and means
all_values <- c(combined_data$Upper_VM, combined_data$Lower_VM, group_means$mean_upper_vm, group_means$mean_lower_vm)
axis_limits <- range(all_values, na.rm = TRUE)

# Create the scatter plot with smaller circles for individual points and consistent stroke width
p <- ggplot() +
  # Plot individual points for each subject with smaller size and consistent outline (circle)
  geom_point(data = combined_data, aes(x = Upper_VM, y = Lower_VM, color = group), shape = 21, size = 2, stroke = 1.5) +
  # Plot group means with a cross shape and larger size
  geom_point(data = group_means, aes(x = mean_upper_vm, y = mean_lower_vm, color = group), shape = 3, size = 5, stroke = 2) +
  # Add the unity line (y = x) for reference
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  # Add x and y axis lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  labs(x = "Upper Vertical Meridian (d')", y = "Lower Vertical Meridian (d')", color = "Group") +
  scale_color_manual(values = c("Human" = "black", "Monkey" = "red", "Human Monocular" = "gray")) +
  coord_equal(xlim = axis_limits, ylim = axis_limits) +  # Equal axis range and square layout
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top"
  )

# Print the plot
print(p)

# Export the plot to PowerPoint
# Note: Ensure the "officer" and "rvg" packages are installed to enable export
library(officer)
library(rvg)
dml_pptx <- dml(ggobj = p)
read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(dml_pptx, location = ph_location_fullsize()) %>%

  
print(target = "templatePlot.pptx")

# Load necessary libraries
library(ggplot2)
library(cowplot)

# Ensure that the lengths of human$hva and human_mono$hva match
# If `human` has fewer data points, subset `human_mono` to match
if (length(human$hva) < length(human_mono$hva)) {
  human_mono$hva <- human_mono$hva[1:length(human$hva)]
  human_mono$vma <- human_mono$vma[1:length(human$vma)]
} else if (length(human$hva) > length(human_mono$hva)) {
  human$hva <- human$hva[1:length(human_mono$hva)]
  human$vma <- human$vma[1:length(human_mono$vma)]
}

# Determine common axis range across both HVA and VMA variables
all_values <- c(human$hva, human_mono$hva, human$vma, human_mono$vma)
axis_limits <- range(all_values, na.rm = TRUE)

# Define a function to create scatter plots with equal axes, identity line, and same axis range
create_scatter_plot <- function(x, y, x_label, y_label, limits) {
  ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_point(shape = 21, color = "blue", fill = "lightblue", size = 3, stroke = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "gray") +  # Identity line
    labs(x = x_label, y = y_label) +
    coord_equal(xlim = limits, ylim = limits) +  # Set the same limits for x and y axes
    theme_minimal()
}

# Create scatter plots for HVA and VMA comparisons with the same axis limits
p1 <- create_scatter_plot(human_mono$hva, human$hva, "Monocular HVA", "Binocular HVA", axis_limits)
p2 <- create_scatter_plot(human_mono$vma, human$vma, "Monocular VMA", "Binocular VMA", axis_limits)

# Arrange the two plots side by side with equal sizing using cowplot
plot_grid(p1, p2, ncol = 2, align = "hv")

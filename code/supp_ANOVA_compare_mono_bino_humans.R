# Load necessary library
library(ez)
library(dplyr)

# Ensure data dimensions match by removing any extra rows
if (nrow(human$dprime) < nrow(human_mono$dprime)) {
  human_mono$dprime <- human_mono$dprime[1:nrow(human$dprime), ]
} else if (nrow(human$dprime) > nrow(human_mono$dprime)) {
  human$dprime <- human$dprime[1:nrow(human_mono$dprime), ]
}

# Create a data frame in long format for repeated measures ANOVA
# Assuming each row in `human$dprime` and `human_mono$dprime` corresponds to a participant

# Define participant IDs
participants <- 1:nrow(human$dprime)

# Convert data to long format, with columns for Condition, Location, and Performance
data_long <- data.frame(
  Participant = rep(participants, times = 2 * 8),
  Condition = rep(c("Binocular", "Monocular"), each = nrow(human$dprime) * 8),
  Location = factor(rep(1:8, each = nrow(human$dprime), times = 2)),
  Performance = c(as.vector(human$dprime), as.vector(human_mono$dprime))
)

# Run the repeated measures ANOVA using the ezANOVA function
anova_results <- ezANOVA(
  data = data_long,
  dv = Performance,
  wid = Participant,
  within = .(Condition, Location),
  detailed = TRUE
)

# Print the ANOVA results
print(anova_results)

# Define column indices
rhm_column <- 1
uvm_column <- 3
lhm_column <- 5
lvm_column <- 7
cohendz <- list()

# Ensure that data frames are the same length by trimming excess rows if needed
if (nrow(human$dprime) < nrow(human_mono$dprime)) {
  human_mono$dprime <- human_mono$dprime[1:nrow(human$dprime), ]
} else if (nrow(human$dprime) > nrow(human_mono$dprime)) {
  human$dprime <- human$dprime[1:nrow(human_mono$dprime), ]
}

# Extract UVM and LVM columns for both conditions and perform contrast
uvm_values <- human$dprime[, uvm_column]
lvm_values <- human$dprime[, lvm_column]

uvm_lvm_contrast <- t.test(uvm_values, lvm_values, paired = TRUE)
tval_uvm_lvm <- uvm_lvm_contrast$statistic["t"]
cohendz$vertical <- abs(tval_uvm_lvm / sqrt(length(uvm_values)))
print("Contrast 1: UVM vs. LVM")
print(uvm_lvm_contrast)

# Calculate horizontal and vertical averages for each participant
horizontal_values <- rowMeans(human$dprime[, c(rhm_column, lhm_column)])
vertical_values <- rowMeans(human$dprime[, c(uvm_column, lvm_column)])

# Perform the horizontal vs. vertical contrast
hv_contrast <- t.test(horizontal_values, vertical_values, paired = TRUE)
tval_hv <- hv_contrast$statistic["t"]
cohendz$horizontal <- abs(tval_hv / sqrt(length(horizontal_values)))
print("Contrast 2: Horizontal (RHM & LHM) vs. Vertical (UVM & LVM)")
print(hv_contrast)

subj <- c('SJ1', 'SJ2', 'SJ3', 'SJ4','SJ5', 'SJ6', 'SJ7','SJ8','SJ9', 'SJ10', 'SJ11', 
  'SJ12', 'SJ13', 'SJ14', 'SJ15',  'SJ16', 'SJ17',  'SJ18', 'SJ19', 'SJ20', 'SJ21',
  'SJ22', 'SJ23', 'SJ24', 'SJ25')

# Set data directory
project_dir <- "/Volumes/purplab/EXPERIMENTS/1_Current_Experiments/Ekin/monkeyPF"
setwd(project_dir)
data_dir <- file.path(project_dir, 'data_to_analyze/')

# Preallocate output vectors
angles <- c(0, 45, 90, 135, 180, 225, 270, 315)
num_locs <- length(angles)

# Initialize human data structures
human <- list(
  dprime = matrix(NA, nrow = length(subj), ncol = num_locs),
  reaction_time = matrix(NA, nrow = length(subj), ncol = num_locs)
)


calculate_dprime <- function(data) {
  
  trial_info <- rep(0, dim(data)[1])
  
  for (t in 1:length(trial_info)) {
    if (data[t, 1] == 1 && data[t, 2] == 1) {
      trial_info[t] <- 1  # hits
    } else if (data[t, 1] == -1 && data[t, 2] == 1) {
      trial_info[t] <- 2  # false alarms
    } else if (data[t, 1] == 1 && data[t, 2] == -1) {
      trial_info[t] <- 3  # misses
    } else if (data[t, 1] == -1 && data[t, 2] == -1) {
      trial_info[t] <- 4  # correct rejects
    }
  }
  
  h_ind <- trial_info == 1
  fa_ind <- trial_info == 2
  misses <- trial_info == 3
  crs <- trial_info == 4
  
  hits <- (sum(h_ind) + 0.5) / (sum(h_ind) + sum(misses) + 1)
  false_alarms <- (sum(fa_ind) + 0.5) / (sum(fa_ind) + sum(crs) + 1)
  dprime <- qnorm(hits) - qnorm(false_alarms)
  
  return(dprime)
}


# Loop through subjects
for (sb in 1:length(subj)) {
  output <- read.csv(paste0(data_dir, sprintf('subj%i_cleaned_output.csv', sb)), header = FALSE)
  # Loop through target locations
  for (target in 1:num_locs) {
    curr_dat <- output[output[, 1] == angles[target], ]
    curr_dat[curr_dat[, 3] == 0, 3] <- -1
    human$dprime[sb, target] <- calculate_dprime(curr_dat[, 2:3])
    human$reaction_time[sb, target] <- mean(curr_dat[, 4], na.rm = TRUE)
  }
}

# Identify and remove outliers
outlier_ids <- rowMeans(human$dprime) < 0.17
outlier_ids <- as.numeric(outlier_ids)
outlier_ids[outlier_ids > 0] <- 1
human$dprime <- human$dprime[outlier_ids != 1, ]
human$reaction_time <- human$reaction_time[outlier_ids != 1, ]

# Calculate hva and vma
human$rhm_column <- 1
human$uvm_column <- 3
human$lhm_column <- 5
human$lvm_column <- 7

human$dprime_normalized <- human$dprime / rowMeans(human$dprime)

human$hm <- rowMeans(cbind(human$dprime_normalized[, human$rhm_column], human$dprime_normalized[, human$lhm_column]), na.rm = TRUE)
human$vm <- rowMeans(cbind(human$dprime_normalized[, human$uvm_column], human$dprime_normalized[, human$lvm_column]), na.rm = TRUE)
human$uvm <- human$dprime_normalized[, human$uvm_column]
human$lvm <- human$dprime_normalized[, human$lvm_column]

human$uvm[human$uvm == 0] <- 0.5  # Correct for infinity

human$hva <- ((human$hm - human$vm) / (human$vm + human$hm))
human$vma <- ((human$lvm - human$uvm) / (human$lvm + human$uvm))

human$asymmetries <- cbind(human$hva, human$vma)

# Normalize the data
human_full_vf_data <- colMeans(human$dprime_normalized)
human_fullvf_RT_data <- colMeans(human$reaction_time)

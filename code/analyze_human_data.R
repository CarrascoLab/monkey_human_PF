analyze_human_data <- function(project_dir) {
  source(file.path(project_dir, 'code', 'calculate_dprime.R'))
  print('Calculating d-prime for human observers...')
  subj <- c('SJ1', 'SJ2', 'SJ3', 'SJ4','SJ5', 'SJ6', 'SJ7','SJ8','SJ9', 'SJ10', 'SJ11', 
    'SJ12', 'SJ13', 'SJ14', 'SJ15',  'SJ16', 'SJ17',  'SJ18', 'SJ19', 'SJ20', 'SJ21',
    'SJ22', 'SJ23', 'SJ24', 'SJ25')
  
  # Set data directory
  data_dir <- file.path(project_dir, 'data_to_analyze/')
  
  # Preallocate output vectors
  angles <- c(0, 45, 90, 135, 180, 225, 270, 315)
  num_locs <- length(angles)
  
  # Initialize human data structures
  human <- list(
    dprime = matrix(NA, nrow = length(subj), ncol = num_locs),
    reaction_time = matrix(NA, nrow = length(subj), ncol = num_locs)
  )
  
  # Loop through subjects
  for (sb in 1:length(subj)) {
    output <- read.csv(paste0(data_dir, sprintf('subj%i_cleaned_output.csv', sb)), header = FALSE)
    # Loop through target locations
    for (target in 1:num_locs) {
      curr_dat <- output[output[, 1] == angles[target], ]
      curr_dat[curr_dat[, 3] == 0, 3] <- -1
      human$dprime[sb, target] <- calculate_dprime(curr_dat[, 2:3], 'human')
      human$reaction_time[sb, target] <- mean(curr_dat[, 4], na.rm = TRUE)
    }
  }
  
  # Identify and remove outliers
  outlier_ids <- rowMeans(human$dprime) < 0.2
  outlier_ids <- as.numeric(outlier_ids)
  outlier_ids[outlier_ids > 0] <- 1
  human$dprime <- human$dprime[outlier_ids != 1, ]
  human$reaction_time <- human$reaction_time[outlier_ids != 1, ]
  
  # Calculate hva and vma
  rhm_column <- 1
  uvm_column <- 3
  lhm_column <- 5
  lvm_column <- 7
  
  human$dprime_normalized <- human$dprime / rowMeans(human$dprime)
  
  hm <- rowMeans(cbind(human$dprime_normalized[, rhm_column], human$dprime_normalized[, lhm_column]), na.rm = TRUE)
  vm <- rowMeans(cbind(human$dprime_normalized[, uvm_column], human$dprime_normalized[, lvm_column]), na.rm = TRUE)
  uvm <- human$dprime_normalized[, uvm_column]
  lvm <- human$dprime_normalized[, lvm_column]
  
  uvm[uvm == 0] <- 0.5  # Correct for infinity
  
  human$hva <- ((hm - vm) / (vm + hm))
  human$vma <- ((lvm - uvm) / (lvm + uvm))
  
  human$asymmetries <- cbind(human$hva, human$vma)
  
  # Normalize the data
  human$full_visual_field_dprime <- colMeans(human$dprime_normalized)
  human$full_visual_field_RT <- colMeans(human$reaction_time)
  
  return(human)
  
}
# Load required libraries


# Set project directory and load data
project_dir <- "/Volumes/purplab/EXPERIMENTS/1_Current_Experiments/Ekin/monkeyPF"
monkey_pf_data <- read.csv(file.path(project_dir, "Data", "organized_monkey_data.csv"))

# dataset var name: monkey_pf_data
# column information
# Col1 : Observer ID
# Col2 : Location index 1: 0deg, 2: 45deg, 3: 90 deg, 4: 135 deg, 5: 180
# deg, 6: 224 deg, 7: 270 deg, 8: 315
# Col3 : Tested eye ) 1:LE, 2:RE
# Col4 : Contrast level, with the lowest contrast tested represented by 1
# and the highest represented by 6. The absolute contrast level changed
# from observer to observer due to thresholding
# Col5 : attention condition, 1: cued focally, 2: neutral

att_indices <- c(1, 2, 0)  # 1:cued , 2: neutral, 0: both
angles <- c(0, 45, 90, 135, 180, 225, 270, 315)

# Initialize data structures
monkey.dprime_attention_conds <- array(NA, dim = c(2, 3, length(unique(monkey_pf_data[,1])), length(angles)))
monkey.dprime <- array(NA, dim = c(2, length(unique(monkey_pf_data[,1])), length(angles)))
monkey.reaction_time <- array(NA, dim = c(2, 3, length(unique(monkey_pf_data[,1])), length(angles)))

calculate_dprime <- function(data) {

  trial_info <- rep(0, length(data))
  
  for (t in 1:dim(trial_responses)[1]) {
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


# Loop through eyes and attention conditions
for (eye in 1:2) {
  for (att_cond in 1:3) {
    indices <- monkey_pf_data[,2] != 0 &
      monkey_pf_data[,4] %in% c(2, 3, 4) &
      monkey_pf_data[,3] == eye &
      monkey_pf_data[,5] != att_indices[att_cond]
    data <- monkey_pf_data[indices,]
    for (sub_id in unique(monkey_pf_data[,1])) {
      this_sub <- data[data[,1] == sub_id,]
      for (loc in 1:length(angles)) {
        this_loc <- this_sub[this_sub[,2] == loc,]
        trial_responses <- cbind(this_loc[,6], this_loc[,7])
        monkey.dprime_attention_conds[eye, att_cond, sub_id, loc] <- calculate_dprime(trial_responses)
        if (att_cond == 1) {
          monkey.dprime[eye, sub_id, loc] <- calculate_dprime(trial_responses)
        }
        monkey.reaction_time[eye, att_cond, sub_id, loc] <- mean(this_loc[,8], na.rm = TRUE)
      }
    }
  }
}

# Define columns
rhm_column <- 1
uvm_column <- 3
lhm_column <- 5
lvm_column <- 7

# Compute mean dprime for both eyes and normalize
monkey.dprime_both_eyes <- apply(monkey.dprime, c(2, 3), mean)
monkey.dprime_normalized <- monkey.dprime_both_eyes / rowMeans(monkey.dprime_both_eyes)

# Compute monkey.hm and monkey.vm
monkey.hm <- rowMeans(cbind(monkey.dprime_normalized[,rhm_column], monkey.dprime_normalized[,lhm_column]))
monkey.vm <- rowMeans(cbind(monkey.dprime_normalized[,uvm_column], monkey.dprime_normalized[,lvm_column]))

# Compute monkey.hva and monkey.vma
monkey.hva <- (monkey.hm - monkey.vm) / (monkey.hm + monkey.vm)
monkey.uvm <- monkey.dprime_normalized[, uvm_column]
monkey.lvm <- monkey.dprime_normalized[, lvm_column]
monkey.vma <- (monkey.lvm - monkey.uvm) / (monkey.lvm + monkey.uvm)
monkey.asymmetries <- cbind(monkey.hva, monkey.vma)

# Normalize the data
monkey_full_vf_data <- apply(monkey.dprime_normalized, 2, mean)
monkey_fullvf_RT_data_tmp <- apply(monkey.reaction_time, c(2, 3, 4), mean)/1000
monkey_fullvf_RT_data <- apply(monkey_fullvf_RT_data_tmp[1,,], 2, mean)

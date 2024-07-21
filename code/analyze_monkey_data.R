analyze_monkey_data <- function(project_dir) {
  monkey_pf_data <- read.csv(file.path(project_dir, "Data", "organized_monkey_data.csv"))
  print('Calculating d-prime for macaque observers...')

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
  monkey <- list(
    dprime_attention_conds = array(NA, dim = c(2, 3, length(unique(monkey_pf_data[,1])), length(angles))),
    dprime = array(NA, dim = c(2, length(unique(monkey_pf_data[,1])), length(angles))),
    reaction_time = array(NA, dim = c(2, 3, length(unique(monkey_pf_data[,1])), length(angles)))
  )
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
          monkey$dprime_attention_conds[eye, att_cond, sub_id, loc] <- calculate_dprime(trial_responses, 'monkey')
          if (att_cond == 1) {
            monkey$dprime[eye, sub_id, loc] <- calculate_dprime(trial_responses, 'monkey')
          }
          monkey$reaction_time[eye, att_cond, sub_id, loc] <- mean(this_loc[,8], na.rm = TRUE)
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
  monkey$dprime_both_eyes <- apply(monkey$dprime, c(2, 3), mean)
  monkey$dprime_normalized <- monkey$dprime_both_eyes / rowMeans(monkey$dprime_both_eyes)
  
  # Compute monkey.hm and monkey.vm
  hm <- rowMeans(cbind(monkey$dprime_normalized[,rhm_column], monkey$dprime_normalized[,lhm_column]))
  vm <- rowMeans(cbind(monkey$dprime_normalized[,uvm_column], monkey$dprime_normalized[,lvm_column]))
  uvm <- monkey$dprime_normalized[, uvm_column]
  lvm <- monkey$dprime_normalized[, lvm_column]
  
  # Compute monkey.hva and monkey.vma
  monkey$hva <- (hm - vm) / (hm + vm)
  monkey$vma <- (lvm - uvm) / (lvm + uvm)
  monkey$asymmetries <- cbind(monkey$hva, monkey$vma)
  
  # Normalize the data
  monkey$full_visual_field_dprime <- apply(monkey$dprime_normalized, 2, mean)
  temp <- apply(monkey$reaction_time, c(2, 3, 4), mean)/1000
  monkey$full_visual_field_RT <- apply(temp[1,,], 2, mean)
  
  return(monkey)
}

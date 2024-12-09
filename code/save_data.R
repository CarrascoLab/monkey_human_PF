save_data <- function(human, monkey, human_mono, comp_cond){
  # long script to save all the data needed for statistical analysis:
  
  # Required libraries
  library(dplyr)
  
  if (comp_cond == 1) {
    add_label = 'monocularParticipants'
  } else if (comp_cond == 0) {
    add_label = 'allBinocularParticipants'
  }
  
  # Data preparation
  var_names <- c("id","group", "RHM", "IC_NE", "UVM", "IC_NW","LHM","IC_SW", "LVM", "IC_SE")
  
  # Combine data and calculate the mean for monkey.dprime
  monkey_dprime_mean <- apply(monkey$dprime, c(2, 3), mean)
  
  # Create the data frame
  data_table <- data.frame(matrix(ncol = length(var_names), nrow = dim(human$dprime)[1] +dim(monkey$dprime)[2]))
  colnames(data_table) <- var_names
  
  # Assign id column
  data_table$id <- 1:(dim(human$dprime)[1] +dim(monkey$dprime)[2])
  
  # Assign group column
  data_table$group[1:dim(human$dprime)[1]] <- 1
  data_table$group[(dim(human$dprime)[1] + 1):(dim(human$dprime)[1] + dim(monkey$dprime)[2])] <- 2
  
  # Assign the dprime columns
  data_table$RHM <- c(human$dprime[,1], monkey_dprime_mean[,1])
  data_table$IC_NE <- c(human$dprime[,2], monkey_dprime_mean[,2])
  data_table$UVM <- c(human$dprime[,3], monkey_dprime_mean[,3])
  data_table$IC_NW <- c(human$dprime[,4], monkey_dprime_mean[,4])
  data_table$LHM <- c(human$dprime[,5], monkey_dprime_mean[,5])
  data_table$IC_SW <- c(human$dprime[,6], monkey_dprime_mean[,6])
  data_table$LVM <- c(human$dprime[,7], monkey_dprime_mean[,7])
  data_table$IC_SE <- c(human$dprime[,8], monkey_dprime_mean[,8])
  
  # Write to CSV file
  write.csv(data_table, file.path(project_dir, 'data_to_analyze', sprintf('data_botheyes_all_locs_%s.csv', add_label)), row.names = FALSE, quote = TRUE)
  print('>>> Main data file is saved!')
  
  ######
  # Create the data frame
  var_names <- c("id","group", "RHM", "IC_NE", "UVM", "IC_NW","LHM","IC_SW", "LVM", "IC_SE")
  
  data_table <- data.frame(matrix(ncol = length(var_names), nrow = dim(human$dprime)[1] +dim(monkey$dprime)[2]))
  colnames(data_table) <- var_names
  
  # Assign id column
  data_table$id <- 1:(dim(human$dprime)[1] +dim(monkey$dprime)[2])
  
  # Assign group column
  data_table$group[1:dim(human$dprime)[1]] <- 1
  data_table$group[(dim(human$dprime)[1] + 1):(dim(human$dprime)[1] + dim(monkey$dprime)[2])] <- 2
  
  # Assign the dprime columns
  data_table$RHM <- c(human$dprime_normalized[,1], monkey$dprime_normalized[,1])
  data_table$UVM <- c(human$dprime_normalized[,3], monkey$dprime_normalized[,3])
  data_table$LHM <- c(human$dprime_normalized[,5], monkey$dprime_normalized[,5])
  data_table$LVM <- c(human$dprime_normalized[,7], monkey$dprime_normalized[,7])

  # Write to CSV file
  write.csv(data_table, file.path(project_dir, 'data_to_analyze', sprintf('normalized_data_botheyes_cardinals_%s.csv',add_label)), row.names = FALSE, quote = TRUE)
  print('>>> Normalized data file is saved!')
  
  ######
  # Create the data frame
  var_names <- c("id","group", "RHM", "UVM","LHM", "LVM")
  
  data_table <- data.frame(matrix(ncol = length(var_names), nrow = dim(human$dprime)[1] + dim(human_mono$dprime)[1] + dim(monkey$dprime)[2]))
  colnames(data_table) <- var_names
  
  # Assign id column
  data_table$id <- 1:(dim(human$dprime)[1] + dim(human_mono$dprime)[1] + dim(monkey$dprime)[2])
  
  # Assign group column
  data_table$group[1:dim(human$dprime)[1]] <- 1
  data_table$group[(dim(human$dprime)[1] + 1):(dim(human$dprime)[1] + dim(human_mono$dprime)[1])] <- 3
  data_table$group[(dim(human_mono$dprime)[1] + dim(human$dprime)[1] + 1):(dim(human_mono$dprime)[1] + dim(human$dprime)[1] + dim(monkey$dprime)[2])] <- 2
  
  # Assign the dprime columns
  data_table$RHM <- c(human$dprime_normalized[,1], human_mono$dprime_normalized[,1], monkey$dprime_normalized[,1])
  data_table$UVM <- c(human$dprime_normalized[,3], human_mono$dprime_normalized[,3], monkey$dprime_normalized[,3])
  data_table$LHM <- c(human$dprime_normalized[,5], human_mono$dprime_normalized[,5], monkey$dprime_normalized[,5])
  data_table$LVM <- c(human$dprime_normalized[,7], human_mono$dprime_normalized[,7], monkey$dprime_normalized[,7])
  
  # Write to CSV file
  write.csv(data_table, file.path(project_dir, 'data_to_analyze', sprintf('normalized_data_wbinocularhuman_cardinals_%s.csv', add_label)), row.names = FALSE, quote = TRUE)
  print('>>> Normalized data file for the monocular session is saved!')
  
  
  # Data preparation
  var_names <- c("id","group", "RHM", "UVM","LHM", "LVM")
  
  # Combine data and calculate the mean for monkey.dprime
  monkey_dprime_mean <- apply(monkey$dprime, c(2, 3), mean)
  
  data_table <- data.frame(matrix(ncol = length(var_names), nrow = dim(human$dprime)[1] + dim(human_mono$dprime)[1] + dim(monkey$dprime)[2]))
  colnames(data_table) <- var_names
  
  # Assign id column
  data_table$id <- 1:(dim(human$dprime)[1] + dim(human_mono$dprime)[1] + dim(monkey$dprime)[2])
  
  # Assign group column
  data_table$group[1:dim(human$dprime)[1]] <- 1
  data_table$group[(dim(human$dprime)[1] + 1):(dim(human$dprime)[1] + dim(human_mono$dprime)[1])] <- 3
  data_table$group[(dim(human_mono$dprime)[1] + dim(human$dprime)[1] + 1):(dim(human_mono$dprime)[1] + dim(human$dprime)[1] + dim(monkey$dprime)[2])] <- 2
  
  
  # Assign the dprime columns
  data_table$RHM <- c(human$dprime[,1], human_mono$dprime[,1], monkey_dprime_mean[,1])
  data_table$UVM <- c(human$dprime[,3], human_mono$dprime[,3], monkey_dprime_mean[,3])
  data_table$LHM <- c(human$dprime[,5], human_mono$dprime[,5], monkey_dprime_mean[,5])
  data_table$LVM <- c(human$dprime[,7], human_mono$dprime[,7],monkey_dprime_mean[,7])
  
  # Write to CSV file
  write.csv(data_table, file.path(project_dir, 'data_to_analyze', sprintf('data_wbinocularhuman_cardinals_%s.csv', add_label)), row.names = FALSE, quote = TRUE)
  print('>>> Non-normalized, cardinal data w binocular session are saved!')
  
  ########
  # Extract data for amblyopic and fellow eyes
  monkey_dprime_fellow_eye <- monkey$dprime[1,,]
  monkey_dprime_amb_eye <- monkey$dprime[2,,]
  
  # Define column names
  var_names <- c("id","eye_cond", "RHM", "IC_NE", "UVM", "IC_NW","LHM","IC_SW", "LVM", "IC_SE")
  
  # Create the data frame
  data_table <- data.frame(matrix(ncol = length(var_names), nrow = nrow(monkey_dprime_amb_eye) + nrow(monkey_dprime_fellow_eye)))
  colnames(data_table) <- var_names
  
  # Assign id column
  data_table$id <- c(1:nrow(monkey_dprime_amb_eye), 1:nrow(monkey_dprime_fellow_eye))
  
  # Assign eye_cond column
  data_table$eye_cond[1:nrow(monkey_dprime_amb_eye)] <- 1
  data_table$eye_cond[(nrow(monkey_dprime_amb_eye) + 1):(nrow(monkey_dprime_amb_eye) + nrow(monkey_dprime_fellow_eye))] <- 2
  
  # Assign the dprime columns
  data_table$RHM <- c(monkey_dprime_fellow_eye[,1], monkey_dprime_amb_eye[,1])
  data_table$IC_NE <- c(monkey_dprime_fellow_eye[,2], monkey_dprime_amb_eye[,2])
  data_table$UVM <- c(monkey_dprime_fellow_eye[,3], monkey_dprime_amb_eye[,3])
  data_table$IC_NW <- c(monkey_dprime_fellow_eye[,4], monkey_dprime_amb_eye[,4])
  data_table$LHM <- c(monkey_dprime_fellow_eye[,5], monkey_dprime_amb_eye[,5])
  data_table$IC_SW <- c(monkey_dprime_fellow_eye[,6], monkey_dprime_amb_eye[,6])
  data_table$LVM <- c(monkey_dprime_fellow_eye[,7], monkey_dprime_amb_eye[,7])
  data_table$IC_SE <- c(monkey_dprime_fellow_eye[,8], monkey_dprime_amb_eye[,8])
  
  # Write to CSV file
  write.csv(data_table, file.path(project_dir, 'data_to_analyze', 'monkey_data.csv'), row.names = FALSE, quote = TRUE)
  print('>>> Macaque data file for data from two eyes is saved!')
  
  # Normalize the data
  monkey_dprime_across_att <- apply(monkey$dprime_attention_conds, c(2, 3, 4), mean)
  monkey_dprime_neutral <- monkey_dprime_across_att[1,,]
  monkey_dprime_cued <- monkey_dprime_across_att[2,,]
  
  # Define column names
  var_names <- c("id","att_cond", "RHM", "IC_NE", "UVM", "IC_NW","LHM","IC_SW", "LVM", "IC_SE")
  
  # Create the data frame
  data_table <- data.frame(matrix(ncol = length(var_names), nrow = nrow(monkey_dprime_neutral) + nrow(monkey_dprime_cued)))
  colnames(data_table) <- var_names
  
  # Assign id column
  data_table$id <- c(1:nrow(monkey_dprime_neutral), 1:nrow(monkey_dprime_cued))
  
  # Assign att_cond column
  data_table$att_cond[1:nrow(monkey_dprime_neutral)] <- 1
  data_table$att_cond[(nrow(monkey_dprime_neutral) + 1):(nrow(monkey_dprime_neutral) + nrow(monkey_dprime_cued))] <- 2
  
  # Assign the dprime columns
  data_table$RHM <- c(monkey_dprime_neutral[,1], monkey_dprime_cued[,1])
  data_table$IC_NE <- c(monkey_dprime_neutral[,2], monkey_dprime_cued[,2])
  data_table$UVM <- c(monkey_dprime_neutral[,3], monkey_dprime_cued[,3])
  data_table$IC_NW <- c(monkey_dprime_neutral[,4], monkey_dprime_cued[,4])
  data_table$LHM <- c(monkey_dprime_neutral[,5], monkey_dprime_cued[,5])
  data_table$IC_SW <- c(monkey_dprime_neutral[,6], monkey_dprime_cued[,6])
  data_table$LVM <- c(monkey_dprime_neutral[,7], monkey_dprime_cued[,7])
  data_table$IC_SE <- c(monkey_dprime_neutral[,8], monkey_dprime_cued[,8])
  
  # Write to CSV file
  write.csv(data_table, file.path(project_dir, 'data_to_analyze', 'monkey_attention_data.csv'), row.names = FALSE, quote = TRUE)
  print('>>> Macaque data file for data from two attention conditions is saved!')
  
  # Define column names
  var_names <- c("id", "RHM", "IC_NE", "UVM", "IC_NW","LHM","IC_SW", "LVM", "IC_SE")
  
  # Create the data frame
  data_table <- data.frame(matrix(ncol = length(var_names), nrow = nrow(human$dprime)))
  colnames(data_table) <- var_names
  
  # Assign id column
  data_table$id <- 1:nrow(human$dprime)
  
  # Assign the dprime columns
  data_table$RHM <- human$dprime[, 1]
  data_table$IC_NE <- human$dprime[, 2]
  data_table$UVM <- human$dprime[, 3]
  data_table$IC_NW <- human$dprime[, 4]
  data_table$LHM <- human$dprime[, 5]
  data_table$IC_SW <- human$dprime[, 6]
  data_table$LVM <- human$dprime[, 7]
  data_table$IC_SE <- human$dprime[, 8]
  
  # Write to CSV file
  write.csv(data_table, file.path(project_dir, 'data_to_analyze', 'human_data'), row.names = FALSE, quote = TRUE)
  print('>>> Human data file is saved!')
  
  
}
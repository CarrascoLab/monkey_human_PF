calculate_dprime <- function(data, group) {

  if (group == 'human') {
    trial_info <- rep(0, dim(data)[1])
  } else if (group == 'monkey') {
    trial_info <- rep(0, length(data))
  }
  
  for (t in 1:dim(data)[1]) {
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

# Example usage:
# Assuming 'data' is a matrix with appropriate dimensions
# Replace 'data' with your actual data
# dprime_value <- calculate_dprime(data)

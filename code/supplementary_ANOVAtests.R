# Load libraries
library(dplyr)
library(car)
library(ggpubr)
library(cowplot)
library(rstatix)
library(effectsize)

directory = "/Volumes/purplab/EXPERIMENTS/1_Current_Experiments/Ekin/monkeyPF"
setwd(directory)

# comparing Asymmetry ratio or dprime data:
file_name = "data"
observer_group <- "monkey"

if (file_name == "data") {
  data_tbl <- read.csv((sprintf("%s/data_to_analyze/%s_%s.csv", directory, observer_group, file_name)), header = T)
  
  if (observer_group == "monkey") {
    att_data <- read.csv((sprintf("%s/data_to_analyze/monkey_attention_data.csv", directory, observer_group, file_name, location)), header = T)
  }
  
} else {
  observer_group <- NA
  location <- NA
  HVA <- read.csv((sprintf("%s/data_to_analyze/HVA.csv", directory)), header = T)
  VMA <- read.csv((sprintf("%s/data_to_analyze/VMA.csv", directory)), header = T)
}

if ((file_name == "data" || file_name == "normalized_data") & location == "cardinals") {
  data_tbl <- data_tbl %>%
    gather(key = "location", value = "sensitivity", UVM, LVM, LHM, RHM)
} else if(file_name == "data" || file_name == "normalized_data" & location == "all_locs") {
  data_tbl <- data_tbl %>%
    gather(key = "location", value = "sensitivity", RHM,IC_NE,UVM,IC_NW,LHM,IC_SW, LVM,IC_SE) %>%
    convert_as_factor(location)
  if (observer_group == "monkey") {
    att_data <- att_data %>%
    gather(key = "location", value = "sensitivity", RHM,IC_NE,UVM,IC_NW,LHM,IC_SW, LVM,IC_SE)
  }
} else if (file_name == "asymmetry_ratio") {
  HVA <- HVA %>% gather(key = "location", value = "sensitivity", HM, VM)
  HVA <- HVA %>% convert_as_factor(location)
  VMA <- VMA %>% gather(key = "location", value = "sensitivity", LVM, UVM)
  VMA <- VMA %>% convert_as_factor(location)
}

if (observer_group == "monkey") {
  # Make matrix variable ordered factor
  data_tbl$eye_cond <- factor(
    data_tbl$eye_cond,
    levels = c('1', '2')
  )
  
  att_data$att_cond <- factor(
    att_data$att_cond,
    levels = c('1', '2')
  )
  
  data_tbl %>%
    group_by(eye_cond, location) %>%
    get_summary_stats(sensitivity, type = 'mean_sd')
  
  with(data_tbl, tapply(sensitivity, list(eye_cond, location), mean))
  
  res.aov <- anova_test(
    data = data_tbl, dv = sensitivity, wid = id,
    within = c(eye_cond, location),
    effect.size = 'pes'
  )
  get_anova_table(res.aov)
  # average across eyes, since only the main effect of location is significant:
  #detach(package:plyr)
  
  new_data <- data_tbl %>%
    dplyr::group_by(location, id)%>%
    dplyr::summarise(avg_sen=mean(sensitivity))
  new_data <- new_data %>%
    convert_as_factor(location)
  new_data <- as.data.frame(new_data)
  
  one.way2 <- new_data %>% anova_test(avg_sen ~ location + Error(id/(location)))
  get_anova_table(one.way2)
  pairwise_data <- new_data
  pairwise_data$sensitivity <- pairwise_data$avg_sen
  
  # analyze the attention data:
  att_res.aov <- anova_test(
    data = att_data, dv = sensitivity, wid = id,
    within = c(att_cond, location),
    effect.size = 'pes'
  )
  print(att_res.aov)
  
  new_data2 <- att_data %>%
    group_by(location, id)%>%
    dplyr::summarise(avg_sen=mean(sensitivity))
  new_data2 <- new_data2 %>%
    convert_as_factor(location)
  new_data2 <- as.data.frame(new_data2)
  
  one.way2 <- new_data2 %>% anova_test(avg_sen ~ location + Error(id/(location)))
  get_anova_table(one.way2)
  pairwise_data <- new_data2
  pairwise_data$sensitivity <- pairwise_data$avg_sen
  
} else if (observer_group == 'human') {
    res.aov <- anova_test(
      data = data_tbl, dv = sensitivity, wid = id,
      within = c(location),
      effect.size = 'pes'
    )
    
    get_anova_table(res.aov)
    pairwise_data <- data_tbl
    
} else if (file_name == 'asymmetry ratio') {

    res1.aov <- anova_test(
      data = HVA, dv = sensitivity, wid = id,
      within = c(location), between = group,
      effect.size = 'pes'
    )
    
    res2.aov <- anova_test(
      data = VMA, dv = sensitivity, wid = id,
      within = c(location), between = group,
      effect.size = 'pes'
    )
    get_anova_table(res1.aov)
    get_anova_table(res2.aov)
    
}


if (is.na(observer_group) == FALSE) {
  
  upper <-pairwise_data[pairwise_data$location == "UVM", 'sensitivity']
  lower <-pairwise_data[pairwise_data$location == "LVM", 'sensitivity']
  
  up_vs_low <- t.test(upper, lower, paired = TRUE, alternative = "two.sided")
  
  horizontal = apply(cbind(pairwise_data[pairwise_data$location == "LHM", 'sensitivity'], pairwise_data[pairwise_data$location == "RHM" , 'sensitivity']), 1, mean)
  vertical = apply(cbind(pairwise_data[pairwise_data$location == "LVM", 'sensitivity'], pairwise_data[pairwise_data$location == "UVM" , 'sensitivity']), 1, mean)
  hori_vs_vert <- t.test(horizontal, vertical, paired = TRUE, alternative = "two.sided")
  
  # get the descriptives of contrasts:
  IC_mean = mean(pairwise_data[pairwise_data$location == "IC_NE" | pairwise_data$location == "IC_NW" | pairwise_data$location == "IC_SE" | pairwise_data$location == "IC_SE", 'sensitivity'])
  Card_mean = mean(pairwise_data[pairwise_data$location == "RHM" | pairwise_data$location == "LHM" | pairwise_data$location == "RHM" | pairwise_data$location == "LHM", 'sensitivity'])
  horizontal_mean = mean(pairwise_data[pairwise_data$location == "RHM" | pairwise_data$location == "LHM", 'sensitivity'])
  vertical_mean = mean(pairwise_data[pairwise_data$location == "LVM" | pairwise_data$location == "UVM", 'sensitivity'])
  lvm_mean = mean(pairwise_data[pairwise_data$location == "LVM", 'sensitivity'])
  uvm_mean = mean(pairwise_data[pairwise_data$location == "UVM", 'sensitivity'])
  
}

#calculate effect size with cohen's Dz (paired t-test) based on Rosenthal (1991):
cohendz <- list()
tval = up_vs_low$statistic['t']
cohendz$vertical = abs(tval/sqrt(max(unique(data_tbl$id))))

tval = hori_vs_vert$statistic['t']
cohendz$horizontal = abs(tval/sqrt(max(unique(data_tbl$id))))


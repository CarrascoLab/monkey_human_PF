# Author: Ekin Tuncok (2023-2024)
###### MAIN SCRIPT for 1) cleaning, 2) analyzing, 3) visualizing, 4) running statistics on the data
library(plotrix)  # Load the plotrix library for polar plots
library(dplyr)
library(plyr)  # For data manipulation
library(export)
library(boot)
library(reshape2)
library(ggplot2)
library(cowplot)

# Set project directory and load data
project_dir <- "/Volumes/purplab/EXPERIMENTS/1_Current_Experiments/Ekin/monkeyPF"
setwd(project_dir)

opt_run_analysis = 1
opt_save_data = 0
opt_visualize_data = 1
opt_run_stats = 0

if (opt_visualize_data == 1) {
  run_analysis = 1
}

if (opt_run_analysis == 1) {
  # Script to analyze the cleaned data:
  source(file.path(project_dir, 'code', 'analyze_human_data.R'))
  source(file.path(project_dir, 'code', 'analyze_monkey_data.R'))
  source(file.path(project_dir, 'code', 'save_data.R'))
  
  setwd(project_dir)
  
  human <- analyze_human_data(project_dir)
  
  monkey <- analyze_monkey_data(project_dir)
  
  if (opt_save_data == 1) {
    save_data(human, monkey)
    print('All data saved!')
  }
}

if (opt_visualize_data == 1) {
  source(file.path(project_dir, 'code', 'fig_2_A.R'))
  source(file.path(project_dir, 'code', 'fig_3_A.R'))
  source(file.path(project_dir, 'code', 'fig_3_B.R'))
  
  fig_2_A(human, monkey)
  fig_3_A(monkey)
  fig_3_B(monkey)
}

# Author: Ekin Tuncok (2023-2024)
###### MAIN SCRIPT for 1) cleaning, 2) analyzing, 3) visualizing, 4) running statistics on the data
library(plotrix)  # Load the plotrix library for polar plots
library(dplyr)
library(plyr)  # For data manipulation
library(tidyr)
library(export)
library(boot)
library(superb)
library(rstatix)
library(reshape2)
library(ggplot2)
library(cowplot)

# Set project directory and loßad data
project_dir <- "/Volumes/purplab/EXPERIMENTS/1_Current_Experiments/Ekin/monkeyPF"
setwd(project_dir)

opt_run_analysis = 1
opt_save_data = 0
opt_visualize_data = 1
opt_run_stats = 0

if (opt_visualize_data == 1) {
  run_analysis = 1
}
comp_cond = 0 # set this to 1 for looking at the comparison of 
# human subjects who participated in BOTH the monocular and binocular viewing conditions. 
# a value of 0 will plot all the data from the binocular condition, including those who were
# NOT tested under monocular viewing. 
if (opt_run_analysis == 1) {
  # Script to analyze the cleaned data:
  source(file.path(project_dir, 'code', 'analyze_human_data.R'))
  source(file.path(project_dir, 'code', 'analyze_monkey_data.R'))
  source(file.path(project_dir, 'code', 'save_data.R'))
  
  setwd(project_dir)
  human_mono <- analyze_human_data(project_dir, 2, comp_cond)
  human <- analyze_human_data(project_dir, 1, comp_cond)
  monkey <- analyze_monkey_data(project_dir)
  
  # monkey data in individual order: 
  #natural amblyope, healthy control, anisometric amblyopia, strabismus, strabismus, anisometric amblyopia
  
  if (opt_save_data == 1) {
    save_data(human, monkey, human_mono, comp_cond)
    print('All data saved!')
  }
}

if (opt_visualize_data == 1) {
  print('Visualizing the data...')
  source(file.path(project_dir, 'code', 'fig_2_A.R'))
  source(file.path(project_dir, 'code', 'fig_2_B.R'))
  source(file.path(project_dir, 'code', 'fig_2_C.R'))
  source(file.path(project_dir, 'code', 'fig_2_D.R'))
  source(file.path(project_dir, 'code', 'fig_3.R'))
  source(file.path(project_dir, 'code', 'fig_4.R'))
  
  fig_2_A(human, monkey)
  fig_2_B(project_dir)
  fig_2_C(human, human_mono, monkey)
  fig_2_D(human, human_mono, monkey)
  fig_3(monkey)
  fig_4(monkey)
}

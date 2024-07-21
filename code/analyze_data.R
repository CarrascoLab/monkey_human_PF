# Set project directory and load data
project_dir <- "/Volumes/purplab/EXPERIMENTS/1_Current_Experiments/Ekin/monkeyPF"

# Script to analyze the cleaned data:
source(file.path(project_dir, 'code', 'analyze_human_data.R'))
source(file.path(project_dir, 'code', 'analyze_monkey_data.R'))

setwd(project_dir)

human <- analyze_human_data(project_dir)

monkey <- analyze_monkey_data(project_dir)
% 2023 August: Ekin Tuncok

% For TUNCOK, KIORPES, CARRASCO (2024) 

% The initial script to run on the RAW data files for both humans and
% monkeys. Inside each function there are group-specific cleaning, labeling
% and organization stages. 

% We share the output of these scripts in 'data_to_analyze' folder so the
% reader does not have to run this script to reproduce paper figures.  

home_dir = getenv('HOME');
if strcmp(home_dir, '/Users/ekintuncok')
    project_dir = '/Volumes/purplab/EXPERIMENTS/1_Current_Experiments/Ekin/monkeyPF/';
else
    project_dir = input('project directory:');
end

code_dir = fullfile(project_dir, 'code');

addpath(genpath(project_dir));

organize_human_data(project_dir);

organize_monkey_data(project_dir);
project_dir = '/Volumes/purplab/EXPERIMENTS/1_Current_Experiments/Ekin/monkeyPF/';
subj = {'SJ1', 'SJ2', 'SJ3', 'SJ4', 'SJ6', 'SJ7','SJ8','SJ9', 'SJ10', 'SJ11', ...
    'SJ12', 'SJ13', 'SJ14', 'SJ15',  'SJ16', 'SJ17',  'SJ18', 'SJ19', 'SJ20', 'SJ21',...
    'SJ22', 'SJ23', 'SJ24', 'SJ25'}; % subject 5 is not included in the analysis, so the abb is deleted
data_dir = fullfile(project_dir, 'Data/');
age = ones(1, length(subj));
for sb = 1:length(subj)
    load([data_dir, sprintf('SJ%i/Block1/SJ%i_monkeyPF_const_file.mat', sb, sb)])
    age(sb) = const.sjct_age;
end

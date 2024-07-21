function organize_human_data(project_dir)
% 2023 August: Ekin Tuncok
% Extract the human behavior data:
% Takes in a project directory in which the data resides, creates clean 
% data files and saves it for further use. 

data_dir = fullfile(project_dir, 'Data/');
subj = {'SJ1', 'SJ2', 'SJ3', 'SJ4','SJ5', 'SJ6', 'SJ7','SJ8','SJ9', 'SJ10', 'SJ11', ...
    'SJ12', 'SJ13', 'SJ14', 'SJ15',  'SJ16', 'SJ17',  'SJ18', 'SJ19', 'SJ20', 'SJ21',...
    'SJ22', 'SJ23', 'SJ24', 'SJ25'};

location_column = 3; % from the output file:
for sb = 1:length(subj)

    if exist(fullfile(project_dir, 'data_to_analyze', sprintf('subj%i_cleaned_output.csv', sb)), 'file')
        fprintf('%s, cleaned output file exists\n', subj{sb});
    else

        fprintf('%s\n', subj{sb});
        curr_dir = [data_dir, subj{sb}];
        load([curr_dir, sprintf('/Block1/%s_monkeyPF_Output.mat', subj{sb})])

        % extract all the non-nan values:
        Output(isnan(Output(:,12)),:) = [];

        % assign the location. To keep the human experiment as similar to
        % the monkey experiment, we created wedges around each angular
        % target position, and sampled discriminability for the edges and
        % the central value of the wedge. Here I assign all three locations
        % into the corresponding angular wedge:

        Output(Output(:,location_column) == 1 |  Output(:,location_column) == 9  |  Output(:,location_column) == 17, location_column) = 0;
        Output(Output(:,location_column) == 2 |  Output(:,location_column) == 10 | Output(:,location_column) == 18, location_column) = 45;
        Output(Output(:,location_column) == 3 |  Output(:,location_column) == 11 | Output(:,location_column) == 19, location_column) = 90;
        Output(Output(:,location_column) == 4 |  Output(:,location_column) == 12 |  Output(:,location_column) == 20, location_column) = 135;
        Output(Output(:,location_column) == 5 |  Output(:,location_column) == 13 |  Output(:,location_column) == 21, location_column) = 180;
        Output(Output(:,location_column) == 6 |  Output(:,location_column) == 14 |  Output(:,location_column) == 22, location_column) = 225;
        Output(Output(:,location_column) == 7 |  Output(:,location_column) == 15 |  Output(:,location_column) == 23, location_column) = 270;
        Output(Output(:,location_column) == 8 |  Output(:,location_column) == 16 |  Output(:,location_column) == 24, location_column) = 315;


        output = [Output(:, location_column), Output(:, 9),  Output(:, 10),  Output(:, 12)];
        save(fullfile(project_dir, 'data_to_analyze', sprintf('subj%i_cleaned_output.mat', sb)), 'output')
        csvwrite(sprintf(fullfile(project_dir, 'data_to_analyze', sprintf('subj%i_cleaned_output.csv', sb)), sb), output)

    end
end
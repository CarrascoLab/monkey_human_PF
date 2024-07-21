function organize_monkey_data(project_dir)

% 2023 August: Ekin Tuncok
% Extracts the monkey behavior data from Pham, Carrasco & Kiorpes, 2018
% Takes in a project directory in which the data resides, creates clean 
% data files and saves it for further use. 

file_names      = dir(fullfile(project_dir, 'kiorpes_data', '*txt'));
monkey_abb = {'B1','KG', 'MX','PD','PE', 'TA'};
% natural amblyope, healthy, abisos, strab, strab, abisos

% define the angle of interest
monkey_pf_data = [];
uvm_angl = 90;
lvm_angl = 270;
lhm_angl = 180;
rhm_angl1 = 0; rhm_angl2 = 360;

ic1_angl = 45;
ic2_angl = 135;
ic3_angl = 225;
ic4_angl = 315;
angle_offset = 10;

for f = 1:length(file_names)

    curr_file = file_names(f).name;
    % get the monkey's subject initials, and use this info to code the data
    % file later:
    subj_code = curr_file(1:2);
    eye_code = curr_file(4:5);
    if strcmp(subj_code, 'PD') && strcmp(eye_code, 'RE')
        contrast = curr_file(13);contrast = str2double(contrast);
        att_code = curr_file(8);
    else
        contrast = curr_file(12);contrast = str2double(contrast);
        att_code = curr_file(7);

    end
    fprintf('current subject: %s\n', subj_code)

    if ~strcmp(subj_code, 'PF') % PF data is excluded from the main analysis, skip this subject
        monkey_code =  strcmp(subj_code, monkey_abb);
        [~, id] = find(monkey_code > 0);

        current_data = readtable([project_dir, 'kiorpes_data/', sprintf('%s', curr_file)]);
        current_data_mat = table2array(current_data);


        [theta, rho] = cart2pol(current_data_mat(:,1), current_data_mat(:,2));
        pos_in_deg = rad2deg(theta);
        idx = pos_in_deg < 0;
        pos_in_deg(idx) = pos_in_deg(idx) + 360; % convert negatives to positives

        current_data_mat(:,1) = rho;
        current_data_mat(:,2) = pos_in_deg;

        uvm = current_data_mat(:,2) > uvm_angl - angle_offset & current_data_mat(:,2) < uvm_angl + angle_offset;
        lvm = current_data_mat(:,2) > lvm_angl - angle_offset & current_data_mat(:,2) < lvm_angl + angle_offset;
        lhm = current_data_mat(:,2) > lhm_angl - angle_offset & current_data_mat(:,2) < lhm_angl + angle_offset;
        rhm = current_data_mat(:,2) > rhm_angl1 & current_data_mat(:,2) < rhm_angl1+angle_offset |...
            current_data_mat(:,2) > rhm_angl2-angle_offset & current_data_mat(:,2) < rhm_angl2;
        
        ic1 = current_data_mat(:,2) > ic1_angl - angle_offset & current_data_mat(:,2) < ic1_angl + angle_offset;
        ic2 = current_data_mat(:,2) > ic2_angl - angle_offset & current_data_mat(:,2) < ic2_angl + angle_offset;
        ic3 = current_data_mat(:,2) > ic3_angl - angle_offset & current_data_mat(:,2) < ic3_angl + angle_offset;
        ic4 = current_data_mat(:,2) > ic4_angl - angle_offset & current_data_mat(:,2) < ic4_angl + angle_offset;

        reorg_data = zeros(length(current_data_mat), 8);
        reorg_data(:,1) =ones*id;

        % assign location:
        reorg_data(rhm,2) = 1;
        reorg_data(ic1,2) = 2;
        reorg_data(uvm,2) = 3;
        reorg_data(ic2,2) = 4;
        reorg_data(lhm,2) = 5;
        reorg_data(ic3,2) = 6;
        reorg_data(lvm,2) = 7;
        reorg_data(ic4,2) = 8;

        % assign eye:
        if strcmp(eye_code, 'LE')
            reorg_data(:, 3) = 1;
        else
            reorg_data(:, 3) = 2;
        end

        % assign contrast:
        reorg_data(:, 4) = contrast;

        % assign cue validity:
        if strcmp(att_code, 'c')
            reorg_data(:, 5) = 1;
        else
            reorg_data(:, 5) = 2;
        end

        % % assign trial accuracy:
        reorg_data(:, 6) = current_data_mat(:,3);
        reorg_data(:, 7) = current_data_mat(:,4);
        reorg_data(:, 8) = current_data_mat(:,5);
    end
    monkey_pf_data = cat(1, monkey_pf_data, reorg_data);
end

save(fullfile(project_dir, 'Data', 'organized_monkey_data.mat'), 'monkey_pf_data');


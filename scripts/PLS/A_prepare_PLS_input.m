%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prepare data for PLS
% for the LC-arousal project
% written by shelby bachman, sbachman@usc.edu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% setup

clear all
close all
clc


%% set directories

dir.root        = uigetdir('~', 'Select location of the parent directory LC-arousal: ');
dir.data        = [dir.root, filesep, 'data', filesep, 'derivatives'];


%% load PLS data -- all phases (single-group version)

% load data as table
data_all = readtable([dir.data, filesep, 'LC-arousal_pls-all_peak.csv'], ...
    'TreatAsMissing', 'NaN', 'NumHeaderLines', 1, 'VariableNamesLine', 1);

% set row names as subject IDs, for indexing purposes
data_all.Properties.RowNames = data_all.label_subject;

% split data into X and Y
%cols_all = [3:26]; % columns to extract physio data
cols_all = [3:23]; % columns to extract physio data (w/o resp)
for ii = 1:length(data_all.label_subject)
    % subset data for this ID
    tmp = data_all(data_all.label_subject{ii},:);
    % extract ID into input structure
    pls_input_all.id{ii,1} = data_all.label_subject{ii};
    % extract LC ratios into input structure
    pls_input_all.LC_ratio_meta_peak(ii,1) = tmp.LC_ratio_meta_peak;
    pls_input_all.LC_ratio_meta_rostral(ii,1) = tmp.LC_ratio_meta_rostral;
    pls_input_all.LC_ratio_meta_caudal(ii,1) = tmp.LC_ratio_meta_caudal;
    % extract physio data into input structure
    pls_input_all.X(ii,:) = table2array(tmp(:,cols_all));
end


%% load PLS data -- all phases (multi-group version)

% load data to table
data_all = readtable([dir.data, filesep, 'LC-arousal_pls-all_peak.csv'], ...
    'TreatAsMissing', 'NaN', 'NumHeaderLines', 1, 'VariableNamesLine', 1);

% set row names as subject IDs, for indexing purposes
data_all.Properties.RowNames = data_all.label_subject;

% subset data for each age group
data_all_YA = data_all(strcmp(data_all.age_group,'YA'),:);
data_all_OA = data_all(strcmp(data_all.age_group,'OA'),:);

% split data into X and Y
%cols_all = [3:26]; % columns to extract physio data
cols_all = [3:23]; % columns to extract physio data (w/o resp)
for ii = 1:length(data_all_YA.label_subject)
    % subset data for this ID
    tmp = data_all_YA(data_all_YA.label_subject{ii},:);
    % extract ID into input structure
    pls_input_all_YA.id{ii,1} = data_all_YA.label_subject{ii};
    % extract LC ratios into input structure
    pls_input_all_YA.LC_ratio_meta_peak(ii,1) = tmp.LC_ratio_meta_peak;
    pls_input_all_YA.LC_ratio_meta_rostral(ii,1) = tmp.LC_ratio_meta_rostral;
    pls_input_all_YA.LC_ratio_meta_caudal(ii,1) = tmp.LC_ratio_meta_caudal;
    % extract physio data into input structure
    pls_input_all_YA.X(ii,:) = table2array(tmp(:,cols_all));
end

for ii = 1:length(data_all_OA.label_subject)
    % subset data for this ID
    tmp = data_all_OA(data_all_OA.label_subject{ii},:);
    % extract ID into input structure
    pls_input_all_OA.id{ii,1} = data_all_OA.label_subject{ii};
    % extract LC ratios into input structure
    pls_input_all_OA.LC_ratio_meta_peak(ii,1) = tmp.LC_ratio_meta_peak;
    pls_input_all_OA.LC_ratio_meta_rostral(ii,1) = tmp.LC_ratio_meta_rostral;
    pls_input_all_OA.LC_ratio_meta_caudal(ii,1) = tmp.LC_ratio_meta_caudal;
    % extract physio data into input structure
    pls_input_all_OA.X(ii,:) = table2array(tmp(:,cols_all));
end

clear data_all data_all_YA data_all_OA cols_all tmp ii


%% save data

save([dir.data, filesep, 'PLS_all_input_peak.mat'], 'pls_input_all', 'pls_input_all_YA', 'pls_input_all_OA')
disp('all done preparing PLS input data!')

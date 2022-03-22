%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run PLS analyses
% for the LC-arousal project
% written by shelby bachman, sbachman@usc.edu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% setup
clear all
close all
clc

rng(42) % set random number generator


%% set directories

dir.root        = uigetdir('~', 'Select location of the parent directory LC-arousal: ');
dir.data          = [dir.root, filesep, 'data', filesep, 'derivatives'];
dir.toolbox       = [dir.root, filesep, 'toolboxes'];

addpath(genpath(dir.toolbox))


%% settings for PLS analysis

% for a detailed list of settings, see help pls_analysis
pls_opt = [];
pls_opt.method    = 3; % regular behavioral PLS correlation 
pls_opt.num_perm  = 1000; % number of permutations
pls_opt.num_boot  = 1000; % number of bootstraps


%% run PLS -- all phases (single-group version, YA only)

disp('%%%%%% RUNNING ALL-PHASES PLS (YA) %%%%%%')

%%%%%% run analysis for peak LC (hemi-averaged)
disp('running PLS for peak LC (hemi-averaged)')

% load input data
load([dir.data, filesep, 'PLS_all_input_peak.mat'])

% copy the behavioral data
pls_opt.stacked_behavdata = pls_input_all_YA.LC_ratio_meta_peak;

% set number of subjects and conditions
pls_opt.nr_subj = size(pls_input_all_YA.X, 1);
pls_opt.nr_cond = 1;

% run PLS
pls_YA_result_all_peak = pls_analysis({pls_input_all_YA.X},pls_opt.nr_subj,pls_opt.nr_cond,pls_opt);


%%%%%% run analysis for rostral LC
disp('running PLS for rostral LC')

% copy the behavioral data
pls_opt.stacked_behavdata = pls_input_all_YA.LC_ratio_meta_rostral;

% set number of subjects and conditions
pls_opt.nr_subj = size(pls_input_all_YA.X, 1);
pls_opt.nr_cond = 1;

% run PLS
pls_YA_result_all_rostral = pls_analysis({pls_input_all_YA.X},pls_opt.nr_subj,pls_opt.nr_cond,pls_opt);


%%%%%% run analysis for caudal LC
disp('running PLS for caudal LC')

% copy the behavioral data
pls_opt.stacked_behavdata = pls_input_all_YA.LC_ratio_meta_caudal;

% set number of subjects and conditions
pls_opt.nr_subj = size(pls_input_all_YA.X, 1);
pls_opt.nr_cond = 1;

% run PLS
pls_YA_result_all_caudal = pls_analysis({pls_input_all_YA.X},pls_opt.nr_subj,pls_opt.nr_cond,pls_opt);


%% run PLS -- all phases (single-group version, OA only)

disp('%%%%%% RUNNING ALL-PHASES PLS (OA) %%%%%%')

%%%%%% run analysis for peak LC (hemi-averaged)
disp('running PLS for peak LC (hemi-averaged)')

% load input data
load([dir.data, filesep, 'PLS_all_input_peak.mat'])

% copy the behavioral data
pls_opt.stacked_behavdata = pls_input_all_OA.LC_ratio_meta_peak;

% set number of subjects and conditions
pls_opt.nr_subj = size(pls_input_all_OA.X, 1);
pls_opt.nr_cond = 1;

% run PLS
pls_OA_result_all_peak = pls_analysis({pls_input_all_OA.X},pls_opt.nr_subj,pls_opt.nr_cond,pls_opt);


%%%%%% run analysis for rostral LC
disp('running PLS for rostral LC')

% copy the behavioral data
pls_opt.stacked_behavdata = pls_input_all_OA.LC_ratio_meta_rostral;

% set number of subjects and conditions
pls_opt.nr_subj = size(pls_input_all_OA.X, 1);
pls_opt.nr_cond = 1;

% run PLS
pls_OA_result_all_rostral = pls_analysis({pls_input_all_OA.X},pls_opt.nr_subj,pls_opt.nr_cond,pls_opt);


%%%%%% run analysis for caudal LC
disp('running PLS for caudal LC')

% copy the behavioral data
pls_opt.stacked_behavdata = pls_input_all_OA.LC_ratio_meta_caudal;

% set number of subjects and conditions
pls_opt.nr_subj = size(pls_input_all_OA.X, 1);
pls_opt.nr_cond = 1;

% run PLS
pls_OA_result_all_caudal = pls_analysis({pls_input_all_OA.X},pls_opt.nr_subj,pls_opt.nr_cond,pls_opt);

%% run PLS -- all phases (multi-group version)

disp('%%%%%% RUNNING ALL-PHASES PLS (MULTI-GROUP) %%%%%%')

%%%%%% run analysis for peak LC (hemi-averaged)
disp('running PLS for peak LC (hemi-averaged)')

% load input data
load([dir.data, filesep, 'PLS_all_input_peak.mat'])

% load the structures for each group
grp1 = pls_input_all_YA;
grp2 = pls_input_all_OA;
clear pls_input_all_YA pls_input_all_OA;

% copy the behavioral data by stacking the two groups
pls_opt.stacked_behavdata = cat(1,grp1.LC_ratio_meta_peak,grp2.LC_ratio_meta_peak);

% set number of subjects and conditions
pls_opt.nr_subj = [size(grp1.X,1),size(grp2.X,1)];
pls_opt.nr_cond = 1;

% run PLS
pls_result_all_peak = pls_analysis({grp1.X, grp2.X},pls_opt.nr_subj,pls_opt.nr_cond,pls_opt);


%%%%%% run analysis for rostral LC
disp('running PLS for rostral LC')

% copy the behavioral data by stacking the two groups
pls_opt.stacked_behavdata = cat(1,grp1.LC_ratio_meta_rostral,grp2.LC_ratio_meta_rostral);

% set number of subjects and conditions
pls_opt.nr_subj = [size(grp1.X,1),size(grp2.X,1)];
pls_opt.nr_cond = 1;

% run PLS
pls_result_all_rostral = pls_analysis({grp1.X, grp2.X},pls_opt.nr_subj,pls_opt.nr_cond,pls_opt);


%%%%%% run analysis for caudal LC
disp('running PLS for caudal LC')

% copy the behavioral data by stacking the two groups
pls_opt.stacked_behavdata = cat(1,grp1.LC_ratio_meta_caudal,grp2.LC_ratio_meta_caudal);

% set number of subjects and conditions
pls_opt.nr_subj = [size(grp1.X,1),size(grp2.X,1)];
pls_opt.nr_cond = 1;

% run PLS
pls_result_all_caudal = pls_analysis({grp1.X, grp2.X},pls_opt.nr_subj,pls_opt.nr_cond,pls_opt);

%%%%%% clear variables
clear grp1 grp2


%% inspect PLS results -- all phases (YA)

disp('%%%%%% ALL-PHASES RESULTS (YA) %%%%%%')

% are there reliable LVs?
disp('%%% LV p-values, peak LC:')
pls_YA_result_all_peak.perm_result.sprob

disp('%%% LV p-values, rostral LC:')
pls_YA_result_all_rostral.perm_result.sprob

disp('%%% LV p-values, caudal LC:')
pls_YA_result_all_caudal.perm_result.sprob


%% inspect PLS results -- all phases (OA)

disp('%%%%%% ALL-PHASES RESULTS (OA) %%%%%%')

% are there reliable LVs?
disp('%%% LV p-values, peak LC:')
pls_OA_result_all_peak.perm_result.sprob

disp('%%% LV p-values, rostral LC:')
pls_OA_result_all_rostral.perm_result.sprob

disp('%%% LV p-values, caudal LC:')
pls_OA_result_all_caudal.perm_result.sprob


%% inspect PLS results -- all phases (multi-group version)

disp('%%%%%% ALL-PHASES RESULTS (MULTI-GROUP) %%%%%%')

% are there reliable LVs?
disp('%%% LV p-values, peak LC:')
disp(pls_result_all_peak.perm_result.sprob)

disp('%%% LV p-values, rostral LC:')
pls_result_all_rostral.perm_result.sprob

disp('%%% LV p-values, caudal LC:')
pls_result_all_caudal.perm_result.sprob


%% save PLS results (multi-group only, for now)

save([dir.data, filesep, 'PLS_all_output_peak.mat'], ...
    'pls_result_all_peak', ...
    'pls_result_all_rostral', ...
    'pls_result_all_caudal');


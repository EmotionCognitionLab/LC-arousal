# this script prepares data for PLS analysis in matlab
# and saves relevant dataframes at data/derivatives/
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# prepare data for PLS analysis -------------------------------------------

# joining data from baseline, reactivity and recovery together
# (removing respiration rates to increase available complete cases)
data_PLS_physio <- left_join(
  data_physio_baseline %>%
    select(label_subject, HR_mean, 
           #resp_rate,
           BP_systolic, BP_diastolic,
           aSKNA_mean, log_RMSSD, log_hf_lomb, log_lf_lomb),
  data_physio_reactivity_avg %>%
    select(label_subject, HR_mean_react,
           #resp_rate_react,
           BP_systolic_react, BP_diastolic_react,
           aSKNA_mean_react, log_RMSSD_react, log_hf_lomb_react, log_lf_lomb_react),
  by = 'label_subject') %>%
  left_join(
    data_physio_recovery %>%
      select(label_subject, HR_mean_recov,
             #resp_rate_recov,
             BP_systolic_recov, BP_diastolic_recov,
             aSKNA_mean_recov, log_RMSSD_recov, log_hf_lomb_recov, log_lf_lomb_recov),
    by = 'label_subject'
  ) %>%
  left_join(data_LC_ratios %>%
              select(label_subject, age_group,
                     LC_ratio_meta_peak, LC_ratio_meta_rostral, LC_ratio_meta_caudal),
            by = 'label_subject') %>%
  select(label_subject, age_group, 
         HR_mean, 
         #resp_rate,
         BP_systolic, BP_diastolic,
         aSKNA_mean, log_RMSSD, log_hf_lomb, log_lf_lomb,
         HR_mean_react,
         #resp_rate_react,
         BP_systolic_react, BP_diastolic_react,
         aSKNA_mean_react, log_RMSSD_react, log_hf_lomb_react, log_lf_lomb_react,
         HR_mean_recov,
         #resp_rate_recov,
         BP_systolic_recov, BP_diastolic_recov,
         aSKNA_mean_recov, log_RMSSD_recov, log_hf_lomb_recov, log_lf_lomb_recov,
         LC_ratio_meta_peak, LC_ratio_meta_rostral, LC_ratio_meta_caudal) 

# save data with complete cases only
write.csv(data_PLS_physio %>% na.omit(),
          here('data', 'derivatives', 'LC-arousal_pls-all_avg.csv'),
          quote = FALSE, row.names = FALSE, na = 'NaN')

# store n used in PLS analyses (with complete cases)
n_PLS_avg <- data_PLS_physio %>% na.omit() %>% count(age_group)


# prepare data for PLS analysis (stroop only) -----------------------------

# joining data from baseline, reactivity and recovery together
# (for younger participants, challenge phase reflects only stroop)
# (removing respiration rates at the end to increase available complete cases)
data_PLS_physio_strooponly <- left_join(
  data_physio_baseline %>%
    select(label_subject, HR_mean, 
           resp_rate,
           BP_systolic, BP_diastolic,
           aSKNA_mean, log_RMSSD, log_hf_lomb, log_lf_lomb),
  data_physio_reactivity_avg_strooponly %>%
    select(label_subject, HR_mean_react,
           resp_rate_react,
           BP_systolic_react, BP_diastolic_react,
           aSKNA_mean_react, log_RMSSD_react, log_hf_lomb_react, log_lf_lomb_react),
  by = 'label_subject') %>%
  left_join(
    data_physio_recovery %>%
      select(label_subject, HR_mean_recov,
             resp_rate_recov,
             BP_systolic_recov, BP_diastolic_recov,
             aSKNA_mean_recov, log_RMSSD_recov, log_hf_lomb_recov, log_lf_lomb_recov),
    by = 'label_subject'
  ) %>%
  left_join(data_LC_ratios %>%
              select(label_subject, age_group,
                     LC_ratio_meta_peak, LC_ratio_meta_rostral, LC_ratio_meta_caudal),
            by = 'label_subject') %>%
  select(label_subject, age_group, 
         HR_mean, 
         #resp_rate,
         BP_systolic, BP_diastolic,
         aSKNA_mean, log_RMSSD, log_hf_lomb, log_lf_lomb,
         HR_mean_react,
         #resp_rate_react,
         BP_systolic_react, BP_diastolic_react,
         aSKNA_mean_react, log_RMSSD_react, log_hf_lomb_react, log_lf_lomb_react,
         HR_mean_recov,
         #resp_rate_recov,
         BP_systolic_recov, BP_diastolic_recov,
         aSKNA_mean_recov, log_RMSSD_recov, log_hf_lomb_recov, log_lf_lomb_recov,
         LC_ratio_meta_peak, LC_ratio_meta_rostral, LC_ratio_meta_caudal) 

# save data with complete cases only
write.csv(data_PLS_physio_strooponly %>% na.omit(),
          here('data', 'derivatives', 'LC-arousal_pls-all_avg_strooponly.csv'),
          quote = FALSE, row.names = FALSE, na = 'NaN')

# store n used in PLS analyses (with complete cases)
n_PLS_strooponly <- data_PLS_physio_strooponly %>% na.omit() %>% count(age_group)


# instructions to run PLS -------------------------------------------------

message('PLS data saved! Now run PLS analysis using the MATLAB scripts in `scripts/PLS/')

# run the following matlab scripts:
# 1. `scripts/PLS/A_prepare_PLS_input.m`
# 2. `scripts/PLS/B_run_PLS.m`

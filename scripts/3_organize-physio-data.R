# this script organizes physio data
# by (a) computing peak measures during the challenge phase
# and (b) calculating measures for baseline (rest), stress reactivity and stress recovery
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# log transform physio values ---------------------------------------------

data_physio <- data_physio %>%
  mutate(log_RMSSD = log(RMSSD),
         log_hf_lomb = log(hf_lomb),
         log_lf_lomb = log(lf_lomb)) %>%
  select(label_subject, label_session, phase,
         HR_mean, 
         resp_rate,
         BP_systolic,
         BP_diastolic,
         aSKNA_mean,
         log_RMSSD,
         log_hf_lomb,
         log_lf_lomb,
         include_ECG, tdflag, fdflag, include_RSP, include_BP)

data_physio_interval <- data_physio_interval %>%
  mutate(log_RMSSD = log(RMSSD)) %>%
  select(label_subject, label_session, phase, time,
         HR_mean,
         resp_rate,
         BP_systolic,
         BP_diastolic,
         aSKNA_mean,
         log_RMSSD,
         include_ECG, tdflag, include_RSP, include_BP)


# remove outliers from physio data & create summary -----------------------

# (this is performed for each age group separately)

### separate data by age group
# because outlier removal happens for each age group separately
data_physio_YA <- data_physio %>%
  left_join(data_LCA %>% select(label_subject, age_group),
            by = 'label_subject') %>%
  filter(age_group == 'YA')

data_physio_interval_YA <- data_physio_interval %>%
  left_join(data_LCA %>% select(label_subject, age_group),
            by = 'label_subject') %>%
  filter(age_group == 'YA')

data_physio_OA <- data_physio %>%
  left_join(data_LCA %>% select(label_subject, age_group),
            by = 'label_subject') %>%
  filter(age_group == 'OA')

data_physio_interval_OA <- data_physio_interval %>%
  left_join(data_LCA %>% select(label_subject, age_group),
            by = 'label_subject') %>%
  filter(age_group == 'OA')

### outlier removal - YA
# create summary of outliers identified by the MAD-median rule
summary_rmout_YA <- sapply(data_physio_YA[,4:11], summary_outliers)
n_percol_YA <- sapply(data_physio_YA[,4:11], function(x) {sum(!is.na(x))})
summary_rmout_YA <- data.frame(variable = names(summary_rmout_YA),
                            n_outliers = as.numeric(summary_rmout_YA),
                            pct_outliers = (as.numeric(summary_rmout_YA)/as.numeric(n_percol_YA))*100)
data_physio_YA[,4:11] <- purrr::map_df(data_physio_YA[,4:11], madmedianrule)

# remove outliers from interval physio measures & show tabular summary
summary_rmout_interval_YA <- sapply(data_physio_interval_YA[,5:10], summary_outliers)
n_percol_YA <- sapply(data_physio_interval_YA[,5:10], function(x) {sum(!is.na(x))})
summary_rmout_interval_YA <- data.frame(variable = names(summary_rmout_interval_YA),
                                     n_outliers = as.numeric(summary_rmout_interval_YA),
                                     pct_outliers = (as.numeric(summary_rmout_interval_YA)/as.numeric(n_percol_YA))*100)
data_physio_interval_YA[,5:10] <- purrr::map_df(data_physio_interval_YA[,5:10], madmedianrule)


### outlier removal - OA
# create summary of outliers identified by the MAD-median rule
summary_rmout_OA <- sapply(data_physio_OA[,4:11], summary_outliers)
n_percol_OA <- sapply(data_physio_OA[,4:11], function(x) {sum(!is.na(x))})
summary_rmout_OA <- data.frame(variable = names(summary_rmout_OA),
                               n_outliers = as.numeric(summary_rmout_OA),
                               pct_outliers = (as.numeric(summary_rmout_OA)/as.numeric(n_percol_OA))*100)
data_physio_OA[,4:11] <- purrr::map_df(data_physio_OA[,4:11], madmedianrule)

# remove outliers from interval physio measures & show tabular summary
summary_rmout_interval_OA <- sapply(data_physio_interval_OA[,5:10], summary_outliers)
n_percol_OA <- sapply(data_physio_interval_OA[,5:10], function(x) {sum(!is.na(x))})
summary_rmout_interval_OA <- data.frame(variable = names(summary_rmout_interval_OA),
                                        n_outliers = as.numeric(summary_rmout_interval_OA),
                                        pct_outliers = (as.numeric(summary_rmout_interval_OA)/as.numeric(n_percol_OA))*100)
data_physio_interval_OA[,5:10] <- purrr::map_df(data_physio_interval_OA[,5:10], madmedianrule)

### join outlier removal summaries
summary_rmout <- as.data.frame(
  rbind(summary_rmout_YA %>% mutate(age_group = 'Younger'),
        summary_rmout_OA %>% mutate(age_group = 'Older'))
)
summary_rmout_interval <- as.data.frame(
  rbind(summary_rmout_interval_YA %>% mutate(age_group = 'Younger'),
        summary_rmout_interval_OA %>% mutate(age_group = 'Older'))
)
rm(summary_rmout_YA, summary_rmout_OA,
   summary_rmout_interval_YA, summary_rmout_interval_OA)

### replace dataframes with outlier-removed versions
data_physio <- as.data.frame(rbind(
  data_physio_YA, data_physio_OA)
)

data_physio_interval <- as.data.frame(rbind(
  data_physio_interval_YA, data_physio_interval_OA)
)

rm(data_physio_YA, data_physio_OA,
   data_physio_interval_YA, data_physio_interval_OA,
   n_percol_YA, n_percol_OA)


# restrict timing on interval physio data ---------------------------------

# this restricts PASAT data to 160sec and Stroop data to 120sec
# (timestamps refer to middle of each window)

data_physio_interval_pasat <- data_physio_interval %>%
  filter(phase == 'pasat') %>%
  filter(time <= 150)

data_physio_interval_stroop <- data_physio_interval %>%
  filter(phase == 'stroop') %>%
  filter(time <= 110)

data_physio_interval <- as.data.frame(
  rbind(
    data_physio_interval_pasat,
    data_physio_interval_stroop
  )
)

rm(data_physio_interval_pasat, data_physio_interval_stroop)


# reorganize interval physio data for computing peak metrics --------------

# reset time values to be relative to start of entire challenge phase
# this doesn't affect OA, only YA since they had two tasks:
# (10 for stroop would be 170 for PASAT, so add 160 to stroop time values for YA)
# (so for YA, < 160 = pasat, > 160 = stroop)
data_physio_interval_stroopYA <- data_physio_interval %>%
  filter(phase == 'stroop', age_group == 'YA') %>%
  rowwise() %>%
  mutate(time = time + 160)

data_physio_interval_stroopOA <- data_physio_interval %>%
  filter(phase == 'stroop', age_group == 'OA') 

# then re-join data, including only challenge tasks
# so that interval data now only includes challenge phase
# (we use baseline and recovery metrics from data_physio as defined above)
data_physio_interval <- as.data.frame(
  rbind(
    data_physio_interval %>%
      filter(phase == 'pasat'),
    data_physio_interval_stroopYA,
    data_physio_interval_stroopOA
  )
)
rm(data_physio_interval_stroopYA, data_physio_interval_stroopOA)


# compute peak metrics during challenge phase -----------------------------

data_physio_peak_times <- left_join(
  
  # find peaks and peak onsets for heart rate
  data_physio_interval %>%
    ungroup() %>%
    group_by(label_subject) %>%
    summarize(HR_mean_peak = ifelse(sum(!is.na(HR_mean)) == 0, NA,
                                    max(HR_mean, na.rm = TRUE)),
              HR_mean_peaktime = ifelse(sum(!is.na(HR_mean)) == 0, NA,
                                        time[which.max(HR_mean)])),
  
  # find peaks and peak onsets for breathing rate
  data_physio_interval %>%
    ungroup() %>%
    group_by(label_subject) %>%
    summarize(resp_rate_peak = ifelse(sum(!is.na(resp_rate)) == 0, NA,
                                      max(resp_rate, na.rm = TRUE)),
              resp_rate_peaktime = ifelse(sum(!is.na(resp_rate)) == 0, NA,
                                          time[which.max(resp_rate)])),
  
  by = 'label_subject'
) %>%
  left_join(
    # find peaks and peat onsets for systolic BP
    data_physio_interval %>%
      ungroup() %>%
      group_by(label_subject) %>%
      summarize(BP_systolic_peak = ifelse(sum(!is.na(BP_systolic)) == 0, NA,
                                          max(BP_systolic, na.rm = TRUE)),
                BP_systolic_peaktime = ifelse(sum(!is.na(BP_systolic)) == 0, NA,
                                              time[which.max(BP_systolic)])) ,
    
    by = 'label_subject'
  ) %>%
  left_join(
    # find peaks and peat onsets for diastolic BP
    data_physio_interval %>%
      ungroup() %>%
      group_by(label_subject) %>%
      summarize(BP_diastolic_peak = ifelse(sum(!is.na(BP_diastolic)) == 0, NA,
                                           max(BP_diastolic, na.rm = TRUE)),
                BP_diastolic_peaktime = ifelse(sum(!is.na(BP_diastolic)) == 0, NA,
                                               time[which.max(BP_diastolic)])),
    by = 'label_subject'
  ) %>%
  left_join(
    # find peaks and peak onsets for sympathetic tone
    data_physio_interval %>%
      ungroup() %>%
      group_by(label_subject) %>%
      summarize(aSKNA_mean_peak = ifelse(sum(!is.na(aSKNA_mean)) == 0, NA,
                                         max(aSKNA_mean, na.rm = TRUE)),
                aSKNA_mean_peaktime = ifelse(sum(!is.na(aSKNA_mean)) == 0, NA,
                                             time[which.max(aSKNA_mean)])),
    
    by = 'label_subject'
  ) %>%
  left_join(
    # find peaks and peak onsets for RMSSD
    data_physio_interval %>%
      ungroup() %>%
      group_by(label_subject) %>%
      summarize(log_RMSSD_min = ifelse(sum(!is.na(log_RMSSD)) == 0, NA,
                                       max(log_RMSSD, na.rm = TRUE)),
                log_RMSSD_mintime = ifelse(sum(!is.na(log_RMSSD)) == 0, NA,
                                           time[which.max(log_RMSSD)])),
    by = 'label_subject'
  )


# join data from all phases (peak version) --------------------------------

data_physio_baseline <- data_physio %>%
  filter(phase == 'baseline') %>%
  select(label_subject, label_session, phase, 
         HR_mean,
         resp_rate,
         BP_systolic,
         BP_diastolic,
         aSKNA_mean,
         log_RMSSD,
         log_hf_lomb,
         log_lf_lomb)

data_physio_peak <- as.data.frame(
  
  rbind(
    
    # baseline physio data
    data_physio %>%
      filter(phase %in% c('baseline', 'recovery')) %>%
      select(label_subject, phase, 
             HR_mean,
             resp_rate,
             BP_systolic,
             BP_diastolic,
             aSKNA_mean,
             log_RMSSD,
             log_hf_lomb,
             log_lf_lomb),
    
    # stress task physio data - peak values for specific metrics only
    data_physio_peak_times %>%
      mutate(phase = 'peak') %>%
      select(label_subject, phase,
             HR_mean = HR_mean_peak, 
             resp_rate = resp_rate_peak, 
             BP_systolic = BP_systolic_peak,
             BP_diastolic = BP_diastolic_peak,
             aSKNA_mean = aSKNA_mean_peak,
             log_RMSSD = log_RMSSD_min) %>%
      
      # bind age group information
      left_join(data_LCA %>% 
                  select(label_subject, age_group),
                by = 'label_subject') %>%
      
      # bind LF and HF power values from data_physio dataframe
      left_join(
        
        as.data.frame(
          rbind(
            data_physio %>%
              filter(phase %in% c('pasat', 'stroop'),
                     age_group == 'YA') %>%
              group_by(label_subject) %>%
              # average across pasat and stroop for YA
              summarize(log_hf_lomb = mean(log_hf_lomb, na.rm = TRUE),
                        log_lf_lomb = mean(log_lf_lomb, na.rm = TRUE)) %>%
              rowwise() %>%
              mutate(log_hf_lomb = ifelse(log_hf_lomb == 'NaN', NA, log_hf_lomb),
                     log_lf_lomb = ifelse(log_lf_lomb == 'NaN', NA, log_lf_lomb)),
            
            data_physio %>%
              # just take stroop values for OA
              filter(phase == 'stroop',
                     age_group == 'OA',
                     ! label_subject %in% c(subs_OA_oldtask)) %>%
              select(label_subject, 
                     log_hf_lomb, log_lf_lomb)
            
          )
        ),
        
        by = c('label_subject')) %>%
      select(-age_group)
    
  )
)


# compute reactivity (peak version) ---------------------------------------

# (reactivity = difference between baseline and challenge phases)

data_physio_reactivity <- data_physio_peak %>%
  filter(phase %in% c('baseline', 'peak')) %>%
  pivot_wider(names_from = phase,
              names_glue = "{.value}_{phase}",
              values_from = c(HR_mean, 
                              resp_rate, 
                              BP_systolic,
                              BP_diastolic,
                              aSKNA_mean,
                              log_RMSSD,
                              log_hf_lomb,
                              log_lf_lomb)) %>%
  rowwise() %>%
  mutate(HR_mean_react = ((HR_mean_peak) - HR_mean_baseline),
         resp_rate_react = ((resp_rate_peak) - resp_rate_baseline),
         BP_systolic_react = ((BP_systolic_peak) - BP_systolic_baseline),
         BP_diastolic_react = ((BP_diastolic_peak) - BP_diastolic_baseline),
         aSKNA_mean_react = ((aSKNA_mean_peak) - aSKNA_mean_baseline),
         log_RMSSD_react = ((log_RMSSD_peak) - log_RMSSD_baseline),
         log_hf_lomb_react = ((log_hf_lomb_peak) - log_hf_lomb_baseline),
         log_lf_lomb_react = ((log_lf_lomb_peak) - log_lf_lomb_baseline)) %>%
  select(label_subject, HR_mean_react:log_lf_lomb_react) %>%
  left_join(data_LCA %>%
              select(label_subject, age_group, gender),
            by = 'label_subject')


# compute recovery (peak version) -----------------------------------------

# (recovery = difference between recovery and baseline phases)

data_physio_recovery <- data_physio_peak %>%
  filter(phase %in% c('recovery', 'baseline')) %>%
  pivot_wider(names_from = phase,
              names_glue = "{.value}_{phase}",
              values_from = c(HR_mean, 
                              resp_rate, 
                              BP_systolic,
                              BP_diastolic,
                              aSKNA_mean,
                              log_RMSSD,
                              log_hf_lomb,
                              log_lf_lomb)) %>%
  rowwise() %>%
  mutate(HR_mean_recov = ((HR_mean_recovery) - HR_mean_baseline),
         resp_rate_recov = ((resp_rate_recovery) - resp_rate_baseline),
         BP_systolic_recov = ((BP_systolic_recovery) - BP_systolic_baseline),
         BP_diastolic_recov = ((BP_diastolic_recovery) - BP_diastolic_baseline),
         aSKNA_mean_recov = ((aSKNA_mean_recovery) - aSKNA_mean_baseline),
         log_RMSSD_recov = ((log_RMSSD_recovery) - log_RMSSD_baseline),
         log_hf_lomb_recov = ((log_hf_lomb_recovery) - log_hf_lomb_baseline),
         log_lf_lomb_recov = ((log_lf_lomb_recovery) - log_lf_lomb_baseline)) %>%
  select(label_subject, HR_mean_recov:log_lf_lomb_recov) %>%
  left_join(data_LCA %>%
              select(label_subject, age_group, gender),
            by = 'label_subject')


# bind demographic data to physio data (peak version) ---------------------

data_physio_peak <- data_physio_peak %>%
  rowwise() %>%
  mutate(phase = str_to_title(phase)) %>%
  left_join(data_LCA %>% select(label_subject, age_group, gender),
            by = 'label_subject') %>%
  select(label_subject, age_group, gender, phase, HR_mean:log_lf_lomb)


# remove unneeded variables -----------------------------------------------

rm(data_physio_interval, data_physio_peak_times)


# subset challenge phase data (avg version) -------------------------------

# ( average over stroop and pasat for younger)
data_physio_stress_YA <- data_physio %>%
  filter(age_group == 'YA') %>%
  filter(phase %in% c('pasat', 'stroop')) %>%
  group_by(label_subject, label_session) %>%
  summarize(HR_mean = mean(HR_mean, na.rm = TRUE),
            resp_rate = mean(resp_rate, na.rm = TRUE),
            BP_systolic = mean(BP_systolic, na.rm = TRUE),
            BP_diastolic = mean(BP_diastolic, na.rm = TRUE),
            aSKNA_mean = mean(aSKNA_mean, na.rm = TRUE),
            log_RMSSD = mean(log_RMSSD, na.rm = TRUE),
            log_hf_lomb = mean(log_hf_lomb, na.rm = TRUE),
            log_lf_lomb = mean(log_lf_lomb, na.rm = TRUE)) %>%
  mutate(phase = 'challenge') %>%
  select(label_subject, label_session, phase, 
         HR_mean,
         resp_rate,
         BP_systolic,
         BP_diastolic,
         aSKNA_mean,
         log_RMSSD,
         log_hf_lomb,
         log_lf_lomb)
data_physio_stress_YA[data_physio_stress_YA == 'NaN'] <- NA


data_physio_stress_OA <- data_physio %>%
  filter(age_group == 'OA') %>%
  filter(! label_subject %in% subs_OA_oldtask) %>%
  filter(phase == 'stroop') %>%
  select(-phase) %>%
  mutate(phase = 'challenge') %>%
  select(label_subject, label_session, phase, 
         HR_mean,
         resp_rate,
         BP_systolic,
         BP_diastolic,
         aSKNA_mean,
         log_RMSSD,
         log_hf_lomb,
         log_lf_lomb)

data_physio_stress <- as.data.frame(
  rbind(data_physio_stress_YA,
        data_physio_stress_OA)
)


# join data from all phases (avg version) ---------------------------------

# (not computing reactivity from these data, they are for visualization
# and testing effectiveness of acute stress induction protocol, see script 6_)
data_physio_avg <- as.data.frame(
  
  rbind(
    
    data_physio %>%
      filter(phase == 'baseline') %>%
      select(label_subject, label_session, phase, 
             HR_mean,
             resp_rate,
             BP_systolic,
             BP_diastolic,
             aSKNA_mean,
             log_RMSSD,
             log_hf_lomb,
             log_lf_lomb),
    
    data_physio_stress,
    
    data_physio %>%
      filter(phase == 'recovery') %>%
      select(label_subject, label_session, phase, 
             HR_mean,
             resp_rate,
             BP_systolic,
             BP_diastolic,
             aSKNA_mean,
             log_RMSSD,
             log_hf_lomb,
             log_lf_lomb))
  
) %>%
  # make phase title case for plotting, consistent with peak dataframe
  rowwise() %>%
  mutate(phase = str_to_title(phase)) %>%
  left_join(data_LCA %>% select(label_subject, age_group, gender),
            by = 'label_subject') %>%
  select(label_subject, age_group, gender, phase, HR_mean:log_lf_lomb)


# compute reactivity (avg version) ----------------------------------------

# # (reactivity = difference between baseline and challenge phases)
# 
# data_physio_reactivity_avg <- data_physio_avg %>%
#   filter(phase %in% c('Baseline', 'Challenge')) %>%
#   pivot_wider(names_from = phase,
#               names_glue = "{.value}_{phase}",
#               values_from = c(HR_mean,
#                               resp_rate,
#                               BP_systolic,
#                               BP_diastolic,
#                               aSKNA_mean,
#                               log_RMSSD,
#                               log_hf_lomb,
#                               log_lf_lomb)) %>%
#   rowwise() %>%
#   mutate(HR_mean_react = ((HR_mean_Challenge) - HR_mean_Baseline),
#          resp_rate_react = ((resp_rate_Challenge) - resp_rate_Baseline),
#          BP_systolic_react = ((BP_systolic_Challenge) - BP_systolic_Baseline),
#          BP_diastolic_react = ((BP_diastolic_Challenge) - BP_diastolic_Baseline),
#          aSKNA_mean_react = ((aSKNA_mean_Challenge) - aSKNA_mean_Baseline),
#          log_RMSSD_react = ((log_RMSSD_Challenge) - log_RMSSD_Baseline),
#          log_hf_lomb_react = ((log_hf_lomb_Challenge) - log_hf_lomb_Baseline),
#          log_lf_lomb_react = ((log_lf_lomb_Challenge) - log_lf_lomb_Baseline)) %>%
#   dplyr::select(label_subject, HR_mean_react:log_lf_lomb_react) %>%
#   left_join(data_LCA %>%
#               dplyr::select(label_subject, age_group, gender),
#             by = 'label_subject')


# compute recovery (avg version) ------------------------------------------

# # (recovery = difference between recovery and baseline phases)
# 
# data_physio_recovery_avg <- data_physio_avg %>%
#   filter(phase %in% c('Recovery', 'Baseline')) %>%
#   pivot_wider(names_from = phase,
#               names_glue = "{.value}_{phase}",
#               values_from = c(HR_mean,
#                               resp_rate,
#                               BP_systolic,
#                               BP_diastolic,
#                               aSKNA_mean,
#                               log_RMSSD,
#                               log_hf_lomb,
#                               log_lf_lomb)) %>%
#   rowwise() %>%
#   mutate(HR_mean_recov = ((HR_mean_Recovery) - HR_mean_Baseline),
#          resp_rate_recov = ((resp_rate_Recovery) - resp_rate_Baseline),
#          BP_systolic_recov = ((BP_systolic_Recovery) - BP_systolic_Baseline),
#          BP_diastolic_recov = ((BP_diastolic_Recovery) - BP_diastolic_Baseline),
#          aSKNA_mean_recov = ((aSKNA_mean_Recovery) - aSKNA_mean_Baseline),
#          log_RMSSD_recov = ((log_RMSSD_Recovery) - log_RMSSD_Baseline),
#          log_hf_lomb_recov = ((log_hf_lomb_Recovery) - log_hf_lomb_Baseline),
#          log_lf_lomb_recov = ((log_lf_lomb_Recovery) - log_lf_lomb_Baseline)) %>%
#   dplyr::select(label_subject, HR_mean_recov:log_lf_lomb_recov) %>%
#   left_join(data_LCA %>%
#               dplyr::select(label_subject, age_group, gender),
#             by = 'label_subject')


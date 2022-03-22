# this script performs qc on the physio data
# by excluding measures based on signal quality
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# exclude older participants who completed earlier stress task version --------

# (these get excluded from analyses of stress reactivity and recovery, but not baseline)
subs_OA_oldtask <- unique(data_LCA$label_subject[data_LCA$completed_oldversion_stresstask == 1])

# exclude these older participants' data for challenge and recovery
excl_OA_oldtask <- which(data_physio$label_subject %in% subs_OA_oldtask
                         & data_physio$phase %in% c('pasat', 'stroop', 'recovery'))
data_physio <- data_physio[-excl_OA_oldtask,]
rm(excl_OA_oldtask)

# exclude these participants in interval data
excl_OA_oldtask <- which(data_physio_interval$label_subject %in% subs_OA_oldtask
                         & data_physio_interval$phase %in% c('pasat', 'stroop', 'recovery'))
data_physio_interval <- data_physio_interval[-excl_OA_oldtask,]
rm(excl_OA_oldtask)


# record info on physio signals excluded based on manual inspection -------

# (manual inspection was performed for QC of ECG, RSP & BP signals)
data_physio$phase <- factor(data_physio$phase, levels = c('baseline', 'pasat', 'stroop', 'recovery'))
excl_physio_manual <- data_physio %>%
  left_join(data_LCA %>% select(label_subject, age_group),
            by = 'label_subject') %>%
  summarize(n_signals = n(),
            # n and % of ECG signals
            n_excl_ECG_manual = sum(include_ECG == 0),
            frac_excl_ECG_manual = n_excl_ECG_manual/n_signals,
            # n and % of RSP signals
            n_excl_RSP_manual = sum(include_RSP == 0),
            frac_excl_RSP_manual = n_excl_RSP_manual/n_signals,
            # n and % of BP signals
            n_excl_BP_manual = sum(include_BP == 0),
            frac_excl_BP_manual = n_excl_BP_manual/n_signals
            ) %>%
  rowwise() %>%
  # summarize n and % in text (customized for manuscript)
  mutate(ECG_excl = format_pct_n(frac_excl_ECG_manual, n_excl_ECG_manual),
         RSP_excl = format_n_pct(n_excl_RSP_manual, frac_excl_RSP_manual),
         BP_excl = format_n_pct(n_excl_RSP_manual, frac_excl_BP_manual))


# parameters for exclusions -----------------------------------------------

# (segments with SQI below this threshold get excluded)
thresh_rrsqi <- 0.7


# apply exclusions to ECG derivative measures -----------------------------

# record metrics on additionally excluded ECG segments
excl_ECG_rrsqi <- data_physio %>%
  left_join(data_LCA %>% select(label_subject, age_group),
            by = 'label_subject') %>%

  # record only metrics on those not excluded during manual QC
  filter(include_ECG == 1) %>%
  
  # summarize segments excluded for each reason
  summarize(n_signals = n(),
            # n and % of signals excluded for low quality
            n_excl_ECG_lowqual = sum(tdflag == 2 | rr_sqi < thresh_rrsqi, na.rm = TRUE),
            frac_excl_ECG_lowqual = n_excl_ECG_lowqual/n_signals,
            
            # n and % where of signals excluded for AF being detected
            n_excl_ECG_AFdet = sum(AF_det > 0, na.rm = TRUE),
            frac_excl_ECG_AFdet = n_excl_ECG_AFdet/n_signals,
            
            # n and % where too much data was missing from window for HRV analysis
            n_excl_ECG_missing = sum(tdflag == 3 & AF_det == 0, na.rm = TRUE),
            frac_excl_ECG_missing = n_excl_ECG_missing/n_signals)

# mark ECG signals with tdflag 2 or 3, or with AF detected, or with rr_sqi below threshold, to be excluded
data_physio$include_ECG[data_physio$tdflag == 2 | data_physio$tdflag == 3 | data_physio$AF_det > 0] <- 0
data_physio$include_ECG[data_physio$rr_sqi < thresh_rrsqi] <- 0

# then set metrics from flagged segments to NA
data_physio$HR_mean[data_physio$include_ECG == 0] <- NA
data_physio$RMSSD[data_physio$include_ECG == 0] <- NA
data_physio$lf_lomb[data_physio$include_ECG == 0] <- NA
data_physio$hf_lomb[data_physio$include_ECG == 0] <- NA
data_physio$aSKNA_mean[data_physio$include_ECG == 0] <- NA


# apply exclusions to RSP derivative measures -----------------------------

# (summary of segments excluded for signal quality is above)
data_physio$resp_rate[data_physio$include_RSP == 0] <- NA


# apply exclusions to BP derivative measures ------------------------------

# (summary of segments excluded for signal quality is above)

# summary of segments where calibration happened mid-segment (after signal quality exclusions)
excl_BP_trimmed <- data_physio %>%
  left_join(data_LCA %>% select(label_subject, age_group),
            by = 'label_subject') %>%
  filter(include_BP == 1) %>%
  summarize(n_signals = n(),
            n_excl_BP_trimmed = sum(bp_trimmed == 1, na.rm = TRUE),
            frac_excl_BP_trimmed = n_excl_BP_trimmed/n_signals)

data_physio$include_BP[data_physio$bp_trimmed == 1] <- 0
data_physio$BP_systolic[data_physio$bp_trimmed == 1] <- NA
data_physio$BP_diastolic[data_physio$bp_trimmed == 1] <- NA


# apply exclusions to interval physio data --------------------------------

# ECG (note: no AF metrics for interval data)
data_physio_interval$include_ECG[data_physio_interval$tdflag == 2 | data_physio_interval$tdflag == 3] <- 0
data_physio_interval$include_ECG[data_physio_interval$rr_sqi < thresh_rrsqi] <- 0
data_physio_interval$HR_mean[data_physio_interval$include_ECG == 0] <- NA
data_physio_interval$aSKNA_mean[data_physio_interval$include_ECG == 0] <- NA
data_physio_interval$RMSSD[data_physio_interval$include_ECG == 0] <- NA

# RSP
data_physio_interval$resp_rate[data_physio_interval$include_RSP == 0] <- NA

# BP
data_physio_interval$include_BP[data_physio$bp_trimmed == 1] <- 0
data_physio_interval$BP_systolic[data_physio_interval$include_BP == 0] <- NA
data_physio_interval$BP_diastolic[data_physio_interval$include_BP == 0] <- NA


# summary of physio signals included for analysis -------------------------

incl_physio <- data_physio %>%
  left_join(data_LCA %>% select(label_subject, age_group),
            by = 'label_subject') %>%
  group_by(age_group, phase) %>%
  summarize(n_ECG = sum(include_ECG == 1),
            n_RSP = sum(include_RSP == 1),
            n_BP = sum(include_BP == 1)) %>%
  arrange(desc(age_group))


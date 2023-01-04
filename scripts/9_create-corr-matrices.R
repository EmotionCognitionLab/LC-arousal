# this script creates correlation matrices
# reflecting associations between LC and arousal measures
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# prepare data for correlation matrices -----------------------------------

# average
data_corr_avg <- left_join(
  data_physio_baseline %>%
    select(label_subject, HR_mean,
           resp_rate,
           BP_systolic, BP_diastolic,
           aSKNA_mean, log_RMSSD, log_hf_lomb, log_lf_lomb),
  data_physio_reactivity_avg %>%
    select(label_subject, HR_mean_react,
           resp_rate_react,
           BP_systolic_react, BP_diastolic_react,
           aSKNA_mean_react, log_RMSSD_react, log_hf_lomb_react, log_lf_lomb_react),
  by = 'label_subject') %>%
  left_join(
    data_physio_recovery_avg %>%
      select(label_subject, HR_mean_recov,
             resp_rate_recov,
             BP_systolic_recov, BP_diastolic_recov,
             aSKNA_mean_recov, log_RMSSD_recov, log_hf_lomb_recov, log_lf_lomb_recov),
    by = 'label_subject'
  ) %>%
  left_join(data_LC_ratios %>%
              select(label_subject, age_group,
                     LC_ratio_meta_peak, LC_ratio_meta_rostral, LC_ratio_meta_caudal),
            by = 'label_subject')


# average, stroop only
data_corr_avg_strooponly <- left_join(
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
            by = 'label_subject')


# create correlation matrices (average) -----------------------------------

### YA

# correlation coefficient matrix
cor_mat_all_YA <- cor(
  data_corr_avg %>%
    filter(age_group == 'YA') %>%
    select(-c(label_subject, age_group)),
  use = 'pairwise.complete.obs',
  method = 'pearson'
)

# correlation p-value matrix
cor_p_all_YA <- ggcorrplot::cor_pmat(
  data_corr_avg %>%
    filter(age_group == 'YA') %>%
    select(-c(label_subject, age_group)),
  use = 'pairwise.complete.obs',
  method = 'pearson'
)

# visualize
cor_plot_all_YA <- ggcorrplot2::ggcorrplot(
  cor_mat_all_YA,
  method = 'ellipse',
  type = 'full',
  show.diag = FALSE,
  p.mat = cor_p_all_YA,
  insig = 'label_sig',
  sig.lvl = c(0.05, 0.01, 0.001),
  pch = "+", pch.cex = 4
)

### OA
# correlation coefficient matrix
cor_mat_all_OA <- cor(
  data_corr_avg %>%
    filter(age_group == 'OA') %>%
    select(-c(label_subject, age_group)),
  use = 'pairwise.complete.obs',
  method = 'pearson'
)

# correlation p-value matrix
cor_p_all_OA <- ggcorrplot::cor_pmat(
  data_corr_avg %>%
    filter(age_group == 'OA') %>%
    select(-c(label_subject, age_group)),
  use = 'pairwise.complete.obs',
  method = 'pearson'
)

# visualize
cor_plot_all_OA <- ggcorrplot2::ggcorrplot(
  cor_mat_all_OA,
  method = 'ellipse',
  type = 'full',
  show.diag = FALSE,
  p.mat = cor_p_all_OA,
  insig = 'label_sig',
  sig.lvl = c(0.05, 0.01, 0.001),
  pch = "+", pch.cex = 4
)

# create composite YA + OA figure
cor_plot_all_avg <- plot_grid(
  cor_plot_all_YA + theme(legend.position = "none"),
  cor_plot_all_OA + theme(legend.position = "none"),
  rel_widths = c(1, 1), nrow = 2, align = 'hv',
  labels = c("A. Younger adults", "B. Older adults"), label_x = 0, label_y = 1)

# extract & add legend
cor_plot_all_avg <- plot_grid(cor_plot_all_avg,
                              get_legend(cor_plot_all_YA),
                              ncol = 2,
                              rel_heights  = c(1, 0.05))

# save figure
ggsave(cor_plot_all_avg, filename = here('figures', 'figure5_corr_matrices_OLD_avg.svg'),
       device = 'svg', bg = 'white',
       width = 15, height = 20)


# perform correlation analyses for significant pairs ----------------------

###### younger adults

# peak LC & breathing rate during recovery
cor_YA_LCpeak_resp_recov <- cor.test(~ LC_ratio_meta_peak + resp_rate_recov, data_corr_avg %>% filter(age_group == 'YA'))

# rostral LC & aSKNA during recovery
cor_YA_LCrostral_aSKNA_recov <- cor.test(~ LC_ratio_meta_rostral + aSKNA_mean_recov, data_corr_avg %>% filter(age_group == 'YA'))

###### older adults

# peak LC & LF power at baseline
cor_OA_LCpeak_LF_baseline <- cor.test(~ LC_ratio_meta_peak + log_lf_lomb, data_corr_avg %>% filter(age_group == 'OA'))

# caudal LC & RMSSD during recovery (marginally significant)
cor_OA_LCcaudal_HF_recov <- cor.test(~ LC_ratio_meta_caudal + log_hf_lomb_recov, data_corr_avg %>% filter(age_group == 'OA'))


# create correlation matrices (avg, stroop only) -------------------------

### YA

# correlation coefficient matrix
cor_mat_all_YA_strooponly <- cor(
  data_corr_avg_strooponly %>%
    filter(age_group == 'YA') %>%
    select(-c(label_subject, age_group)),
  use = 'pairwise.complete.obs',
  method = 'pearson'
)

# correlation p-value matrix
cor_p_all_YA_strooponly <- ggcorrplot::cor_pmat(
  data_corr_avg_strooponly %>%
    filter(age_group == 'YA') %>%
    select(-c(label_subject, age_group)),
  use = 'pairwise.complete.obs',
  method = 'pearson'
)

# visualize
cor_plot_all_YA_strooponly <- ggcorrplot2::ggcorrplot(
  cor_mat_all_YA_strooponly, 
  method = 'ellipse',
  type = 'full',
  show.diag = FALSE,
  p.mat = cor_p_all_YA_strooponly, 
  insig = 'label_sig', 
  sig.lvl = c(0.05, 0.01, 0.001),
  pch = "+", pch.cex = 4
)

### OA
# correlation coefficient matrix
cor_mat_all_OA_strooponly <- cor(
  data_corr_avg_strooponly %>%
    filter(age_group == 'OA') %>%
    select(-c(label_subject, age_group)),
  use = 'pairwise.complete.obs',
  method = 'pearson'
)

# correlation p-value matrix
cor_p_all_OA_strooponly <- ggcorrplot::cor_pmat(
  data_corr_avg_strooponly %>%
    filter(age_group == 'OA') %>%
    select(-c(label_subject, age_group)),
  use = 'pairwise.complete.obs',
  method = 'pearson'
)

# visualize
cor_plot_all_OA_strooponly <- ggcorrplot2::ggcorrplot(
  cor_mat_all_OA_strooponly, 
  method = 'ellipse',
  type = 'full',
  show.diag = FALSE,
  p.mat = cor_p_all_OA_strooponly, 
  insig = 'label_sig', 
  sig.lvl = c(0.05, 0.01, 0.001),
  pch = "+", pch.cex = 4
)

# create composite YA + OA figure
cor_plot_all_avg_strooponly <- plot_grid(
  cor_plot_all_YA_strooponly + theme(legend.position = "none"),
  cor_plot_all_OA_strooponly + theme(legend.position = "none"),
  rel_widths = c(1, 1), nrow = 2, align = 'hv',
  labels = c("A. Younger adults", "B. Older adults"), label_x = 0, label_y = 1)

# extract & add legend
cor_plot_all_peak_strooponly <- plot_grid(cor_plot_all_avg_strooponly, 
                                          get_legend(cor_plot_all_YA_strooponly), 
                                          ncol = 2,
                                          rel_heights  = c(1, 0.05))

# save figure
ggsave(cor_plot_all_avg_strooponly, 
       filename = here('figures', 'figureS4_corr_matrices_OLD_avg_strooponly.svg'), 
       device = 'svg', bg = 'white',
       width = 15, height = 20)


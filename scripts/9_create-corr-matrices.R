# this script creates correlation matrices
# reflecting associations between LC and arousal measures
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# prepare data for correlation matrices -----------------------------------

# peak
data_corr_peak <- left_join(
  data_physio_baseline %>%
  select(label_subject, HR_mean, 
         resp_rate,
         BP_systolic, BP_diastolic,
         aSKNA_mean, log_RMSSD, log_hf_lomb, log_lf_lomb),
  data_physio_reactivity %>%
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


# create correlation matrices (peak) --------------------------------------

### YA

# correlation coefficient matrix
cor_mat_all_YA <- cor(
  data_corr_peak %>%
    filter(age_group == 'YA') %>%
    select(-c(label_subject, age_group)),
  use = 'pairwise.complete.obs',
  method = 'pearson'
)

# correlation p-value matrix
cor_p_all_YA <- ggcorrplot::cor_pmat(
  data_corr_peak %>%
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
  data_corr_peak %>%
    filter(age_group == 'OA') %>%
    select(-c(label_subject, age_group)),
  use = 'pairwise.complete.obs',
  method = 'pearson'
)

# correlation p-value matrix
cor_p_all_OA <- ggcorrplot::cor_pmat(
  data_corr_peak %>%
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
cor_plot_all_peak <- plot_grid(
  cor_plot_all_YA + theme(legend.position = "none"),
  cor_plot_all_OA + theme(legend.position = "none"),
  rel_widths = c(1, 1), nrow = 2, align = 'hv',
  labels = c("A. Younger adults", "B. Older adults"), label_x = 0, label_y = 1)

# extract & add legend
cor_plot_all_peak <- plot_grid(cor_plot_all_peak, 
               get_legend(cor_plot_all_YA), 
               ncol = 2,
               rel_heights  = c(1, 0.05))

# save figure
ggsave(cor_plot_all_peak, filename = here('figures', 'figure5_corr_matrices_OLD_peak.svg'), 
       device = 'svg', bg = 'white',
       width = 15, height = 20)


# perform correlation analyses for significant pairs ----------------------


###### younger adults

# peak LC & breathing rate during recovery
cor_YA_LCpeak_resp_recov <- cor.test(~ LC_ratio_meta_peak + resp_rate_recov, data_corr_peak %>% filter(age_group == 'YA'))
 
# rostral LC & aSKNA during recovery
cor_YA_LCrostral_aSKNA_recov <- cor.test(~ LC_ratio_meta_rostral + aSKNA_mean_recov, data_corr_peak %>% filter(age_group == 'YA'))
 

###### older adults

# peak LC & LF power at baseline
cor_OA_LCpeak_LF_baseline <- cor.test(~ LC_ratio_meta_peak + log_lf_lomb, data_corr_peak %>% filter(age_group == 'OA'))

# caudal LC & sysBP reactivity
cor_OA_LCcaudal_sysBP_react <- cor.test(~ LC_ratio_meta_caudal + BP_systolic_react, data_corr_peak %>% filter(age_group == 'OA'))

# caudal LC & RMSSD during recovery (marginally significant)
cor_OA_LCcaudal_HF_recov <- cor.test(~ LC_ratio_meta_caudal + log_hf_lomb_recov, data_corr_peak %>% filter(age_group == 'OA'))

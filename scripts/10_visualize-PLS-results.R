# this script visualizes results of PLS analyses
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# set color palette for PLS figure ----------------------------------------

# (OA color is slightly darker than for other figures, 
# for increased visibility)
palette_agegr_darker <- c('Younger' = '#00AFBB',
                          'Older' = '#e7b800')


# update variable names for figure ----------------------------------------

names(data_PLS_physio) <- c('label_subject', 'age_group',
                            'Heart rate', 'Systolic BP', 'Diastolic BP', 'Symp. tone', 'RMSSD', 'HF power', 'LF power',
                            'Heart rate, react', 'Systolic BP, react', 'Diastolic BP, react', 'Symp. tone, react', 'RMSSD, react', 'HF power, react', 'LF power, react',
                            'Heart rate, recov', 'Systolic BP, recov', 'Diastolic BP, recov', 'Symp. tone, recov', 'RMSSD, recov', 'HF power, recov', 'LF power, recov',
                            'LC_ratio_meta_peak', 'LC_ratio_meta_rostral', 'LC_ratio_meta_caudal')


# load PLS (avg) results --------------------------------------------------

pls_out_all <- readMat(here('data', 'derivatives', 'PLS_all_output_avg.mat'))


# plot PLS results for all phases / caudal LC / LV #1 ---------------------

n_agegr <- pls_out_all$pls.result.all.caudal[,,1]$num.subj.lst
nsubs_YA <- n_agegr[1]
nsubs_OA <- n_agegr[2]
rm(n_agegr)

# extract behavioral data (LC) & U saliences (arousal scores)
salience_all_caudal <- data.frame(
  usc = pls_out_all$pls.result.all.caudal[,,1]$usc[,1],
  behavdata = pls_out_all$pls.result.all.caudal[,,1]$stacked.behavdata[,1],
  age_group = c(rep('Younger', nsubs_YA), rep('Older', nsubs_OA)))

salience_all_caudal$age_group <- factor(salience_all_caudal$age_group, 
                                        levels = c('Younger', 'Older'))

# set labels for facets in plots
# to include number of participants
age_group.labs <- c(paste('Younger', '\n(n = ', nsubs_YA, ')', sep = ''), 
               paste('Older', '\n(n = ', nsubs_OA, ')', sep = ''))
names(age_group.labs) <- c('Younger', 'Older')

# create figure of LC vs. arousal scores 
fig_pls_corr_all_caudal <- ggplot(data = salience_all_caudal,
                                       aes(x = behavdata, y = usc, colour = age_group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'lm', 
              colour = '#666666') +
  stat_cor(cor.coef.name = 'r', 
           r.digits = 2, p.accuracy = 0.001,
           label.y = 0, 
           colour = '#666666') +
  scale_colour_manual(values = palette_agegr_darker) +
  labs(x = 'Caudal LC contrast', 
       y = 'LV #1 physiological score',
       colour = '',
       subtitle = ' ') +
  facet_wrap(~age_group,
             labeller = labeller(age_group = age_group.labs)
  ) +
  theme_apa() +
  theme(legend.position = 'none')

# compute pearson correlations between caudal LC and LC contrast
cor_caudalLC_LVphysio_YA <- cor.test(~ behavdata + usc, data = salience_all_caudal %>%
                                       filter(age_group == 'Younger'))
cor_caudalLC_LVphysio_OA <- cor.test(~ behavdata + usc, data = salience_all_caudal %>%
                                       filter(age_group == 'Older'))

# extract bootstrap ratios for this LV
data_LV_all_caudal <- data.frame(
  variable = names(data_PLS_physio)[3:23],
  boot_ratio = pls_out_all$pls.result.all.caudal[,,1]$boot.result[,,1]$compare.u[,1]
)
data_LV_all_caudal$variable <- factor(data_LV_all_caudal$variable, 
                                    levels = c('Heart rate', 'Breathing rate', 'Systolic BP', 'Diastolic BP', 'Symp. tone', 'RMSSD', 'HF power', 'LF power',
                                               'Heart rate, react', 'Breathing rate, react', 'Systolic BP, react', 'Diastolic BP, react', 'Symp. tone, react', 'RMSSD, react', 'HF power, react', 'LF power, react',
                                               'Heart rate, recov', 'Breathing rate, recov', 'Systolic BP, recov', 'Diastolic BP, recov', 'Symp. tone, recov', 'RMSSD, recov', 'HF power, recov', 'LF power, recov'))

# plot bootstrap ratios
fig_pls_LV_all_caudal <- ggplot(data = data_LV_all_caudal,
                                aes(x = variable, y = boot_ratio)) +
  geom_col() +
  geom_hline(yintercept = 2, colour = 'salmon', linetype = 2) +
  geom_hline(yintercept = -2, colour = 'salmon', linetype = 2) +
  coord_cartesian(ylim = c(-5, 3)) +
  labs(x = '', y = 'Bootstrap ratio',
       subtitle = paste('Latent variable #1 (p=', 
                        round(pls_out_all$pls.result.all.caudal[,,1]$perm.result[,,1]$sprob[1], 3), 
                        ')',
                        sep = '')) +
  theme_apa() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# create composite figure
fig_pls_caudal_avg <- ggarrange(fig_pls_LV_all_caudal, 
          fig_pls_corr_all_caudal, 
          nrow = 1, ncol = 2, widths = c(1.3, 1))


# save figures ------------------------------------------------------------

ggsave(filename = here('figures', 'figure6_pls_avg.svg'),
       plot = fig_pls_caudal_avg,
       device = 'svg',
       bg = 'white',
       width = 13, height = 4,
       dpi = 600)


# create PLS figure (stroop-only version) ---------------------------------

names(data_PLS_physio_strooponly) <- c('label_subject', 'age_group',
                                       'Heart rate', 'Systolic BP', 'Diastolic BP', 'Symp. tone', 'RMSSD', 'HF power', 'LF power',
                                       'Heart rate, react', 'Systolic BP, react', 'Diastolic BP, react', 'Symp. tone, react', 'RMSSD, react', 'HF power, react', 'LF power, react',
                                       'Heart rate, recov', 'Systolic BP, recov', 'Diastolic BP, recov', 'Symp. tone, recov', 'RMSSD, recov', 'HF power, recov', 'LF power, recov',
                                       'LC_ratio_meta_peak', 'LC_ratio_meta_rostral', 'LC_ratio_meta_caudal')

# read in PLS output generated in matlab
pls_out_all_avg_strooponly <- readMat(here('data', 'derivatives', 'PLS_all_output_avg_strooponly.mat'))

# plot PLS results for all phases / caudal LC / LV #1
n_agegr <- pls_out_all_avg_strooponly$pls.result.all.caudal[,,1]$num.subj.lst
nsubs_YA_strooponly <- n_agegr[1]
nsubs_OA_strooponly <- n_agegr[2]
rm(n_agegr)

# extract behavioral data (LC) & U saliences (arousal scores)
salience_all_caudal_strooponly <- data.frame(
  usc = pls_out_all_avg_strooponly$pls.result.all.caudal[,,1]$usc[,1],
  behavdata = pls_out_all_avg_strooponly$pls.result.all.caudal[,,1]$stacked.behavdata[,1],
  age_group = c(rep('Younger', nsubs_YA_strooponly), rep('Older', nsubs_OA_strooponly)))

salience_all_caudal_strooponly$age_group <- factor(salience_all_caudal_strooponly$age_group, 
                                                   levels = c('Younger', 'Older'))

# set labels for facets in plots
# to include number of participants
age_group.labs <- c(paste('Younger', '\n(n = ', nsubs_YA_strooponly, ')', sep = ''), 
                    paste('Older', '\n(n = ', nsubs_OA_strooponly, ')', sep = ''))
names(age_group.labs) <- c('Younger', 'Older')

# create figure of LC vs. arousal scores 
fig_pls_corr_all_caudal_strooponly <- ggplot(data = salience_all_caudal_strooponly,
                                             aes(x = behavdata, y = usc, colour = age_group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'lm', 
              colour = '#666666') +
  stat_cor(cor.coef.name = 'r', 
           r.digits = 2, p.accuracy = 0.001,
           label.y = 0, 
           colour = '#666666') +
  scale_colour_manual(values = palette_agegr_darker) +
  labs(x = 'Caudal LC contrast', 
       y = 'LV #1 physiological score',
       colour = '',
       subtitle = ' ') +
  facet_wrap(~age_group,
             labeller = labeller(age_group = age_group.labs)) +
  theme_apa() +
  theme(legend.position = 'none')

# compute pearson correlations between caudal LC and LC contrast
cor_caudalLC_LVphysio_YA_strooponly <- cor.test(~ behavdata + usc, data = salience_all_caudal_strooponly %>%
                                                  filter(age_group == 'Younger'))
cor_caudalLC_LVphysio_OA_strooponly <- cor.test(~ behavdata + usc, data = salience_all_caudal_strooponly %>%
                                                  filter(age_group == 'Older'))

# extract bootstrap ratios for this LV
data_LV_all_caudal_strooponly <- data.frame(
  variable = names(data_PLS_physio_strooponly)[3:23],
  boot_ratio = pls_out_all_avg_strooponly$pls.result.all.caudal[,,1]$boot.result[,,1]$compare.u[,1]
)
data_LV_all_caudal_strooponly$variable <- factor(data_LV_all_caudal_strooponly$variable, 
                                                 levels = c('Heart rate', 'Systolic BP', 'Diastolic BP', 'Symp. tone', 'RMSSD', 'HF power', 'LF power',
                                                            'Heart rate, react', 'Systolic BP, react', 'Diastolic BP, react', 'Symp. tone, react', 'RMSSD, react', 'HF power, react', 'LF power, react',
                                                            'Heart rate, recov', 'Systolic BP, recov', 'Diastolic BP, recov', 'Symp. tone, recov', 'RMSSD, recov', 'HF power, recov', 'LF power, recov'))

# plot bootstrap ratios
fig_pls_LV_all_caudal_strooponly <- ggplot(data = data_LV_all_caudal_strooponly,
                                           aes(x = variable, y = boot_ratio)) +
  geom_col() +
  geom_hline(yintercept = 2, colour = 'salmon', linetype = 2) +
  geom_hline(yintercept = -2, colour = 'salmon', linetype = 2) +
  coord_cartesian(ylim = c(-5, 3)) +
  labs(x = '', y = 'Bootstrap ratio',
       subtitle = paste('Latent variable #1 (p=', 
                        round(pls_out_all_avg_strooponly$pls.result.all.caudal[,,1]$perm.result[,,1]$sprob[1], 3), 
                        ')',
                        sep = '')) +
  theme_apa() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# create composite figure
fig_pls_caudal_peak_strooponly <- ggarrange(fig_pls_LV_all_caudal_strooponly, 
                                            fig_pls_corr_all_caudal_strooponly, 
                                            nrow = 1, ncol = 2, widths = c(1.3, 1))


ggsave(filename = here('figures', 'figureS5_pls_avg_strooponly.svg'),
       plot = fig_pls_caudal_peak_strooponly,
       device = 'svg',
       bg = 'white',
       width = 13, height = 4,
       dpi = 600)

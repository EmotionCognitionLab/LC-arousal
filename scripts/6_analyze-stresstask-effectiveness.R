# this script analyses stress task data
# to examine effectiveness of the task
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# set factor levels for plotting ------------------------------------------

# make a copy of the data for plotting
data_physio_fig <- data_physio_avg

data_physio_fig$phase <- factor(data_physio_fig$phase, levels = c('Baseline', 'Challenge', 'Recovery'))
data_physio_fig$age_group[data_physio_fig$age_group == 'YA'] <- 'Younger'
data_physio_fig$age_group[data_physio_fig$age_group == 'OA'] <- 'Older'
data_physio_fig$age_group <- factor(data_physio_fig$age_group, levels = c('Younger', 'Older'))


# create figure -----------------------------------------------------------

figure3 <- ggarrange(
  ggplot(data = data_physio_fig,
         aes(x = phase, y = HR_mean,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Heart rate (beats per minute)',
         colour = 'Age group', fill = 'Age group',
         title = 'A. Heart rate') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),

  ggplot(data = data_physio_fig,
         aes(x = phase, y = resp_rate,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Breathing rate (breaths per minute)',
         colour = 'Age group', fill = 'Age group',
         title = 'B. Breathing rate') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),

  ggplot(data = data_physio_fig,
         aes(x = phase, y = BP_systolic,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Systolic blood pressure (mmHg)',
         colour = 'Age group', fill = 'Age group',
         title = 'C. Systolic blood pressure') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  ggplot(data = data_physio_fig,
         aes(x = phase, y = BP_diastolic,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Diastolic blood pressure (mmHg)',
         colour = 'Age group', fill = 'Age group',
         title = 'D. Diastolic blood pressure') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),

  ggplot(data = data_physio_fig,
         aes(x = phase, y = aSKNA_mean,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'aSKNA (microvolts)',
         colour = 'Age group', fill = 'Age group',
         title = 'E. Sympathetic tone') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  ggplot(data = data_physio_fig,
         aes(x = phase, y = log_RMSSD,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'RMSSD (log transformed)',
         colour = 'Age group', fill = 'Age group',
         title = 'F. RMSSD') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  ggplot(data = data_physio_fig,
         aes(x = phase, y = log_hf_lomb,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'HF power (log transformed)',
         colour = 'Age group', fill = 'Age group',
         title = 'G. HF power') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  ggplot(data = data_physio_fig,
         aes(x = phase, y = log_lf_lomb,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'LF power (log transformed)',
         colour = 'Age group', fill = 'Age group',
         title = 'H. LF power') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  

  #nrow = 2, ncol = 4,
  nrow = 4, ncol = 2,

  common.legend = TRUE, legend = 'right'
)

# save figure
ggsave(filename = here('figures', 'figure3_stresstask_effectiveness.svg'),
       plot = figure3,
       device = 'svg',
       bg = 'white',
       dpi = 600,
       #width = 16, height = 9,
       width = 9,
       height = 16)


# create figure (stroop-only version) -------------------------------------

# make a copy of the data for plotting
data_physio_fig_strooponly <- data_physio_avg_strooponly

data_physio_fig_strooponly$phase <- factor(data_physio_fig_strooponly$phase, levels = c('Baseline', 'Challenge', 'Recovery'))
data_physio_fig_strooponly$age_group[data_physio_fig_strooponly$age_group == 'YA'] <- 'Younger'
data_physio_fig_strooponly$age_group[data_physio_fig_strooponly$age_group == 'OA'] <- 'Older'
data_physio_fig_strooponly$age_group <- factor(data_physio_fig_strooponly$age_group, levels = c('Younger', 'Older'))

figure3_strooponly <- ggarrange(
  ggplot(data = data_physio_fig_strooponly,
         aes(x = phase, y = HR_mean,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Heart rate (beats per minute)',
         colour = 'Age group', fill = 'Age group',
         title = 'A. Heart rate') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  ggplot(data = data_physio_fig_strooponly,
         aes(x = phase, y = resp_rate,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Breathing rate (breaths per minute)',
         colour = 'Age group', fill = 'Age group',
         title = 'B. Breathing rate') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  ggplot(data = data_physio_fig_strooponly,
         aes(x = phase, y = BP_systolic,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Systolic blood pressure (mmHg)',
         colour = 'Age group', fill = 'Age group',
         title = 'C. Systolic blood pressure') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  ggplot(data = data_physio_fig_strooponly,
         aes(x = phase, y = BP_diastolic,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Diastolic blood pressure (mmHg)',
         colour = 'Age group', fill = 'Age group',
         title = 'D. Diastolic blood pressure') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  ggplot(data = data_physio_fig_strooponly,
         aes(x = phase, y = aSKNA_mean,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'aSKNA (microvolts)',
         colour = 'Age group', fill = 'Age group',
         title = 'E. Sympathetic tone') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  ggplot(data = data_physio_fig_strooponly,
         aes(x = phase, y = log_RMSSD,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'RMSSD (log transformed)',
         colour = 'Age group', fill = 'Age group',
         title = 'F. RMSSD') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  ggplot(data = data_physio_fig_strooponly,
         aes(x = phase, y = log_hf_lomb,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'HF power (log transformed)',
         colour = 'Age group', fill = 'Age group',
         title = 'G. HF power') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  ggplot(data = data_physio_fig_strooponly,
         aes(x = phase, y = log_lf_lomb,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitter(width = 0.05)) +
    geom_flat_violin(alpha = 0.4,
                     position = position_nudge(x = 0.15),
                     trim = TRUE) +
    stat_summary(aes(group = age_group),
                 geom = 'line',
                 position = position_nudge(x = 0.25)) +
    stat_sum_df('mean_se',
                fill = 'white',
                position = position_nudge(x = 0.25)) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'LF power (log transformed)',
         colour = 'Age group', fill = 'Age group',
         title = 'H. LF power') +
    facet_wrap(~age_group, strip.position = 'top') +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
          plot.title = element_text(face = 'bold'),
          strip.text.x = element_blank()),
  
  
  #nrow = 2, ncol = 4,
  nrow = 4, ncol = 2,
  
  common.legend = TRUE, legend = 'right'
)

# save figure
ggsave(filename = here('figures', 'figureS3_stresstask_effectiveness_strooponly.svg'),
       plot = figure3_strooponly,
       device = 'svg',
       bg = 'white',
       dpi = 600,
       #width = 16, height = 9,
       width = 9,
       height = 16)


# set factor levels for models --------------------------------------------

# (age group: sum coded; phase = sequential coded)
data_physio_mod <- data_physio_avg
data_physio_mod$age_group <- factor(data_physio_mod$age_group, levels = c('YA', 'OA'))
contrasts(data_physio_mod$age_group) <- c(-0.5, 0.5)
data_physio_mod$phase <- factor(data_physio_mod$phase, levels = c('Baseline', 'Challenge', 'Recovery'))
contrasts(data_physio_mod$phase) <- MASS::contr.sdif(3)


# model: heart rate -------------------------------------------------------

# fit model
mod.HR <- lmer(HR_mean ~ phase * age_group + (1 | label_subject), 
               data = data_physio_mod)

# format parameters table
params.HR <- summary(mod.HR)$coefficients %>%
  as.data.frame() %>%
  select(Coefficient = Estimate, SE = `Std. Error`, df = df, t = `t value`, p = `Pr(>|t|)`)
params.HR$Parameter <- params.HR %>% rownames()

# calculate effect size (partial r)
r.HR <- t_to_r(t = params.HR$t, df_error = params.HR$df)


# pairwise comparisons: heart rate ----------------------------------------

# pairwise comparison of measure for each phase contrast and age group
emm.HR <- emmeans(mod.HR, c('phase', 'age_group'))
pairwise.HR <- pairs(emm.HR, 
                     reverse = TRUE,   # reverse comparisons so they return contrasts of interest
                     by = 'age_group') %>% # return comparisons for each age group
                    # adjust p-values based on ALL comparisons (see https://rdrr.io/cran/emmeans/f/vignettes/FAQs.Rmd#what)
                     summary(by = NULL, 
                             adjust = 'bonferroni') %>%
  as.data.frame()

# format as table and calculate effect size (partial r)
pairwise.HR <- format_pairwise_result(pairwise.HR)


# model: breathing rate ---------------------------------------------------

# fit model
mod.resp <- lmer(resp_rate ~ phase * age_group + (1 | label_subject), 
               data = data_physio_mod)

# format paramters table
params.resp <- summary(mod.resp)$coefficients %>%
  as.data.frame() %>%
  select(Coefficient = Estimate, SE = `Std. Error`, df = df, t = `t value`, p = `Pr(>|t|)`)
params.resp$Parameter <- params.resp %>% rownames()

# calculate effect size (partial r)
r.resp <- t_to_r(t = params.resp$t, df_error = params.resp$df)


# pairwise comparisons: breathing rate ------------------------------------

# pairwise comparison of measure for each phase contrast and age group
emm.resp <- emmeans(mod.resp, c('phase', 'age_group'))
pairwise.resp <- pairs(emm.resp, 
                       reverse = TRUE,   # reverse comparisons so they return contrasts of interest
                       by = 'age_group') %>% # return comparisons for each age group
  # adjust p-values based on ALL comparisons (see https://rdrr.io/cran/emmeans/f/vignettes/FAQs.Rmd#what)
  summary(by = NULL, 
          adjust = 'bonferroni') %>%
  as.data.frame()

# format as table and calculate effect size (partial r)
pairwise.resp <- format_pairwise_result(pairwise.resp)


# model: systolic BP ------------------------------------------------------

# fit model
mod.sysBP <- lmer(BP_systolic ~ phase * age_group + (1 | label_subject), 
               data = data_physio_mod)

# format parameters table
params.sysBP <- summary(mod.sysBP)$coefficients %>%
  as.data.frame() %>%
  select(Coefficient = Estimate, SE = `Std. Error`, df = df, t = `t value`, p = `Pr(>|t|)`)
params.sysBP$Parameter <- params.sysBP %>% rownames()

# calculate effect size (partial r)
r.sysBP <- t_to_r(t = params.sysBP$t, df_error = params.sysBP$df)


# pairwise comparisons: systolic BP ---------------------------------------

# pairwise comparison of measure for each phase contrast and age group
emm.sysBP <- emmeans(mod.sysBP, c('phase', 'age_group')) 
pairwise.sysBP <- pairs(emm.sysBP,
                        reverse = TRUE,   # reverse comparisons so they return contrasts of interest
                        by = 'age_group') %>% # return comparisons for each age group
  # adjust p-values based on ALL comparisons (see https://rdrr.io/cran/emmeans/f/vignettes/FAQs.Rmd#what)
  summary(by = NULL, 
          adjust = 'bonferroni') %>%
  as.data.frame()

# format as table and calculate effect size (partial r)
pairwise.sysBP <- format_pairwise_result(pairwise.sysBP)


# model: diastolic BP -----------------------------------------------------

# fit model
mod.diaBP <- lmer(BP_diastolic ~ phase * age_group + (1 | label_subject), 
               data = data_physio_mod)

# format parameters table
params.diaBP <- summary(mod.diaBP)$coefficients %>%
  as.data.frame() %>%
  select(Coefficient = Estimate, SE = `Std. Error`, df = df, t = `t value`, p = `Pr(>|t|)`)
params.diaBP$Parameter <- params.diaBP %>% rownames()

# calculate effect size (partial r)
r.diaBP <- t_to_r(t = params.diaBP$t, df_error = params.diaBP$df)


# pairwise comparisons: systolic BP ---------------------------------------

# pairwise comparison of measure for each phase contrast and age group
emm.diaBP <- emmeans(mod.diaBP, c('phase', 'age_group')) 
pairwise.diaBP <- pairs(emm.diaBP,
                        reverse = TRUE,   # reverse comparisons so they return contrasts of interest
                        by = 'age_group') %>% # return comparisons for each age group
  # adjust p-values based on ALL comparisons (see https://rdrr.io/cran/emmeans/f/vignettes/FAQs.Rmd#what)
  summary(by = NULL, 
          adjust = 'bonferroni') %>%
  as.data.frame()

# format as table and calculate effect size (partial r)
pairwise.diaBP <- format_pairwise_result(pairwise.diaBP)


# model: aSKNA ------------------------------------------------------------

# fit model
mod.aSKNA <- lmer(aSKNA_mean ~ phase * age_group + (1 | label_subject), 
               data = data_physio_mod)

# format parameters table
params.aSKNA <- summary(mod.aSKNA)$coefficients %>%
  as.data.frame() %>%
  select(Coefficient = Estimate, SE = `Std. Error`, df = df, t = `t value`, p = `Pr(>|t|)`)
params.aSKNA$Parameter <- params.aSKNA %>% rownames()

# for aSKNA specifically, format coefficients and SEs
# in scientific notion and 3 digits
params.aSKNA$Coefficient <- sprintf("%.3e", params.aSKNA$Coefficient)
params.aSKNA$SE <- sprintf("%.3e", params.aSKNA$SE)

# calculate effect size (partial r)
r.aSKNA <- t_to_r(t = params.aSKNA$t, df_error = params.aSKNA$df)


# pairwise comparisons: aSKNA ---------------------------------------------

# pairwise comparison of measure for each phase contrast and age group
emm.aSKNA <- emmeans(mod.aSKNA, c('phase', 'age_group')) 
pairwise.aSKNA <- pairs(emm.aSKNA,
                        reverse = TRUE,   # reverse comparisons so they return contrasts of interest
                        by = 'age_group') %>% # return comparisons for each age group
  # adjust p-values based on ALL comparisons (see https://rdrr.io/cran/emmeans/f/vignettes/FAQs.Rmd#what)
  summary(by = NULL, 
          adjust = 'bonferroni') %>%
  as.data.frame()

# for aSKNA specifically, format estimates and SEs
# in scientific notion and 3 digits
pairwise.aSKNA$estimate <- sprintf("%.3e", pairwise.aSKNA$estimate)
pairwise.aSKNA$SE <- sprintf("%.3e", pairwise.aSKNA$SE)

# format as table and calculate effect size (partial r)
pairwise.aSKNA <- format_pairwise_result(pairwise.aSKNA)


# model: RMSSD ------------------------------------------------------------

# fit model
mod.RMSSD <- lmer(log_RMSSD ~ phase * age_group + (1 | label_subject), 
               data = data_physio_mod)

# format parameters table
params.RMSSD <- summary(mod.RMSSD)$coefficients %>%
  as.data.frame() %>%
  select(Coefficient = Estimate, SE = `Std. Error`, df = df, t = `t value`, p = `Pr(>|t|)`)
params.RMSSD$Parameter <- params.RMSSD %>% rownames()

# calculate effect size (partial r)
r.RMSSD <- t_to_r(t = params.RMSSD$t, df_error = params.RMSSD$df)


# pairwise comparisons: RMSSD ---------------------------------------------

# pairwise comparison of measure for each phase contrast and age group
emm.RMSSD <- emmeans(mod.RMSSD, c('phase', 'age_group')) 
pairwise.RMSSD <- pairs(emm.RMSSD,
                        reverse = TRUE,   # reverse comparisons so they return contrasts of interest
                        by = 'age_group') %>% # return comparisons for each age group
  # adjust p-values based on ALL comparisons (see https://rdrr.io/cran/emmeans/f/vignettes/FAQs.Rmd#what)
  summary(by = NULL, 
          adjust = 'bonferroni') %>%
  as.data.frame()

# format as table and calculate effect size (partial r)
pairwise.RMSSD <- format_pairwise_result(pairwise.RMSSD)


# model: HF power ---------------------------------------------------------

# fit model
mod.HF <- lmer(log_hf_lomb ~ phase * age_group + (1 | label_subject), 
                  data = data_physio_mod)

# format parameters table
params.HF <- summary(mod.HF)$coefficients %>%
  as.data.frame() %>%
  select(Coefficient = Estimate, SE = `Std. Error`, df = df, t = `t value`, p = `Pr(>|t|)`)
params.HF$Parameter <- params.HF %>% rownames()

# calculate effect size (partial r)
r.HF <- t_to_r(t = params.HF$t, df_error = params.HF$df)


# pairwise comparisons: HF power ------------------------------------------

# pairwise comparison of measure for each phase contrast and age group
emm.HF <- emmeans(mod.HF, c('phase', 'age_group'))
pairwise.HF <- pairs(emm.HF, 
                     reverse = TRUE,   # reverse comparisons so they return contrasts of interest
                     by = 'age_group') %>% # return comparisons for each age group
  # adjust p-values based on ALL comparisons (see https://rdrr.io/cran/emmeans/f/vignettes/FAQs.Rmd#what)
  summary(by = NULL, 
          adjust = 'bonferroni') %>%
  as.data.frame()

# format as table and calculate effect size (partial r)
pairwise.HF <- format_pairwise_result(pairwise.HF)


# model: LF power ---------------------------------------------------------

# fit model
mod.LF <- lmer(log_lf_lomb ~ phase * age_group + (1 | label_subject), 
               data = data_physio_mod)

# format parameters table
params.LF <- summary(mod.LF)$coefficients %>%
  as.data.frame() %>%
  select(Coefficient = Estimate, SE = `Std. Error`, df = df, t = `t value`, p = `Pr(>|t|)`)
params.LF$Parameter <- params.LF %>% rownames()

# calculate effect size (partial r)
r.LF <- t_to_r(t = params.LF$t, df_error = params.LF$df)


# pairwise comparisons: LF power ------------------------------------------

# pairwise comparison of measure for each phase contrast and age group
emm.LF <- emmeans(mod.LF, c('phase', 'age_group'))
pairwise.LF <- pairs(emm.LF, 
                     reverse = TRUE,   # reverse comparisons so they return contrasts of interest
                     by = 'age_group') %>% # return comparisons for each age group
  # adjust p-values based on ALL comparisons (see https://rdrr.io/cran/emmeans/f/vignettes/FAQs.Rmd#what)
  summary(by = NULL, 
          adjust = 'bonferroni') %>%
  as.data.frame()

# format as table and calculate effect size (partial r)
pairwise.LF <- format_pairwise_result(pairwise.LF)


# analysis of PASAT performance -------------------------------------------

data_pasat <- data_pasat %>%
  # exclude older participants who completed earlier version of stress task
  filter(!label_subject %in% subs_OA_oldtask) %>%
  select(label_subject, pasat_acc_mean, pasat_RT_mean) %>%
  left_join(data_LCA %>% select(label_subject, age_group),
            by = c('label_subject'))

# summarize mean/SD accuracy and reaction time in table
summary_pasat <- data_pasat %>%
  group_by(age_group) %>%
  summarize(mean_acc = mean(pasat_acc_mean)*100,  # convert to % for reporting
            sd_acc = sd(pasat_acc_mean)*100,
            mean_rt = mean(pasat_RT_mean)/1000, # convert to sec for reporting
            sd_rt = sd(pasat_RT_mean)/1000)


# analysis of Stroop performance ------------------------------------------

data_stroop <- data_stroop %>%
  # exclude older participants who completed earlier version of stress task
  filter(!label_subject %in% subs_OA_oldtask) %>%
  select(label_subject, stroop_acc_mean, stroop_RT_mean) %>%
  left_join(data_LCA %>% select(label_subject, age_group),
            by = c('label_subject'))

# summarize mean/SD accuracy and reaction time in table
summary_stroop <- data_stroop %>%
  group_by(age_group) %>%
  summarize(mean_acc = mean(stroop_acc_mean)*100,  # convert to % for reporting
            sd_acc = sd(stroop_acc_mean)*100,
            mean_rt = mean(stroop_RT_mean)/1000, # convert to sec for reporting
            sd_rt = sd(stroop_RT_mean)/1000)

# perform Welch's t-tests to examine by age group
# (label age group so that younger adults are first)
data_stroop$age_group <- factor(data_stroop$age_group, levels = c('YA', 'OA'))
ttest_stroop_acc <- t.test(stroop_acc_mean ~ age_group, data = data_stroop)
ttest_stroop_rt <- t.test(stroop_RT_mean ~ age_group, data = data_stroop)

# compute relevant effect sizes
r_stroop_acc <- t_to_r(ttest_stroop_acc$statistic, ttest_stroop_acc$parameter)
r_stroop_rt <- t_to_r(ttest_stroop_rt$statistic, ttest_stroop_rt$parameter)


# figure: PASAT & Stroop performance --------------------------------------

# make a copy of data for plotting
data_stroop_fig <- data_stroop
data_pasat_fig <- data_pasat

# add dummy data for OA to pasat
# NOTE: this is only done for spacing purposes
# the dummy data gets removed for the manuscript version
data_pasat_fig <- data_pasat_fig %>%
  add_row(label_subject = 'sub-7001', age_group = 'OA', pasat_acc_mean = 0.84, pasat_RT_mean = 878) %>%
  add_row(label_subject = 'sub-7003', age_group = 'OA', pasat_acc_mean = 0.81, pasat_RT_mean = 900) %>%
  add_row(label_subject = 'sub-7004', age_group = 'OA', pasat_acc_mean = 0.87, pasat_RT_mean = 890)

# rename age group factor for figures
data_pasat_fig$age_group <- as.character(data_pasat_fig$age_group)
data_pasat_fig$age_group[data_pasat_fig$age_group == 'YA'] <- 'Younger'
data_pasat_fig$age_group[data_pasat_fig$age_group == 'OA'] <- 'Older'
data_pasat_fig$age_group <- factor(data_pasat_fig$age_group, levels = c('Younger', 'Older'))
data_stroop_fig$age_group <- as.character(data_stroop_fig$age_group)
data_stroop_fig$age_group[data_stroop_fig$age_group == 'YA'] <- 'Younger'
data_stroop_fig$age_group[data_stroop_fig$age_group == 'OA'] <- 'Older'
data_stroop_fig$age_group <- factor(data_stroop_fig$age_group, levels = c('Younger', 'Older'))

# create figure
fig_pasat <- ggarrange(
  ggplot(data = data_pasat_fig,
         aes(x = age_group, y = pasat_acc_mean,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
              size = 1,
              alpha = 0.4,
              colour = 'black',
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)) +
    geom_flat_violin(alpha = 0.7,
                     position = position_nudge(x = 0.2),
                     width = 1) +
    geom_boxplot(position = position_nudge(x = 0.12), 
                 width = 0.1, 
                 colour = 'black', 
                 outlier.shape = NA) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Mean accuracy (%)',
         fill = 'Age group', colour = 'Age group',
         title = 'A. PASAT',
         subtitle = 'Accuracy') +
    coord_cartesian(ylim = c(0.2, 1.0)) +
    theme_apa() +
    theme(plot.title = element_text(face = 'bold'),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = 'right'),
  
  ggplot(data = data_pasat_fig,
         aes(x = age_group, y = pasat_RT_mean,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)) +
    geom_flat_violin(alpha = 0.7,
                     position = position_nudge(x = 0.2),
                     width = 1) +
    geom_boxplot(position = position_nudge(x = 0.12), 
                 width = 0.1, 
                 colour = 'black', 
                 outlier.shape = NA) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Mean reaction time\n(milliseconds)',
         fill = 'Age group', colour = 'Age group',
         title = ' ',
         subtitle = 'Reaction time') +
    coord_cartesian(ylim = c(200, 1800)) +
    theme_apa() +
    theme(plot.title = element_text(face = 'bold'),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = 'right'),
  
  nrow = 1, ncol = 2,
  common.legend = TRUE, legend = 'none'
  
)

fig_stroop <- ggarrange(
  ggplot(data = data_stroop_fig,
         aes(x = age_group, y = stroop_acc_mean,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)) +
    geom_flat_violin(alpha = 0.7,
                     position = position_nudge(x = 0.2),
                     width = 0.7) +
    geom_boxplot(position = position_nudge(x = 0.12), 
                 width = 0.1, 
                 colour = 'black', 
                 outlier.shape = NA) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Mean accuracy (%)',
         fill = 'Age group', colour = 'Age group',
         title = 'B. Stroop task',
         subtitle = 'Accuracy') +
    coord_cartesian(ylim = c(0.2, 1.0)) +
    theme_apa() +
    theme(plot.title = element_text(face = 'bold'),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = 'right'),
  
  ggplot(data = data_stroop_fig,
         aes(x = age_group, y = stroop_RT_mean,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)) +
    geom_flat_violin(alpha = 0.7,
                     position = position_nudge(x = 0.2),
                     width = 0.7) +
    geom_boxplot(position = position_nudge(x = 0.12), 
                 width = 0.1, 
                 colour = 'black', 
                 outlier.shape = NA) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(x = '', y = 'Mean reaction time\n(milliseconds)',
         fill = 'Age group', colour = 'Age group',
         title = ' ',
         subtitle = 'Reaction time') +
    coord_cartesian(ylim = c(200, 1800)) +
    theme_apa() +
    theme(plot.title = element_text(face = 'bold'),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = 'right'),
  
  nrow = 1, ncol = 2,
  common.legend = TRUE, legend = 'none'
  
)

# extract figure legend
legend_tasks <- get_legend(
  ggplot(data = data_pasat_fig,
         aes(x = age_group, y = pasat_acc_mean,
             colour = age_group, fill = age_group)) +
    geom_point(pch = 21,
               size = 1,
               alpha = 0.4,
               colour = 'black',
               position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)) +
    geom_flat_violin(alpha = 0.7,
                     position = position_nudge(x = 0.2),
                     width = 1) +
    geom_boxplot(position = position_nudge(x = 0.12), 
                 width = 0.1, 
                 colour = 'black', 
                 outlier.shape = NA) +
    scale_colour_manual(values = palette_agegr) +
    scale_fill_manual(values = palette_agegr) +
    labs(colour = 'Age group', fill = 'Age group')
)

# arrange composite figure and save
fig_challenge_tasks <- ggarrange(
  ggarrange(
    
    fig_pasat,
    fig_stroop,
    
    nrow = 2, ncol = 1
  ),
  legend_tasks,
  
  nrow = 1, ncol = 2,
  widths = c(10, 1)
)
  
# don't forget to remove PASAT points for OA in the figure in Inkscape, etc.
ggsave(filename = here('figures', 'figureS2_challenge_tasks.svg'),
       plot = fig_challenge_tasks, device = 'svg',
       bg = 'white',
       dpi = 600, 
       width = 12, height = 9)

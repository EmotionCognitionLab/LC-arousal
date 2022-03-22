# this script analyzes LC contrast in the sample
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# remove outliers for LC ratios -------------------------------------------

### YA
data_LC_ratios_YA <- data_LC_ratios %>%
  filter(age_group == 'YA')
summary_rmout_LC_YA <- sapply(data_LC_ratios_YA[,c(2, 6, 7)], summary_outliers)
summary_rmout_LC_YA <- data.frame(variable = names(summary_rmout_LC_YA),
                                  n_outliers = as.numeric(summary_rmout_LC_YA))
data_LC_ratios_YA[,c(2, 6, 7)] <- purrr::map_df(data_LC_ratios_YA[,c(2, 6, 7)], madmedianrule)


### OA
data_LC_ratios_OA <- data_LC_ratios %>%
  filter(age_group == 'OA')
summary_rmout_LC_OA <- sapply(data_LC_ratios_OA[,c(2, 6, 7)], summary_outliers)
summary_rmout_LC_OA <- data.frame(variable = names(summary_rmout_LC_OA),
                               n_outliers = as.numeric(summary_rmout_LC_OA))
data_LC_ratios_OA[,c(2, 6, 7)] <- purrr::map_df(data_LC_ratios_OA[,c(2, 6, 7)], madmedianrule)


### join data for YA and OA
data_LC_ratios <- as.data.frame(
  rbind(data_LC_ratios_YA,
        data_LC_ratios_OA)
)
summary_rmout_LC <- as.data.frame(
  rbind(summary_rmout_LC_YA %>% 
          mutate(age_group = 'YA', .before = variable),
        summary_rmout_LC_OA %>% 
          mutate(age_group = 'OA', .before = variable))
)


# Welch's t-test: LC ~ age group ------------------------------------------

ttest.LC_agegr <- t.test(LC_ratio_meta_peak ~ age_group, 
                   alternative = 'two.sided',
                   paired = FALSE, 
                   var.equal = FALSE, 
                   data = data_LC_ratios)

# effect size as r
r.LC_agegr <- t_to_r(t = ttest.LC_agegr$statistic, 
                     df_error = ttest.LC_agegr$parameter)


# ANOVA: LC ~ age group x topography --------------------------------------

data_LC_ratios_mod <- data_LC_ratios %>%
  select(label_subject, age_group, 
         LC_ratio_meta_rostral, LC_ratio_meta_caudal) %>%
  pivot_longer(cols = LC_ratio_meta_rostral:LC_ratio_meta_caudal, 
               names_to = 'topo',
               names_prefix = 'LC_ratio_meta_',
               values_to = 'LC_ratio')

aov.LC_topo <- aov_ez(id = 'label_subject', 
                 dv = 'LC_ratio', 
                 between = 'age_group', 
                 within = 'topo',
                data = data_LC_ratios_mod, 
                type = 2)

# format anova results
aovtable.LC_topo <- format_afex_aov_results(aov.LC_topo)

# effect sizes as r
r.LC_topo <- F_to_r(f = aov.LC_topo$anova_table$`F`, 
                    df = aov.LC_topo$anova_table$`num Df`,
                    df_error = aov.LC_topo$anova_table$`den Df`)
r.LC_topo$Effect <- aovtable.LC_topo$Effect


# set factor levels for plotting ------------------------------------------

# make a copy of the data
data_LC_ratios_fig <- data_LC_ratios %>%
  select(label_subject, age_group, gender, 
         LC_ratio_meta_peak,
         LC_ratio_meta_peak_left, LC_ratio_meta_peak_right, 
         LC_ratio_meta_rostral, LC_ratio_meta_caudal)

# set factor levels
data_LC_ratios_fig$age_group[data_LC_ratios_fig$age_group == 'YA'] <- 'Younger'
data_LC_ratios_fig$age_group[data_LC_ratios_fig$age_group == 'OA'] <- 'Older'
data_LC_ratios_fig$age_group <- factor(data_LC_ratios_fig$age_group, levels = c('Younger', 'Older'))

# reshape data for figure
data_LC_ratios_fig <- data_LC_ratios_fig %>%
  select(label_subject, age_group, 
         LC_ratio_meta_peak, LC_ratio_meta_rostral, LC_ratio_meta_caudal) %>%
  pivot_longer(cols = LC_ratio_meta_peak:LC_ratio_meta_caudal,
               names_to = 'topo', 
               names_prefix = 'LC_ratio_meta_', 
               values_to = 'LC_ratio')

# set topo factor for figure
data_LC_ratios_fig <- data_LC_ratios_fig %>%
  rowwise() %>%
  # make topo title case & add " LC" at end
  mutate(topo = str_c(str_to_title(topo), ' LC', sep = ''))
data_LC_ratios_fig$topo <- factor(data_LC_ratios_fig$topo, levels = c('Peak LC', 'Rostral LC', 'Caudal LC'))


# create figure -----------------------------------------------------------

figure4 <- ggplot(data = data_LC_ratios_fig,
       aes(x = age_group, y = LC_ratio,
           colour = age_group, fill = age_group)) +
  geom_point(pch = 21,
             size = 1,
             alpha = 0.4,
             colour = 'black',
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)) +
  geom_flat_violin(alpha = 0.7,
                   position = position_nudge(x = 0.1),
                   width = 0.7) +
  stat_sum_df('mean_se',
              fill = 'white',
              position = position_nudge(x = 0.2)) +
  scale_colour_manual(values = palette_agegr) +
  scale_fill_manual(values = palette_agegr) +
  labs(x = '', y = '',
       fill = 'Age group', colour = 'Age group') +
  coord_cartesian(ylim = c(-0.07, 0.29)) +
  facet_wrap(~topo) +
  theme_apa() +
  theme(plot.title = element_text(face = 'bold'),
        strip.text = element_text(face = 'bold'),
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'right')

# save figure
ggsave(here('figures', 'figure4_LC_ratios.svg'),
       plot = figure4,
       device = 'svg',
       dpi = 600,
       bg = 'white',
       width = 9, height = 4)

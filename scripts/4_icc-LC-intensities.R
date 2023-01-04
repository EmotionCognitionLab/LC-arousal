# this script performs ICC analyses
# reflecting correspondence between peak LC intensities
# determined by each rater using the manual delineation method
# and determine by each method (semiauto vs. manual)
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# perform ICC analyses between raters -------------------------------------

# (because raters rate the same scans, these ICC analyses use type = agreement)

# left LC
icc_raters_leftLC <- irr::icc(data_LC_manual %>%
                                select(LC_left_JL, LC_left_SA), 
                              type = "agreement",
                              model = "twoway",
                              unit = "single")

# right LC
icc_raters_rightLC <- irr::icc(data_LC_manual %>% 
                                 select(LC_right_JL, LC_right_SA), 
                               type = "agreement",
                               model = "twoway",
                               unit = "single")


# perform ICC analyses between methods ------------------------------------

# (because different scans are assessed with each method,
# e.g. manual uses native scans for LC intensity, semi-auto uses warped FSE scans for LC intensity
# these ICC analyses use type = consistency)

# (another note: since we previously saw high correspondence of intensity values
# across raters, for this step we average manual intensity ratings across raters)
data_ICC <- left_join(data_LC_semiauto %>%
                        group_by(label_subject) %>%
                        summarize(
                          # this step takes peak intensities within meta LC map (across all z-slices)
                          LC_L_intensity_peak_meta = max(LC_L_meta_max, na.rm = TRUE),
                          LC_R_intensity_peak_meta = max(LC_R_meta_max, na.rm = TRUE),
                        ),
                      data_LC_manual %>% select(label_subject, 
                                                LC_left_manual_avg, LC_right_manual_avg),
                      by = c('label_subject'))


# perform ICC analysis between methods, left LC ---------------------------

icc_methods_leftLC <- irr::icc(data_ICC %>% 
                                 ungroup() %>% 
                                 select(LC_L_intensity_peak_meta, LC_left_manual_avg) %>% na.omit,
                               type = 'consistency',
                               model = 'twoway',
                               unit = 'single')


# perform ICC analysis between methods, right LC --------------------------

icc_methods_rightLC <- irr::icc(data_ICC %>% 
                                  ungroup() %>% 
                                  select(LC_R_intensity_peak_meta, LC_right_manual_avg) %>% na.omit,
                                type = 'consistency',
                                model = 'twoway',
                                unit = 'single')

rm(data_ICC)

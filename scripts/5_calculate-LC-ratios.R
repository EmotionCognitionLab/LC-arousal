# this script computes LC ratios
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# for each hemi and z-slice, compute LC ratio -----------------------------

LC_ratios_byslice <- data_LC_semiauto %>%
  select(label_subject, z, LC_L_meta_max, LC_R_meta_max, ref_meta_max) %>%
  rowwise() %>%
  # for each hemisphere and z-slice, compute LC ratio
  mutate(LC_ratio_meta_left = (LC_L_meta_max - ref_meta_max) / ref_meta_max,
         LC_ratio_meta_right = (LC_R_meta_max - ref_meta_max) / ref_meta_max) %>%
  # average LC ratios over hemispheres
  mutate(LC_ratio_meta = (LC_ratio_meta_left + LC_ratio_meta_right) / 2)


# compute peak ratio across z-slices --------------------------------------

LC_ratios_peak <- LC_ratios_byslice %>%
  group_by(label_subject) %>%
  summarize(
    # identify peak ratio across all z-slices, from hemi-averaged ratios
    # na.rm is true because meta-mask has different z-range across hemis
    LC_ratio_meta_peak = max(LC_ratio_meta, na.rm = TRUE),
    LC_ratio_meta_peak_z = z[which.max(LC_ratio_meta)],
    
    # identify peak ratio across all z-slices, for left hemi
    LC_ratio_meta_peak_left = max(LC_ratio_meta_left, na.rm = TRUE),
    
    # identify peak ratio across all z-slices, for right hemi
    LC_ratio_meta_peak_right = max(LC_ratio_meta_right, na.rm = TRUE),
  )


# set caudal/rostral coordinates ------------------------------------------

# current range of z-slices
slices_z <- range(data_LC_semiauto$z)

# caudal cluster
# based on percentiles from Bachman et al., 2021
#100-(100/17)*(c(1,6))
start_caudal <- 1-0.941 # caudal end
end_caudal <- 1-0.647 # caudal --> rostral
slices_caudal <- as.numeric(round(quantile(slices_z, c(start_caudal, end_caudal))))
slices_caudal_range <- slices_caudal[1]:slices_caudal[2]

# rostral cluster
# based on percentiles from Bachman et al., 2021
#100-(100/17)*c(10,12)
start_rostral <- 1-0.412 # caudal --> rostral
end_rostral <- 1-0.294 # rostral end
slices_rostral <- as.numeric(round(quantile(slices_z, c(start_rostral, end_rostral))))
slices_rostral_range <- slices_rostral[1]:slices_rostral[2]


# compute caudal LC ratios ------------------------------------------------

LC_ratios_caudal <- LC_ratios_byslice %>%
  filter(z %in% slices_caudal_range) %>%
  group_by(label_subject) %>%
  summarize(
    LC_ratio_meta_caudal = mean(LC_ratio_meta, na.rm = TRUE)
  )


# compute rostral LC ratios -----------------------------------------------

LC_ratios_rostral <- LC_ratios_byslice %>%
  filter(z %in% slices_rostral_range) %>%
  group_by(label_subject) %>%
  summarize(
    LC_ratio_meta_rostral = mean(LC_ratio_meta, na.rm = TRUE)
  )


# join peak, rostral and caudal ratios ------------------------------------

data_LC_ratios <- LC_ratios_peak %>%
  left_join(LC_ratios_rostral, 
            by = 'label_subject') %>%
  left_join(LC_ratios_caudal,
            by = 'label_subject')


# bind demographic data to LC ratios --------------------------------------

data_LC_ratios <- data_LC_ratios %>%
  left_join(data_LCA %>% 
              select(label_subject, age_group, gender), by = 'label_subject')


# remove unneeded variables -----------------------------------------------

rm(LC_ratios_peak)
rm(LC_ratios_caudal)
rm(LC_ratios_rostral)
rm(slices_z, start_caudal, end_caudal, slices_caudal,
   start_rostral, end_rostral, slices_rostral)


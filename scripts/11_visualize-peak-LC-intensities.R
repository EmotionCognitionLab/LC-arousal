# this script finds and plots locations of peak LC intensity
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# for each hemi and z-slice, compute LC ratio -----------------------------

locs_peak_LC <- as.data.frame(
  rbind(
    data_LC_semiauto %>%
      select(label_subject, z, intensity = LC_L_meta_max,
             x = LC_L_meta_max_x, y = LC_L_meta_max_y) %>%
      mutate(hemi = 'left'),
    data_LC_semiauto %>%
      select(label_subject, z, intensity = LC_R_meta_max,
             x = LC_R_meta_max_x, y = LC_R_meta_max_y) %>%
      mutate(hemi = 'right')
  )
) %>%
  left_join(
    data_LCA %>% 
      select(label_subject, age_group), by = 'label_subject'
  )

locs_peak_LC$age_group[locs_peak_LC$age_group == 'YA'] <- 'Younger'
locs_peak_LC$age_group[locs_peak_LC$age_group == 'OA'] <- 'Older'
locs_peak_LC$age_group <- factor(locs_peak_LC$age_group, levels = c('Younger', 'Older'))


# find peak location across slices ----------------------------------------

locs_peak_LC_rostrocaudal <- locs_peak_LC %>%
  group_by(label_subject, age_group, hemi) %>%
  summarize(
    # identify peak ratio across all z-slices, from hemi-averaged ratios
    # na.rm is true because meta-mask has different z-range across hemis
    LC_peak_x = x[which.max(intensity)],
    LC_peak_y = y[which.max(intensity)],
    LC_peak_z = z[which.max(intensity)]
  ) %>%
  select(label_subject, age_group, hemi, x=LC_peak_x, y=LC_peak_y, z=LC_peak_z)


# set rostral and caudal cluster locations --------------------------------

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


# find peak location in each z-slice, rostral cluster ---------------------

locs_peak_LC_rostral <- locs_peak_LC %>%
  filter(z %in% slices_rostral_range) %>%
  mutate(topo = 'rostral') %>%
  select(label_subject, age_group, hemi, topo, x, y, z)


# find peak location in each z-slice, caudal cluster ---------------------

locs_peak_LC_caudal <- locs_peak_LC %>%
  filter(z %in% slices_caudal_range) %>%
  mutate(topo = 'caudal') %>%
  select(label_subject, age_group, hemi, topo, x, y, z)


# join rostral and caudal data --------------------------------------------

locs_peak_LC_topo <- as.data.frame(
  rbind(
    locs_peak_LC_rostral,
    locs_peak_LC_caudal
  )
)


# remove irrelevant variables ---------------------------------------------

rm(slices_z, start_caudal, end_caudal, slices_caudal,
   start_rostral, end_rostral, slices_rostral,
   locs_peak_LC_rostral, locs_peak_LC_caudal)


# plot peak locations along rostrocaudal axis -----------------------------

library(gg3D)

# make x-coordinates negative to show left on left, right on right
locs_peak_LC_rostrocaudal <- locs_peak_LC_rostrocaudal %>%
  mutate(x = x*-1)

# 3D scatterplot settings for rotation, tilt
theta <- 10
phi <- 20

# code to set x, y, z axis limits
fix_bounds_3D <- axis_labs_3D(theta = theta, phi = phi,
                              inherit.aes = FALSE,
                              data=data.frame(
                                x=c(min(locs_peak_LC_rostrocaudal$x), max(locs_peak_LC_rostrocaudal$x)),
                                y=c(min(locs_peak_LC_rostrocaudal$y), max(locs_peak_LC_rostrocaudal$y)),
                                z=c(min(locs_peak_LC_rostrocaudal$z), max(locs_peak_LC_rostrocaudal$z))),
                              aes(x = x, y = y, z = z),
                              geom = 'text', size=3, 
                              hjust=c(-1,-1,1.2,1.2,-1.2,-1.2), 
                              vjust=c(-0.5,-0.5,-.2,-.2,0.2,0.2))

# create 3D scatterplot for each age group & timepoint
fig_peak_LC <- ggplot(data = locs_peak_LC_rostrocaudal,
       aes(x = x, y = y, z = z, 
           colour = age_group)) +
  axes_3D(theta = theta, phi = phi) +
  stat_3D(theta = theta, phi = phi, alpha = 0.6) +
  labs_3D(theta = theta, phi = phi, size = 5,
          labs=c("x", "y", "z"),
          hjust=c(0,1,1), vjust=c(0.8, 1.2, -0.2), angle=c(0, 0, 0)) +
  fix_bounds_3D +
  scale_colour_manual(values = palette_agegr) +
  labs(colour = '',
       title = '',
       subtitle = '') +
  theme_void() +
  theme(text = element_text(size = 14, family = 'Arial'),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'none')



# visualize distribution of peak LC location z-coords ---------------------

locs_peak_LC_rostrocaudal$hemi[locs_peak_LC_rostrocaudal$hemi == 'left'] <- 'Left LC'
locs_peak_LC_rostrocaudal$hemi[locs_peak_LC_rostrocaudal$hemi == 'right'] <- 'Right LC'
fig_hist_peak_z <- ggplot(data = locs_peak_LC_rostrocaudal,
       aes(x = z, fill = age_group)) +
  geom_histogram(position = 'identity', bins = 28, alpha = 0.4) +
  scale_fill_manual(values = palette_agegr) +
  facet_wrap(~age_group*hemi, nrow = 2, ncol = 2) +
  labs(y = 'Count', 
       x = 'z-coordinate (caudal to rostral)', 
       fill = '') +
  coord_flip() +
  theme_apa()


figureS1 <- ggarrange(
  fig_peak_LC, fig_hist_peak_z,
  labels = c('A', 'B')
)

ggsave(filename = here('figures', 'figureS1_LC-peak-locations.svg'), 
       plot = figureS1,
       device = 'svg',
       width = 12, height = 6, dpi = 400, bg = 'white')


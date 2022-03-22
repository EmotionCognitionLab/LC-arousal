# this data gathers info on participant inclusions and exclusions,
# applies exclusions to the various dataframes,
# and summarizes participant demographics
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# gather data on inclusions and exclusions --------------------------------

# IDs which completed pre-training MRI session & stress task
summary_MRI_and_stresstask <- data_LCA %>%
  filter(completed_MRI_TSE == 1,
         completed_stresstask == 1) %>%
  group_by(age_group) %>%
  summarize(n = n())

# IDs excluded before LC pipeline
summary_excl_before_LCpipeline <- data_LCA %>%
  filter(completed_MRI_TSE == 1, 
         completed_stresstask == 1,
         include_LCpipeline == 0) %>%
  group_by(exclude_LCpipeline_reason) %>%
  summarize(n = n())

# IDs excluded after LC pipeline
summary_excl_after_LCpipeline <- data_LCA %>%
  filter(completed_MRI_TSE == 1,
         completed_stresstask == 1,
         include_LCpipeline == 1,
         has_LCcontrast == 0) %>%
  group_by(age_group, missing_LCcontrast_reason) %>%
  summarize(n = n())

# IDs without sound task physio recording due to recording error
summary_excl_missing_physio <- data_LCA %>%
  filter(completed_MRI_TSE == 1,
         include_LCpipeline == 1,
         has_LCcontrast == 1,
         has_physio == 0) %>%
  group_by(age_group, missing_physio_reason) %>%
  summarize(n = n())

# IDs with both LC contrast and all physio measures
IDs_LC_and_physio <- data_LCA %>%
  filter(completed_MRI_TSE == 1,
         include_LCpipeline == 1,
         has_LCcontrast == 1,
         has_physio == 1)
summary_LC_and_physio <- data_LCA %>%
  filter(completed_MRI_TSE == 1,
         include_LCpipeline == 1,
         has_LCcontrast == 1,
         has_physio == 1) %>%
  group_by(age_group) %>%
  summarize(n = n())


# restrict data to include only participants with LC & physio -------------

# composite data
data_LCA <- data_LCA %>%
  filter(label_subject %in% IDs_LC_and_physio$label_subject)

# LC data
data_LC_semiauto <- data_LC_semiauto %>%
  filter(label_subject %in% IDs_LC_and_physio$label_subject)
data_LC_manual <- data_LC_manual %>%
  filter(label_subject %in% IDs_LC_and_physio$label_subject)

# physio data
data_physio <- data_physio %>%
  filter(label_subject %in% IDs_LC_and_physio$label_subject)
data_physio_interval <- data_physio_interval %>%
  filter(label_subject %in% IDs_LC_and_physio$label_subject)

# behavioral data
data_pasat <- data_pasat %>%
  filter(label_subject %in% IDs_LC_and_physio$label_subject)
data_stroop <- data_stroop %>%
  filter(label_subject %in% IDs_LC_and_physio$label_subject)


# table 1: sample characteristics -----------------------------------------

table1 <- data_LCA %>%
  group_by(age_group) %>%
  summarize(n = n(),
            age_mean = mean(age),
            age_sd = sd(age),
            age_min = min(age),
            age_max = max(age),
            age_meanSD = format_mean_sd(age_mean, age_sd),
            age_range = format_range(age_min, age_max),
            
            n_female = sum(gender == 'F'),
            pct_female = n_female/n,
            n_pct_female = format_n_pct(n_female, pct_female),
            
            edu_mean = mean(edu),
            edu_sd = sd(edu),
            edu_min = min(edu),
            edu_max = max(edu),
            edu_meanSD = format_mean_sd(edu_mean, edu_sd),
            edu_range = format_range(edu_min, edu_max),
            
            CESD_mean = mean(CESD),
            CESD_sd = sd(CESD),
            CESD_min = min(CESD),
            CESD_max = max(CESD),
            CESD_meanSD = format_mean_sd(CESD_mean, CESD_sd),
            CESD_range = format_range(CESD_min, CESD_max),
            
            TAI_mean = mean(TAI),
            TAI_sd = sd(TAI),
            TAI_min = min(TAI),
            TAI_max = max(TAI),
            TAI_meanSD = format_mean_sd(TAI_mean, TAI_sd),
            TAI_range = format_range(TAI_min, TAI_max)
            
  ) %>%
  select(age_group, n, age_meanSD, age_range, 
         n_pct_female, 
         edu_meanSD, edu_range,
         CESD_meanSD, CESD_range, 
         TAI_meanSD, TAI_range) %>%
  arrange(desc(age_group))

# change age group labels
table1$age_group[table1$age_group == 'YA'] <- 'Younger'
table1$age_group[table1$age_group == 'OA'] <- 'Older'

# transpose to put age groups in columns
table1 <- transpose(table1, make.names = 'age_group')

# include N in column names
colnames(table1) <- paste(colnames(table1), ' (N = ', as.integer(table1[1,]), ')', sep = '')

# drop row with N
table1 <- table1[-1,]

# add row names
rownames(table1) <- c('Age, mean (SD)', 'Age, range', 
                      'N (%) females',
                      'Education, mean (SD)', 'Education, range', 
                      'CESD, mean (SD)', 'CESD, range', 
                      'TAI, mean (SD)', 'TAI, range') 


# perform age group comparisons and add to table --------------------------

age_p <- t.test(age ~ age_group, var.equal = FALSE, data = data_LCA)$p.value
sex_p <- chisq.test(table(data_LCA$gender, data_LCA$age_group))$p.value
edu_p <- t.test(edu ~ age_group, var.equal = FALSE, data = data_LCA)$p.value
CESD_p <- t.test(CESD ~ age_group, var.equal = FALSE, data = data_LCA)$p.value
TAI_p <- t.test(TAI ~ age_group, var.equal = FALSE, data = data_LCA)$p.value

# add column containing comparisons by age group
table1$`Comparison, p-value` <- ''
table1['Age, mean (SD)', 'Comparison, p-value'] <- fix_pval_table(age_p)
table1['N (%) females', 'Comparison, p-value'] <- fix_pval_table(sex_p)
table1['Education, mean (SD)', 'Comparison, p-value'] <- fix_pval_table(edu_p)
table1['CESD, mean (SD)', 'Comparison, p-value'] <- fix_pval_table(CESD_p)
table1['TAI, mean (SD)', 'Comparison, p-value'] <- fix_pval_table(TAI_p)

rm(age_p, sex_p, edu_p, CESD_p, TAI_p)

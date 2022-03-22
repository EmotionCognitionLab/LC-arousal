# functions and figure settings
# for the LC-arousal project
# written by shelby bachman, sbachman@usc.edu


# functions to format columns in sample characteristics table -------------

# format mean & sd as `mean (SD)`
format_mean_sd <- function(mean, SD) {
  mean_round <- round(mean, digits = 2)
  SD_round <- round(SD, digits = 2)
  paste(mean_round, ' (', SD_round, ')', sep = '')
}

# format range as `min - max`
format_range <- function(min, max) {
  paste(min, ' - ', max, sep = '')
}

# format n & % as `n (%)`
format_n_pct <- function(n, frac) {
  pct_formatted = round(frac*100, digits = 1)
  paste(n, ' (', pct_formatted, '%)', sep = '')
}

# format n & % as `% (n=X)`
format_pct_n <- function(frac, n) {
  pct_formatted = round(frac*100, digits = 1)
  paste(pct_formatted, '% (n = ', n, ')', sep = '')
}


# function to format ICC results as a dataframe ---------------------------

format_icc_results <- function(icc_result) {
  
  icc_val <- paste(round(icc_result$value, digits = 3),
                   ' (',
                   icc_result$df1,
                   ', ',
                   icc_result$df2,
                   ')',
                   sep = '')
  
  ci_95 <- paste(round(icc_result$lbound, digits = 3),
                 ', ',
                 round(icc_result$ubound, digits = 3),
                 sep = '')
  
  icc_df <- data.frame(icc_val, ci_95)
  
  return(icc_df)
}



# function to format ICC results for reporting in text --------------------

format_icc_results_text <- function(icc_result) {
  return(
    # all models in manuscript are two-way, single-unit, hence ICC(3,1)
    paste('ICC(3,1) = ',
          round(icc_result$value, digits = 3), 
          ', 95% CI = ',
          round(icc_result$lbound, digits = 3),
          ' - ',
          round(icc_result$ubound, digits = 3),
          ', p ',
          fix_pval_table(icc_result$p.value),
          sep = '')
  )
}



# function to format correlation analysis results as a dataframe ----------

format_corr_results <- function(corr_result) {
  r <- as.numeric(corr_result$estimate)
  df <- as.numeric(corr_result$parameter)
  p <- as.numeric(corr_result$p.value)
  ci <- paste(round(corr_result$conf.int[1], 3), ' - ', round(corr_result$conf.int[2], 3), sep = '')
  
  corr_df <- data.frame(r, ci, df, p)
  
  return(corr_df)
}


# function to format afex ANOVA results as a dataframe --------------------

format_afex_aov_results <- function(afex_aov_obj) {
 
  # format anova results with `nice`
  aov_table <- nice(afex_aov_obj) %>%
    as.data.frame()
  
  # fix row names
  aov_table$Effect <- str_replace_all(aov_table$Effect, pattern = 'age_group', replacement = 'Age group')
  aov_table$Effect <- str_replace_all(aov_table$Effect, pattern = 'topo', replacement = 'Topography')
  aov_table$Effect <- str_replace_all(aov_table$Effect, pattern = ':', replacement = ' x ')
  
  # fix F column
  aov_table$`F` <- afex_aov_obj$anova_table$`F`
  
  return(aov_table) 
  
}



# function to format lme4 output as a dataframe ---------------------------

format_lme4_results <- function(lme4_mod, r_values) {
  
  # create dataframe from model summary
  df <- summary(lme4_mod)$coefficients %>%
    as.data.frame()
  
  # compute 95% confidence intervals of estimates
  cis <- confint(lme4_mod)
  cis <- cis[-c(1,2),]# remove rows for random effects
  df$CI_lower <- cis[,1]
  df$CI_upper <- cis[,2]
  
  # extract parameter names and set as Predictor column
  df$Predictor <- df %>% rownames()
  
  # select only relevant columns
  df <- df %>%
    select(Predictor, Estimate, SE = `Std. Error`, CI_lower, CI_upper, t = `t value`, p = `Pr(>|t|)`)
    
  # create 95% CI column
  df <- df %>%
    rowwise() %>%
    mutate(`95% CI` = str_c(
      round(CI_lower, 3), ', ', round(CI_upper, 3), sep = ''
    ), .before = CI_lower) %>%
    select(-c(CI_lower, CI_upper))
  
  # fix row names
  rn <- df$Predictor
  for (i in 1:length(rn)) {
    if (rn[i] == "(Intercept)") {
      rn[i] <- str_remove_all(rn[i], "[()]")
    } else {
      
      if (str_detect(rn[i], 'phase2-1')) {
        rn[i] <- str_replace_all(rn[i], pattern = 'phase2-1', replacement = 'Challenge - Baseline')
      }
      if (str_detect(rn[i], 'phase3-2')) {
        rn[i] <- str_replace_all(rn[i], pattern = 'phase3-2', replacement = 'Recovery - Challenge')
      }
      if (str_detect(rn[i], 'age_group1')) {
        rn[i] <- str_replace_all(rn[i], pattern = 'age_group1', replacement = 'Age group')
      }
      
      rn[i] <- str_replace_all(rn[i], 
                               pattern = "[:]", 
                               replacement = " x ")
      
    }
  }
  df$Predictor <- rn
  
  # fix p-value
  df$p <- fix_pval_table(df$p)
  
  # add partial r column
  df$`r` <- round(r_values$r, digits = 3)
  
  # round
  
  
  # move p to be final column
  df <- df %>%
    select(Predictor, Estimate, SE, `95% CI`, t, r, p)
  
  return(df)
}


# function to format pairwise comparison results as a dataframe -----------

format_pairwise_result <- function(table_pairwise) {
  
  # rename age group and select only relevant columns
  df <- table_pairwise %>%
    mutate(`Age Group` = ifelse(age_group == 'YA', 'Younger',
                                ifelse(age_group == 'OA', 'Older', NA)))
  
  # compute effect size as partial r 
  eff_r <- t_to_r(t = df$t.ratio,
                   df_error = df$df)
  
  # add effect size to table
  df$r <- eff_r$r
  
  # sort contrasts in order
  df$contrast <- factor(df$contrast, levels = c('Challenge - Baseline',
                                                'Recovery - Challenge', 
                                                'Recovery - Baseline'))
  df <- df %>%
    arrange(desc(`Age Group`), contrast)
  
  # select columns of interest and rename
  df <- df %>%
    select('Age Group', 
           Contrast = contrast, 
           Estimate = estimate, 
           SE, 
           t = t.ratio, 
           r = r,
           p = p.value)
  
  # fix p-value
  df$p <- fix_pval_table(df$p)
  
  return(df)

}


# function to format p-values for reporting -------------------------------

# converts to character s.t. if p<.001, <.001 is printed

fix_pval_table <- function(p, digits = 3) {
  new_p <- vector(mode = 'character', length = length(p))
  for (ii in 1:length(p)) {
    if (p[ii] == '') {
      new_p[ii] <- ''
    } else if (p[ii] < .001) {
      new_p[ii] <- '<.001' 
    } else {
      new_p[ii] <- as.character(printnum(p[ii], digits = digits))
    }
  }
  
  return(new_p)
}


# function for half-violin plot -------------------------------------------

source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")


# function to create crossbar ---------------------------------------------

stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data = fun, colour = "black", 
               alpha = 0.6, geom = geom, width = 0.1, ...)
}


# function to identify & report number of outliers ------------------------

summary_outliers <- function(x) {
  # compute sample median & mad
  sample_median <- median(x, na.rm = TRUE)
  sample_mad <- mad(x, na.rm = TRUE)
  # identify outliers based on mad & median
  comp <- abs(x - sample_median) / (1.483 * sample_mad)
  out <- ifelse(comp > 2.24, 1, 0)
  # return number of outliers
  n_out <- sum(out == 1, na.rm = TRUE)
  return(n_out)
}


# function to identify & remove outliers ----------------------------------

# citation: Wilcox, 2011
madmedianrule <- function(x) {
  # compute sample median & mad
  sample_median <- median(x, na.rm = TRUE)
  sample_mad <- mad(x, na.rm = TRUE)
  # identify outliers based on mad & median
  comp <- abs(x - sample_median) / (1.483 * sample_mad)
  out <- ifelse(comp > 2.24, 1, 0)
  # return data with outliers removed
  x_rmout <- x
  x_rmout[out == 1] <- NA
  return(x_rmout)
}


# function to summarize p's across a set of mixed model outputs ------------

# summarize p's across a set of lmer outputs
# for effect(s) of interest
summarize_p_across_lmers <- function(list_lmer_params, effects) {

  p <- NULL
  for (jj in 1:length(list_lmer_params)) {
    for (hh in 1:length(effects)) {
      p <- append(p, list_lmer_params[[jj]]$p[list_lmer_params[[jj]]$Parameter == effects[hh]])
    }
  }
  
  # case when both p's are < .001
  if (all(p < 0.001)) {
    return('< .001')
    # case when both p's are >= .05
  } else if (all(p >= 0.05)) {
    return( str_c('>= ', round(min(p), digits = 3), sep = '') )
    # cases when some p's are between .001 and 05
  } else if (all(p < 0.05)) {
    return ( str_c('<= ', round(max(p), digits = 3), sep = '') )
  }
}


# figure settings ---------------------------------------------------------

# color palette
palette_agegr <- c('Younger' = '#00AFBB', 'Older' = '#E7B800')

# set font size larger
theme_font <- theme(axis.text.x = element_text(size = 12), 
                    axis.title.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12), 
                    axis.title.y = element_text(size = 12), 
                    legend.text = element_text(size = 12), 
                    text = element_text(size = 12))


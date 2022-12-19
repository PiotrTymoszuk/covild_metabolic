# Kinetics of thrombo/inflammatory, metabolic variables and CTSS
# in the entire cohort, the dyslipidemia and dysglycemia strata
#
# The recovery effect is assessed by Friedman test with W effect size statistic
# Differences between the strata are assessed by Mann-Whitney test

  insert_head()

# container ------

  tc <- list()

# parallel backend ------

  insert_msg('Parallel backend')

  plan('multisession')

# Analysis globals ------

  insert_msg('Analysis globals')

  ## variables

  tc$variables <- c('TG', 'HDL', 'ADIPOQ', 'LEP',
                    'CRP', 'IL6', 'DDimer', 'FT',
                    'ctss')

  tc$variables <- set_names(tc$variables,
                            tc$variables)

  tc$var_lexicon <- globals$var_lexicon %>%
    filter(variable %in% paste0(tc$variables, '_V0')) %>%
    mutate(variable = stri_replace(variable,
                                   regex = '_V\\d{1}$',
                                   replacement = ''),
           label = stri_split_fixed(label,
                                    pattern = ', ',
                                    simplify = TRUE)[, 1],
           axis_label = ifelse(is.na(unit),
                               label,
                               paste(label, unit, sep = ', ')))

  ## analysis tables with complete longitudinal cases only
  ## introducing a stratification variable

  tc$analysis_tbl <- tc$variables %>%
    map(~select(covild$data,
                patient_id,
                dyslipidemia_V0,
                dysglycemia_V0,
                starts_with(paste0(.x, '_V')))) %>%
    map(~filter(.x, complete.cases(.x))) %>%
    map(mutate,
        strat_dyslipi = dyslipidemia_V0,
        strat_dysgly = dysglycemia_V0) %>%
    map(~rbind(.x,
               mutate(.x,
                      strat_dyslipi = 'cohort',
                      strat_dysgly = 'cohort')))

# Descriptive stats --------

  insert_msg('Descriptive stats')

  tc$desc_stats <- c(strat_dyslipi = 'strat_dyslipi',
                     strat_dysgly = 'strat_dysgly') %>%
    map(function(strata) tc$analysis_tbl %>%
          map(~explore(.x,
                       variables = names(.x)[!names(.x) %in% c('patient_id',
                                                               'dyslipidemia_V0',
                                                               'dysglycemia_V0',
                                                               'strat_dyslipi',
                                                               'strat_dysgly')],
                       split_factor = strata,
                       what = 'table',
                       pub_styled = TRUE)))

# Testing for the differences at particular timepoints ------

  insert_msg('Post-hoc test: differences at the timepoints')

  tc$post_hoc_test <-  c(strat_dyslipi = 'strat_dyslipi',
                         strat_dysgly = 'strat_dysgly') %>%
    map(function(strata) tc$analysis_tbl %>%
          map(filter, .data[[strata]] != 'cohort') %>%
          map(~compare_variables(.x,
                                 variables = names(.x)[!names(.x) %in% c('patient_id',
                                                                         'dyslipidemia_V0',
                                                                         'dysglycemia_V0',
                                                                         'strat_dyslipi',
                                                                         'strat_dysgly')],
                                 split_factor = strata,
                                 what = 'eff_size',
                                 types = 'wilcoxon_r',
                                 exact = FALSE,
                                 ci = FALSE,
                                 pub_styled = FALSE,
                                 adj_method = 'holm')))

# Labels with results of the post-hoc tests ------

  insert_msg('Labels with the significant results of post-hoc tests')

  tc$post_hoc_labs  <- tc$post_hoc_test %>%
    map(~map(.x, ~ifelse(.x$p_adjusted < 0.05,
                         .x$significance, '')))

# Testing for the global kinetic differences --------

  insert_msg('Testing for the global differences')

  ## long-format tables

  tc$long_tbl <- tc$analysis_tbl %>%
    map2(., names(.),
         ~pivot_longer(.x,
                      cols = starts_with(.y),
                      names_to = 'timepoint',
                      values_to = 'value')) %>%
    map(mutate,
        timepoint = stri_extract(timepoint, regex = 'V\\d{1}$'),
        timepoint = factor(timepoint, c('V0', 'V1', 'V2', 'V3')),
        time = car::recode(timepoint,
                           "'V0' = '60';
                                'V1' = '100';
                                'V2' = '180';
                                'V3'= '360'"),
        time = factor(time, c('60', '100', '180', '360')))

  ## Friedman test

  tc$friedman_test <- c(strat_dyslipi = 'strat_dyslipi',
                        strat_dysgly = 'strat_dysgly') %>%
    map(function(strata) tc$long_tbl %>%
          map(~dlply(.x, strata,
                     friedman_test,
                     formula = value ~ timepoint|patient_id) %>%
                compress(names_to = strata)) %>%
          map(mutate,
              significance = ifelse(p < 0.05,
                                    paste('p =', signif(p, 2)),
                                    paste0('ns (p = ', signif(p, 2), ')'))))

  ## Kendall W effect size

  tc$eff_size <- c(strat_dyslipi = 'strat_dyslipi',
                   strat_dysgly = 'strat_dysgly') %>%
    map(function(strata) tc$long_tbl %>%
          future_map(~dlply(.x, strata,
                            friedman_effsize,
                            formula = value ~ timepoint|patient_id,
                            ci = FALSE,
                            ci.type = 'bca') %>%
                       compress(names_to = strata),
                     .options = furrr_options(seed = TRUE)))

  tc$eff_size <- tc$eff_size %>%
    map(~map(.x,
             mutate,
             w_size = paste('W =', signif(effsize, 2))))

# Plot captions ------

  insert_msg('ready-to-use plot captions')

  ## they contain the the effect size and p values for the Friedman test

  tc$plot_caps$strat_dyslipi <- map2(tc$eff_size$strat_dyslipi,
                                     tc$friedman_test$strat_dyslipi,
                                     ~map2_chr(.x$w_size[1:2],
                                               .y$significance[1:2],
                                               paste, sep = ', ') %>%
                                       map2_chr(c('normal', 'dyslipidemia'), .,
                                                paste, sep = ': '))

  tc$plot_caps$strat_dysgly <- map2(tc$eff_size$strat_dysgly,
                                    tc$friedman_test$strat_dysgly,
                                    ~map2_chr(.x$w_size[1:2],
                                              .y$significance[1:2],
                                              paste, sep = ', ') %>%
                                      map2_chr(c('normal', 'dysglycemia'), .,
                                               paste, sep = ': '))

  tc$plot_caps <- tc$plot_caps %>%
    map(~map(.x, paste, collapse = '\n'))

# Plot tags --------

  insert_msg('Plot tags')

  tc$plot_tags$strat_dyslipi <-  tc$friedman_test$strat_dyslipi %>%
    map(~.x$n[1:2]) %>%
    map(~map2_chr(c('normal', 'dyslipidemia'), .x,
                  paste, sep = ': n = ') %>%
          paste(collapse = ', ') %>%
          paste0('\n', .))

  tc$plot_tags$strat_dysgly <-  tc$friedman_test$strat_dysgly %>%
    map(~.x$n[1:2]) %>%
    map(~map2_chr(c('normal', 'dysglycemia'), .x,
                  paste, sep = ': n = ') %>%
          paste(collapse = ', ') %>%
          paste0('\n', .))

# Kinetic plot data: means and IQRs --------

  insert_msg('Kinetic plots: median and IQRs')

  tc$plot_data <- c(strat_dyslipi = 'strat_dyslipi',
                    strat_dysgly = 'strat_dysgly') %>%
    map(function(strata) tc$long_tbl %>%
          map(group_by,
              .data[[strata]],
              timepoint) %>%
          map(summarise,
              median = median(value),
              lower_q = quantile(value, 0.25),
              upper_q = quantile(value, 0.75),
              mean = mean(value),
              sd = sd(value))) %>%
    map(~map(.x,
             mutate,
             time = car::recode(timepoint,
                                "'V0' = '60';
                                'V1' = '100';
                                'V2' = '180';
                                'V3'= '360'"),
             time = factor(time, c('60', '100', '180', '360')))) %>%
    map(~map(.x, ungroup))

# Kinetic plots: lines with ribbons -------

  insert_msg('Plotting kinetics in the strata')

  ## dyslipidemia strata

  tc$line_plots$strat_dyslipi <-
    list(data = tc$plot_data$strat_dyslipi %>%
           map(filter, strat_dyslipi != 'cohort'),
         title = exchange(names(tc$plot_data$strat_dyslipi),
                          dict = tc$var_lexicon,
                          key = 'variable',
                          value = 'label'),
         subtitle = tc$plot_caps$strat_dyslipi,
         y_lab = exchange(names(tc$plot_data$strat_dyslipi),
                          dict = tc$var_lexicon,
                          key = 'variable',
                          value = 'axis_label'),
         tag = tc$plot_tags$strat_dyslipi,
         post_hoc_labs = tc$post_hoc_labs$strat_dyslipi) %>%
    pmap(plot_kinetic,
         strata_var = 'strat_dyslipi') %>%
    map(~.x +
          scale_color_manual(values = c(no = 'steelblue3',
                                        yes = 'coral3'),
                             labels = c(no = 'normal',
                                        yes = 'dyslipidemia'),
                             name = '') +
          scale_fill_manual(values = c(no = 'steelblue3',
                                       yes = 'coral3'),
                            labels = c(no = 'normal',
                                       yes = 'dyslipidemia'),
                            name = ''))
  ## dysglycemia strata

  tc$line_plots$strat_dysgly <-
    list(data = tc$plot_data$strat_dysgly %>%
           map(filter, strat_dysgly != 'cohort'),
         title = exchange(names(tc$plot_data$strat_dysgly),
                          dict = tc$var_lexicon,
                          key = 'variable',
                          value = 'label'),
         subtitle = tc$plot_caps$strat_dysgly,
         y_lab = exchange(names(tc$plot_data$strat_dysgly),
                          dict = tc$var_lexicon,
                          key = 'variable',
                          value = 'axis_label'),
         tag = tc$plot_tags$strat_dysgly,
         post_hoc_labs = tc$post_hoc_labs$strat_dysgly) %>%
    pmap(plot_kinetic,
         strata_var = 'strat_dysgly') %>%
    map(~.x +
          scale_color_manual(values = c(no = 'steelblue3',
                                        yes = 'plum3'),
                             labels = c(no = 'normal',
                                        yes = 'dysglycemia'),
                             name = '') +
          scale_fill_manual(values = c(no = 'steelblue3',
                                       yes = 'plum3'),
                            labels = c(no = 'normal',
                                       yes = 'dysglycemia'),
                            name = ''))

# Kinetic plots: box plots -------

  insert_msg('Kinetic box plots')

  ## dyslipidemia strata

  tc$box_plots$strat_dyslipi <-
    list(data = tc$long_tbl %>%
           map(filter, strat_dyslipi != 'cohort'),
         title = exchange(names(tc$plot_data$strat_dyslipi),
                          dict = tc$var_lexicon,
                          key = 'variable',
                          value = 'label'),
         subtitle = tc$plot_caps$strat_dyslipi,
         y_lab = exchange(names(tc$plot_data$strat_dyslipi),
                          dict = tc$var_lexicon,
                          key = 'variable',
                          value = 'axis_label'),
         tag = tc$plot_tags$strat_dyslipi,
         post_hoc_labs = tc$post_hoc_labs$strat_dyslipi,
         point_hjitter = c(rep(0, 8), 0.1)) %>%
    pmap(plot_box,
         strata_var = 'strat_dyslipi',
         point_alpha = 0.25) %>%
    map(~.x +
          scale_fill_manual(values = c(no = 'steelblue3',
                                       yes = 'coral3'),
                            labels = c(no = 'normal',
                                       yes = 'dyslipidemia'),
                            name = '60d FUP'))

  ## dysglycemia strata

  tc$box_plots$strat_dysgly <-
    list(data = tc$long_tbl %>%
           map(filter, strat_dysgly != 'cohort'),
         title = exchange(names(tc$plot_data$strat_dysgly),
                          dict = tc$var_lexicon,
                          key = 'variable',
                          value = 'label'),
         subtitle = tc$plot_caps$strat_dysgly,
         y_lab = exchange(names(tc$plot_data$strat_dysgly),
                          dict = tc$var_lexicon,
                          key = 'variable',
                          value = 'axis_label'),
         tag = tc$plot_tags$strat_dyslipi,
         post_hoc_labs = tc$post_hoc_labs$strat_dysgly,
         point_hjitter = c(rep(0, 8), 0.1)) %>%
    pmap(plot_box,
         strata_var = 'strat_dysgly',
         point_alpha = 0.25) %>%
    map(~.x +
          scale_fill_manual(values = c(no = 'steelblue3',
                                       yes = 'plum3'),
                            labels = c(no = 'normal',
                                       yes = 'dysglycemia'),
                            name = '60d FUP'))

# END -------

  plan('sequential')

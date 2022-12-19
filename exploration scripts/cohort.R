# Characteristic of the study cohort

  insert_head()

# container -----

  cohort <- list()

# analysis globals --------

  insert_msg('Analysis globals')

  ## variables

  cohort$variables <- globals$var_lexicon$variable

  ## analysis tables

  cohort$analysis_tbl <- list(covild$data %>%
                                mutate(split_var = 'cohort'),
                              covild$data %>%
                                mutate(split_var = severity)) %>%
    reduce(rbind) %>%
    mutate(split_var = factor(split_var,
                              c('cohort', levels(covild$data$severity)))) %>%
    filter(!is.na(split_var))

  ## test and plot types, Y axes titles

  cohort$test_type <- cohort$variables %>%
    map(~is.numeric(cohort$analysis_tbl[[.x]])) %>%
    map(~ifelse(.x, 'kruskal_eta', 'cramer_v')) %>%
    set_names(cohort$variables)

  cohort$plot_type <- cohort$variables %>%
    map(~is.numeric(cohort$analysis_tbl[[.x]])) %>%
    map(~ifelse(.x, 'violin', 'stack')) %>%
    set_names(cohort$variables)

  cohort$y_axes <- cohort$variables %>%
    map(~ifelse(is.numeric(cohort$analysis_tbl[[.x]]),
                exchange(.x,
                         dict = globals$var_lexicon,
                         key = 'variable',
                         value = 'axis_label'),
                '% of strata')) %>%
    set_names(cohort$variables)

  ## colors for plotting

  cohort$fill_scale <- cohort$variables %>%
    map(~is.numeric(cohort$analysis_tbl[[.x]])) %>%
    map(function(x) if(x) {

      scale_fill_manual(values = c(cohort = 'cornsilk2',
                                   mild = 'steelblue',
                                   moderate = 'gray60',
                                   severe = 'coral3'),
                        name = '')

    } else {

      scale_fill_brewer(type = 'div')

    })

# Descriptive statistics -------

  insert_msg('Descriptive statistics')

  cohort$stats <- cohort$analysis_tbl %>%
    explore(split_factor = 'split_var',
            variables = cohort$variables,
            what = 'table',
            pub_styled = TRUE)

# Comparison between the severity subsets ------

  insert_msg('Comparison between the severity strata')

  ## done with Kruskal-Wallis test for numeric variables
  ## and chi-squared test for categorical variables

  cohort$test <- cohort$analysis_tbl %>%
    filter(split_var != 'cohort') %>%
    compare_variables(variables = cohort$variables,
                      split_factor = 'severity',
                      what = 'eff_size',
                      types = cohort$test_type,
                      exact = FALSE,
                      ci = FALSE,
                      pub_styled = TRUE,
                      .parallel = TRUE,
                      .paropts = furrr_options(seed = TRUE,
                                               globals = c('cohort'),
                                               packages = c('rlang',
                                                            'dplyr'))) %>%
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))

# Plotting -------

  insert_msg('Plotting of the variables, split by severity')

  cohort$plots <- list(variable = cohort$variables,
                       type = cohort$plot_type,
                       plot_title = exchange(cohort$variables,
                                             dict = globals$var_lexicon,
                                             key = 'variable',
                                             value = 'label'),
                       y_lab = cohort$y_axes,
                       plot_subtitle = cohort$test$plot_cap) %>%
    pmap(plot_variable,
         cohort$analysis_tbl,
         split_factor = 'split_var',
         scale = 'percent',
         point_hjitter = 0,
         cust_theme = globals$common_theme) %>%
    set_names(cohort$variables)

  ## manual adjustment of the plot colors

  cohort$plots <- map2(cohort$plots,
                       cohort$fill_scale,
                       ~.x + .y)

# END ------

  insert_tail()

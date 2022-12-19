# Distribution of numeric explanatory variables between the sex and severity
# strata. Exploring the needs for normality-improving transformations

  insert_head()

# container ------

  expl_distr <- list()

# globals ------

  insert_msg('Analysis globals')

  expl_distr$variables <-
    c('bmi', 'age', 'CRP_V0',
      'IL6_V0', 'DDimer_V0', 'FT_V0', 'TG_V0',
      'HDL_V0', 'ADIPOQ_V0', 'LEP_V0')

  ## analysis table
  ## adding log and sqrt transformations

  expl_distr$analysis_tbl <-
    covild$data[c('sex', 'severity', expl_distr$variables)]

  for(i in expl_distr$variables) {

    expl_distr$analysis_tbl <- expl_distr$analysis_tbl %>%
      mutate(!!paste0('log_', i) := log(.data[[i]] + 1),
             !!paste0('sqrt_', i) := sqrt(.data[[i]]))

  }

  ## updating the variable list

  expl_distr$variables <-
    names(expl_distr$analysis_tbl)[!names(expl_distr$analysis_tbl) %in% c('sex', 'severity')]

  ## adding the splitting factor variable

  expl_distr$analysis_tbl <- expl_distr$analysis_tbl %>%
    mutate(split_var = interaction(severity, sex)) %>%
    filter(!is.na(split_var))

# Normality testing ------

  insert_msg('Normality testing')

  ## Shapiro-Wilk test

  expl_distr$normality <- expl_distr$analysis_tbl %>%
    explore(split_factor = 'split_var',
            variables = expl_distr$variables,
            what = 'normality',
            pub_styled = FALSE) %>%
    compress(names_to = 'split_var') %>%
    mutate(base_variable = stri_replace(variable,
                                        regex = '(log_)|(sqrt_)',
                                        replacement = ''))

  ## identification of the transformations with the best normality
  ## the criterion is the average W statistic accross the modeling groups

  expl_distr$best_normality <- expl_distr$normality %>%
    group_by(variable) %>%
    summarise(stat = mean(stat),
              base_variable = unique(base_variable)) %>%
    ungroup %>%
    group_by(base_variable) %>%
    filter(stat == max(stat)) %>%
    ungroup %>%
    mutate(transformation = stri_extract(variable, regex = '(log)|(sqrt)')) %>%
    select(variable,
           base_variable,
           transformation,
           stat)

# EOV -------

  insert_msg('Homogeneity of variances')

  expl_distr$eov <- expl_distr$analysis_tbl %>%
    compare_variables(split_factor = 'split_var',
                      variables = expl_distr$variables,
                      what = 'variance',
                      pub_styled = FALSE)

  # checking the EOV for the transformation with the best normality

  expl_distr$eov_best <- expl_distr$eov %>%
    filter(variable %in% expl_distr$best_normality$variable)

# END ------

  rm(i)

  insert_tail()

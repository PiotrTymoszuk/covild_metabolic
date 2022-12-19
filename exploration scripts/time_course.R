# Distribution of thrombo-inflammation markers at consecutive visits
# in participants with and without dysglycemia and dyslipidemia

  insert_head()

# container -----

  tc_distr <- list()

# analysis globals --------

  insert_msg('Analysis globals')

  ## variables of interest

  tc_distr$variables <- c('IL6', 'CRP', 'Ddimer', 'FT', 'ADIPOQ', 'LEP')

  ## analysis tables in a long format

  tc_distr$analysis_tbls <-
    set_names(tc_distr$variables,
              tc_distr$variables) %>%
    map(~select(covild$data,
                patient_id,
                dysglycemia_V0,
                dyslipidemia_V0,
                starts_with(paste0(.x, '_V')))) %>%
    map(~filter(.x,
                complete.cases(.x))) %>%
    map2(tc_distr$variables,
         ~pivot_longer(.x,
                       cols = starts_with(.y),
                       names_to = 'timepoint',
                       values_to = .y)) %>%
    map(mutate,
        timepoint = stri_extract(timepoint, regex = 'V\\d{1}$'),
        timepoint = factor(timepoint, c('V0', 'V1', 'V2', 'V3')),
        dysglycemia_group = interaction(dysglycemia_V0, timepoint),
        dyslipidemia_group = interaction(dyslipidemia_V0, timepoint))

  ## adding the logarithm and sqrt transformations

  tc_distr$analysis_tbls <-
    map2(tc_distr$analysis_tbls,
         tc_distr$variables,
         ~mutate(.x,
                 !!paste0('log_', .y) := log(.data[[.y]] + 1),
                 !!paste0('sqrt_', .y) := sqrt(.data[[.y]])))

# Normality testing -------

  insert_msg('Normality')

  tc_distr$normality$dysglycemia <-
    map2(tc_distr$analysis_tbls,
         tc_distr$variables,
         ~explore(.x,
                  variables = c(.y, paste0(c('log_', 'sqrt_'), .y)),
                  split_factor = 'dysglycemia_group',
                  what = 'normality',
                  pub_styled = FALSE)) %>%
    map(compress,
        names_to = 'dysglycemia_group') %>%
    map_dfr(mutate,
            base_variable = stri_replace(variable,
                                         regex = '(log_)|(sqrt_)',
                                         replacement = ''))

  tc_distr$normality$dyslipidemia <-
    map2(tc_distr$analysis_tbls,
         tc_distr$variables,
         ~explore(.x,
                  variables = c(.y, paste0(c('log_', 'sqrt_'), .y)),
                  split_factor = 'dyslipidemia_group',
                  what = 'normality',
                  pub_styled = FALSE)) %>%
    map(compress,
        names_to = 'dyslipidemia_group') %>%
    map_dfr(mutate,
            base_variable = stri_replace(variable,
                                         regex = '(log_)|(sqrt_)',
                                         replacement = ''))

  ## identifying the optimal transformations to enhance normality
  ## the criterion: mean W statistic

  tc_distr$best_normality <-  tc_distr$normality %>%
    map(group_by, variable) %>%
    map(summarise,
        stat = mean(stat),
        base_variable = unique(base_variable)) %>%
    map(ungroup) %>%
    map(group_by, base_variable) %>%
    map(filter, stat == max(stat)) %>%
    map(mutate,
        transformation = stri_extract(variable, regex = '(log)|(sqrt)'))

# EOV ------

  insert_msg('Homogeneity of variances')

  ## dysglycemia and dyslipidemia strata

  tc_distr$eov$dysglycemia <-
    map2_dfr(tc_distr$analysis_tbls,
             tc_distr$variables,
             ~compare_variables(.x,
                                variables = c(.y,
                                              paste0(c('log_', 'sqrt_'), .y)),
                                split_factor = 'dysglycemia_group',
                                what = 'variance',
                                pub_styled = TRUE))

  tc_distr$eov$dyslipidemia <-
    map2_dfr(tc_distr$analysis_tbls,
             tc_distr$variables,
             ~compare_variables(.x,
                                variables = c(.y,
                                              paste0(c('log_', 'sqrt_'), .y)),
                                split_factor = 'dyslipidemia_group',
                                what = 'variance',
                                pub_styled = TRUE))

  ## for the transformations selected to enhance normality

  tc_distr$eov_best <-
    map2(tc_distr$eov,
         tc_distr$best_normality,
         ~filter(.x, variable %in% .y$variable))

# END ------

  insert_tail()

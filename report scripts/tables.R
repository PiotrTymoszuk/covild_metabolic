# Report tables

  insert_head()

# container ------

  tables <- list()

# Table 1: characteristic of the study cohort -------

  insert_msg('Table 1: characteristic of the study cohort')

  ## descriptive stats, appending with the n numbers

  tables$cohort$stats <- cohort$stats %>%
    map(~map_dfc(.x,
                 stri_replace,
                 regex = '^Mean.*\\nMedian\\s{1}=\\s{1}',
                 replacement = '') %>%
          map_dfc(stri_replace,
                  regex = '^no.*\\nyes:\\s{1}',
                  replacement = '') %>%
          map_dfc(stri_replace,
                  regex = '\\nComplete.*$',
                  replacement = '')) %>%
    reduce(left_join, by = 'variable') %>%
    set_names(c('variable', 'cohort', 'mild', 'moderate', 'severe'))

  tables$cohort$n_numbers <- cohort$analysis_tbl %>%
    count(split_var)

  tables$cohort$stats <- rbind(tibble(variable = 'n_number',
                                      cohort = tables$cohort$n_numbers$n[1],
                                      mild = tables$cohort$n_numbers$n[2],
                                      moderate = tables$cohort$n_numbers$n[3],
                                      severe = tables$cohort$n_numbers$n[4]),
                               tables$cohort$stats)

  ## testing results

  tables$cohort$test <-
    cohort$test[c('variable', 'significance', 'eff_size')]

  ## the entire table

  tables$cohort <- left_join(tables$cohort$stats,
                             tables$cohort$test,
                             by = 'variable') %>%
    mutate(variable = ifelse(variable == 'n_number',
                             'Patricipants, n',
                             exchange(variable,
                                      dict = globals$var_lexicon,
                                      key = 'variable',
                                      value = 'axis_label'))) %>%
    set_names(c('Variable', 'Cohort',
                'Mild COVID-19',
                'Moderate COVID-19',
                'Severe COVID-19',
                'Significance',
                'Effect size'))

  tables$cohort <-
    mdtable(tables$cohort,
            label = 'table_1_cohort',
            ref_name = 'cohort',
            caption = paste('Chracteristic of the study cohort:',
                            'demographic background, medical history,',
                            'COVID-19 severity, inflammatory and metabolic',
                            'parameters. Numeric variables are shown as',
                            'medians with interquartile ranges. Categorical',
                            'variables are presented as percentages and',
                            'counts within the complete observation set.'))

# Table 2: fit stats of the ordinal regression fits, confounders -----

  insert_msg('Table 2: fit stats of the confounder models')

  tables$counfounder_fit <- ctss_mod$conf_mtx$conf_models %>%
    map_dfr(~.x$overall) %>%
    mutate(class_error = 1 - Accuracy,
           fit_stats = paste0('class. error = ', signif(class_error, 2),
                              '\n\u03BA = ', signif(Kappa, 2)),
           variable = names(ctss_mod$conf_mtx$conf_models),
           variable = exchange(variable,
                               dict = globals$var_lexicon,
                               key = 'variable',
                               value = 'label')) %>%
    select(variable, fit_stats) %>%
    set_names(c('Variable', 'Classification statistics')) %>%
    mdtable(label = 'table_2_confounder_fit_stats',
            ref_name = 'conf_models',
            caption = paste('Classification error and accuracy of',
                            'logistic ordinal modeling of chest CT',
                            'abnormality severity at the day 60 follow-up',
                            'as a function of',
                            'age, sex and COVID-19 severity.'))

# Table 3: results of ordinal regression, confounders -----

  insert_msg('Table 3: result of ordinal modelinig, confounders')

  tables$confounders <-
    mdtable(ctss_mod$conf_results,
            label = 'table_3_confounder_modeling',
            ref_name = 'confounders',
            caption = paste('Results of logistic ordinal modeling of chest CT',
                            'abnormality severity at the day 60 follow-up',
                            'as a function of',
                            'age, sex and COVID-19 severity.',
                            'Odds ratios (OR) with 95% confidence',
                            'intervals, p values, numbers of cases in',
                            'the strata and the total observation',
                            'counts are presented.'))

# Table 4: fit stats or ordinal regression, explanatory variables -------

  insert_msg('Table 4: fit stats for oridinal modeling, explanatory variables')

  tables$explanatory_fit <- ctss_mod$conf_mtx[c('uni_models',
                                                'adj_models',
                                                'full_models')] %>%
    map(~map_dfr(.x, ~.x$overall)) %>%
    map(mutate,
        variable = ctss_mod$variables,
        class_error = 1 - Accuracy,
        fit_stats = paste0('class. error = ', signif(class_error, 2),
                           '\n\u03BA = ', signif(Kappa, 2)),
        base_variable = stri_replace(variable,
                                     regex = '(log_)|(sqrt_)',
                                     replacement = ''),
        transformation = stri_extract(variable, regex = 'log|sqrt'),
        variable = exchange(base_variable,
                            dict = globals$var_lexicon,
                            key = 'variable',
                            value = 'label'),
        variable = ifelse(is.na(transformation),
                          variable,
                          paste(transformation, variable))) %>%
    map(select, variable, fit_stats)

  tables$explanatory_fit <- tables$explanatory_fit %>%
    reduce(left_join, by = 'variable') %>%
    set_names(c('Variable',
                'Unadjusted univariable model',
                'Age/sex-adjusted model',
                'Age/sex/severity-adjusted model')) %>%
    mdtable(label = 'table_4_explanatory_fit',
            ref_name = 'explanatory_fit',
            caption = paste('Classification error and accuracy of',
                            'logistic ordinal modeling of chest CT',
                            'abnormality severity at the day 60 follow-up',
                            'as a function of',
                            'inflammatory and metabolic parameters.'))

# Table 5: results of ordinal regression, explanatory variables ------

  insert_msg('Table 5: results of ordinal modeling, explanatory variables')

  tables$explanatory <-
    mdtable(ctss_mod$model_results,
            label = 'table_5_explanatory_modeling',
            ref_name = 'explanatory',
            caption = paste('Results of logistic ordinal modeling of chest CT',
                            'abnormality severity at the day 60 follow-up',
                            'as a function of',
                            'inflammatory and metabolic parameters.',
                            'Odds ratios (OR) with 95% confidence',
                            'intervals, p values, numbers of cases in',
                            'the strata and the total observation',
                            'counts are presented for univariable/unadjusted',
                            'models, age/sex- and',
                            'age/sex/severity-adjusted models.'))

# Saving the tables on the disc ------

  insert_msg('Saving the tables')

  tables$cover <- tables %>%
    map_dfr(~tibble(Table = paste('Table', 1:length(tables)),
                Legend = attr(.x, 'caption')))

  tables[c('cover', names(tables)[names(tables) != 'cover'])] %>%
    set_names(c('Cover', paste('Table', 1:(length(tables) - 1)))) %>%
    write_xlsx('./report/tables.xlsx')

# END -------

  insert_tail()

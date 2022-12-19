# Modeling of the CTSS severity at the 60 day follow-up
#
# The method: fixed-effect ordinal regression.
# CTSS severity was classified as 'absent' (CTSS = 0), 'mild' (1 - 5),
# 'moderate' (6 - 10) and 'severe' (11 and more).
#
# The explanatory variables are: BMI class, elevated CRP, elevated IL-6,
# log DDimer, log ferritin, log triglicerides, log HDL, log adiponectin
# log leptin, dyslipidemia and dysglycemia
#
# The explanatory variables are included in the models as first-order terms.
#
# For log ferritin, a natural spline with df = 2 may be
# considered (see results of exploratory analysis,
# investigation of the response - explanatory relationship for few modeling
# scenarios). This results in a slightly better fit but the message stays
# the same as with the linearity.
#
# The models are constructed in three flavors: univariable, adjusted for age
# and sex, and adjusted for age, sex and CoV severity
#
# Using categorical age, BMI (non-obese vs obese) and TG, since the proportional
# odds assumption is violated with the numeric variables

  insert_head()

# container ------

  ctss_mod <- list()

# globals -----

  insert_msg('Analysis globals')

  ## explanatory variables

  ctss_mod$confounders <- c('age_class', 'sex', 'severity')

  ctss_mod$variables <- c('CRP_class_V0',
                          'IL6_class_V0', 'log_DDimer_V0',
                          'log_FT_V0', 'TG_class_V0', 'log_HDL_V0',
                          'log_ADIPOQ_V0', 'log_LEP_V0',
                          'obesity', 'dyslipidemia_V0', 'dysglycemia_V0')

  ## analysis table

  ctss_mod$analysis_tbl <- covild$data %>%
    transmute(patient_id = patient_id,
              ctss_class_V0 = ctss_class_V0,
              age_class = age_class,
              sex = sex,
              severity = severity,
              obesity = obesity,
              CRP_class_V0 = CRP_class_V0,
              IL6_class_V0 = IL6_class_V0,
              log_DDimer_V0 = log(DDimer_V0),
              log_FT_V0 = log(FT_V0),
              TG_class_V0 = TG_class_V0,
              log_HDL_V0 = log(HDL_V0),
              log_ADIPOQ_V0 = log(ADIPOQ_V0),
              log_LEP_V0 = log(LEP_V0),
              dyslipidemia_V0 = dyslipidemia_V0,
              dysglycemia_V0 = dysglycemia_V0) %>%
    filter(complete.cases(.))

  ## model formulas for the confounders

  ctss_mod$conf_formulas <- paste('ctss_class_V0',
                                  ctss_mod$confounders,
                                  sep = '~') %>%
    map(as.formula) %>%
    set_names(ctss_mod$confounders)

  ## univariable model formulas

  ctss_mod$uni_formulas <- paste('ctss_class_V0',
                                 ctss_mod$variables,
                                 sep = '~') %>%
    map(as.formula) %>%
    set_names(ctss_mod$variables)

  ## models adjusted for age and sex

  ctss_mod$adj_formulas <- paste('ctss_class_V0',
                                 ctss_mod$variables,
                                 sep = '~') %>%
    paste0(., ' + sex + age_class') %>%
    map(as.formula) %>%
    set_names(ctss_mod$variables)

  ## adjusted for age, sex and CoV severity

  ctss_mod$full_formulas <- paste('ctss_class_V0',
                                 ctss_mod$variables,
                                 sep = '~') %>%
    paste0(., ' + sex + age_class + severity') %>%
    map(as.formula) %>%
    set_names(ctss_mod$variables)

# construction of the models ------

  insert_msg('Construction of the models')

  ## confounder models

  ctss_mod$conf_models <- ctss_mod$conf_formulas %>%
    map(~make_lm(data = ctss_mod$analysis_tbl,
                 formula = .x,
                 mod_fun = polr,
                 family = NULL))

  ## uni-variable models

  ctss_mod$uni_models <- ctss_mod$uni_formulas %>%
    map(~make_lm(data = ctss_mod$analysis_tbl,
                 formula = .x,
                 mod_fun = polr,
                 family = NULL))

  ## sex- and age-adjusted models

  ctss_mod$adj_models <- ctss_mod$adj_formulas %>%
    map(~make_lm(data = ctss_mod$analysis_tbl,
                 formula = .x,
                 mod_fun = polr,
                 family = NULL))

  ## sex, age and severity-adjusted models

  ctss_mod$full_models <- ctss_mod$full_formulas %>%
    map(~make_lm(data = ctss_mod$analysis_tbl,
                 formula = .x,
                 mod_fun = polr,
                 family = NULL))

# Assumption check -------

  insert_msg('Assumption check')

  ## normality of the residuals by Shapiro-Wilk test
  ## proportional odds assumption by Brant test
  ## (note: p > 0.05 means that the assumption holds!)

  ctss_mod$assumptions <-
    ctss_mod[c('conf_models',
               'uni_models',
               'adj_models',
               'full_models')] %>%
    map(~map(.x, summary, 'assumptions'))

  ctss_mod$resid_plots <-
    ctss_mod[c('conf_models',
               'uni_models',
               'adj_models',
               'full_models')] %>%
    map(~map(.x, plot, cust_theme = globals$common_theme))

# Model fit stats ------

  insert_msg('Fit stats')

  ## fit stats

  ctss_mod$fit_stats <-
    ctss_mod[c('conf_models',
               'uni_models',
               'adj_models',
               'full_models')] %>%
    map(~map(.x, summary, 'fit'))

  ## confusion matrix, accuracy and kappa

  ctss_mod$conf_mtx <-
    ctss_mod[c('conf_models',
               'uni_models',
               'adj_models',
               'full_models')] %>%
    map(~map(.x,
             ~cbind(model.frame(.x)[, 1],
                    predict(.x))) %>%
          map(as.data.frame) %>%
          map(~map_dfc(.x,
                       car::recode,
                       "1 = 'none';
                       2 = 'mild';
                       3 = 'moderate';
                       4 = 'severe'")) %>%
          map(~map_dfc(.x,
                       factor,
                       c('none', 'mild', 'moderate', 'severe'))) %>%
          map(~confusionMatrix(data = .x[[2]], reference = .x[[1]])))

# LRT -------

  insert_msg('LRT')

  ## likelihood ratio test for the adjusted models
  ## compared are the model with the metabolic/inflammatory variable fo interest
  ## and the confounder-only model

  ## univariable models versus null models

  ctss_mod$lrt$uni_models <- ctss_mod$uni_models %>%
    map(~anova(.x$model,
               polr(formula = ctss_class_V0 ~ 1,
                    data = model.frame(.x))))

  ## age/sex-adjusted models

  ctss_mod$lrt$adj_models <- ctss_mod$adj_models %>%
    map(~anova(.x$model,
               polr(formula = ctss_class_V0 ~ age_class + sex,
                    data = model.frame(.x))))

  ## age/sex/severity-adjusted models

  ctss_mod$lrt$full_models <- ctss_mod$full_models %>%
    map(~anova(.x$model,
               polr(formula = ctss_class_V0 ~ age_class + sex + severity,
                    data = model.frame(.x))))

  ## summaries

  ctss_mod$lrt_summary <- ctss_mod$lrt %>%
    map(~map(.x, as.data.frame) %>%
          map(~.x[2, ]) %>%
          compress(names_to = 'variable')) %>%
    map(as_tibble)

# Model inference -------

  insert_msg('Model inference')

  ## as OR

  ctss_mod$inference <-
    ctss_mod[c('conf_models',
               'uni_models',
               'adj_models',
               'full_models')] %>%
    map(~map(.x, summary, 'inference') %>%
          map(mutate,
              or = exp(estimate),
              lower_or = exp(lower_ci),
              upper_or = exp(upper_ci)))

# Forest plots of the model OR, confounders ---------

  insert_msg('Forest plots of the model OR, confounders')

  ctss_mod$conf_forest <- ctss_mod$inference$conf_models %>%
    map_dfr(filter, variable %in% ctss_mod$confounders) %>%
    mutate(variable = exchange(variable,
                               dict = globals$var_lexicon,
                               key = 'variable',
                               value = 'axis_label'),
           variable = stri_replace(variable,
                                   fixed = ' severity',
                                   replacement = ''))

  ctss_mod$conf_forest <- ctss_mod$conf_forest %>%
    mutate(variable = factor(variable,
                             unique(ctss_mod$conf_forest$variable))) %>%
    plot_forest(estimate = 'or',
                lower_ci = 'lower_or',
                upper_ci = 'upper_or',
                plot_title = paste('Age, sex, COVID-19 severity',
                                   'and CT lung lesion severity, 60d FUP'),
                plot_subtitle = paste('complete: n =',
                                      ctss_mod$conf_forest$n_complete[[1]]),
                x_lab = 'OR, 95% CI',
                cutpoint = 1,
                cust_theme = globals$common_theme) +
    scale_x_continuous(trans = 'log2')

# Forest plots for the remaining explanatory variables --------

  insert_msg('Forest plot of the model OR, expl. variables')

  ## plotting data
  ## extracting the transformaiton, translating variables
  ## and setting them in a righ order

  ctss_mod$expl_forest_data <- ctss_mod$inference[c('uni_models',
                                                    'adj_models',
                                                    'full_models')] %>%
    map(~map_dfr(.x, filter, variable %in% ctss_mod$variables)) %>%
    map(mutate,
        source_var = factor(variable, ctss_mod$variables),
        transformation = stri_extract(variable, regex = 'log|sqrt'),
        variable = stri_replace(variable,
                                regex = '(log_)|(sqrt_)',
                                replacement = ''),
        variable = exchange(variable,
                            dict = globals$var_lexicon,
                            key = 'variable',
                            value = 'axis_label'),
        level = ifelse(level == 'yes',
                       stri_split_fixed(variable,
                                        pattern = ', ',
                                        simplify = TRUE)[, 3],
                       level),
        variable = ifelse(is.na(level) | level == '',
                          variable,
                          stri_replace(variable,
                                       fixed = level,
                                       replacement = '')),
        variable = stri_replace(variable,
                                regex = ',\\s{1}$',
                                replacement = ''),
        variable = ifelse(!is.na(transformation),
                          paste(transformation, variable),
                          variable)) %>%
    map(~mutate(.x,
                variable = factor(variable, .x$variable)))

  ## Forest plots

  ctss_mod$expl_forest <-
    list(x = ctss_mod$expl_forest_data,
         plot_title = c('Uni-variable OR, 60d FUP',
                        'Sex/age-adjusted OR, 60d FUP',
                        'Sex/age/severity-adjusted OR, 60d FUP'),
         plot_subtitle = map(ctss_mod$expl_forest_data,
                             ~paste('complete: n =', .x$n_complete[1]))) %>%
    pmap(plot_forest,
         estimate = 'or',
         lower_ci = 'lower_or',
         upper_ci = 'upper_or',
         x_lab = 'OR, 95% CI',
         cutpoint = 1,
         cust_theme = globals$common_theme)

  ## the Y axis labels in the right order

  for(i in names(ctss_mod$expl_forest)) {

   ctss_mod$expl_forest[[i]]$data <- ctss_mod$expl_forest[[i]]$data %>%
     mutate(y_ax = stri_replace(y_ax,
                                fixed = ': \n',
                                replacement = '\n'),
            plot_order = as.numeric(variable))

  }

  ## log X scale

  ctss_mod$expl_forest <- ctss_mod$expl_forest %>%
    map(~.x + scale_x_continuous(trans = 'log2'))

# Publication ready table with the ORs -------

  insert_msg('Table with the results')

  ## inference for the confounders

  ctss_mod$conf_results <- ctss_mod$inference$conf_models %>%
    map_dfr(filter, variable %in% ctss_mod$confounders) %>%
    mutate(significance = ifelse(p_value < 0.05,
                                 paste('p =', signif(p_value, 2)),
                                 paste0('ns (p = ', signif(p_value, 2), ')')),
           or = paste0('OR = ', signif(or, 2), ' [',
                       signif(lower_or, 2), ' - ', signif(upper_or, 2), 2, ']'),
           n_lab = paste0('strata: n = ', n,
                          '\ncomplete: n = ', n_complete),
           variable = exchange(variable,
                               dict = globals$var_lexicon,
                               key = 'variable',
                               value = 'axis_label'),
           or = paste(or, significance, n_lab, sep = '\n')) %>%
    select(variable, level, or) %>%
    set_names(c('Variable', 'Strata', 'OR'))

  ## the remaining modeling results

  ctss_mod$model_results <- ctss_mod$inference[c('uni_models',
                                                 'adj_models',
                                                 'full_models')] %>%
    map(~map_dfr(.x, filter, variable %in% ctss_mod$variables)) %>%
    map(mutate,
        significance = ifelse(p_value < 0.05,
                              paste('p =', signif(p_value, 2)),
                              paste0('ns (p = ', signif(p_value, 2), ')')),
        or = paste0('OR = ', signif(or, 2), ' [',
                    signif(lower_or, 2), ' - ', signif(upper_or, 2), 2, ']'),
        n_lab = ifelse(is.na(level),
                       paste('complete: n =', n),
                       paste0('strata: n = ', n,
                              '\ncomplete: n = ', n_complete)),
        transformation = stri_extract(variable, regex = 'log|sqrt'),
        variable = stri_replace(variable,
                                regex = '(log_)|(sqrt_)',
                                replacement = ''),
        variable = exchange(variable,
                            dict = globals$var_lexicon,
                            key = 'variable',
                            value = 'axis_label'),
        or = paste(or, significance, n_lab, sep = '\n'),
        level = ifelse(level == 'yes',
                       stri_split_fixed(variable,
                                        pattern = ', ',
                                        simplify = TRUE)[, 3]),
        variable = ifelse(is.na(level) | level == '',
                          variable,
                          stri_replace(variable,
                                       fixed = level,
                                       replacement = '')),
        variable = stri_replace(variable,
                                regex = ',\\s{1}$',
                                replacement = ''),
        variable = ifelse(is.na(transformation),
                          variable,
                          paste(transformation, variable))) %>%
    map(select,
        variable, level, or)

  ctss_mod$model_results <- ctss_mod$model_results %>%
    reduce(left_join, by = c('variable', 'level')) %>%
    set_names(c('Variable',
                'Strata',
                'Unadjusted OR',
                'Age/sex-adjusted OR',
                'Age/sex/severity-adjusted OR'))

# END ------

  rm(i)

  insert_tail()

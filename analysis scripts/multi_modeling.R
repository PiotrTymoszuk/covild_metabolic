# Multi-parameter modeling of CTSS severity class with
# demographic (age, sex), CoV severity, inflammatory (CRP, IL6, DDimer, FT)
# and metabolic variables (TG, HDL, ADIPOQ, LEP, obesity, dyslipidemia
# and dysglycemia) at the V0 - V3 visits
#
# The full models are optimized by AIC-driven backwards elimination.
# Their reproducibility and parameterization is checked by 10-fold CV

  insert_head()

# container -------

  multi_mod <- list()

# analysis globals ------

  insert_msg('Analysis globals')

  ## explanatory variables

  multi_mod$variables <- c('age_class', 'sex', 'severity',
                           'CRP_class_V0',
                           'IL6_class_V0',
                           'log_DDimer_V0',
                           'log_FT_V0',
                           'TG_class_V0',
                           'log_HDL_V0',
                           'log_ADIPOQ_V0',
                           'log_LEP_V0',
                           'obesity',
                           'dyslipidemia_V0',
                           'dysglycemia_V0')

  multi_mod$responses <- c(V0 = 'ctss_class_V0',
                           V1 = 'ctss_class_V1',
                           V2 = 'ctss_class_V2',
                           V3 = 'ctss_class_V3')

  ## analysis tables

  multi_mod$analysis_tbl <- covild$data %>%
    transmute(patient_id = patient_id,
              ctss_class_V0 = ctss_class_V0,
              ctss_class_V1 = ctss_class_V1,
              ctss_class_V2 = ctss_class_V2,
              ctss_class_V3 = ctss_class_V3,
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
              dysglycemia_V0 = dysglycemia_V0)

  multi_mod$analysis_tbl <- multi_mod$responses %>%
    map(~multi_mod$analysis_tbl[c('patient_id', .x, multi_mod$variables)]) %>%
    map(~filter(.x, complete.cases(.x)))

  ## timepoint-specific variable sets: IL6 seems not to work for the
  ## 180d (V2) timepoint

  multi_mod$var_list <-
    list(V0 = multi_mod$variables,
         V1 = multi_mod$variables,
         V2 = multi_mod$variables[multi_mod$variables != 'IL6_class_V0'],
         V3 = multi_mod$variables)

# construction of the models and optimization -------

  insert_msg('Construction of the models')

  ## confounder-only models

  multi_mod$conf_models <- list(data = multi_mod$analysis_tbl,
                                response = multi_mod$responses) %>%
    pmap(make_lm,
         indep_variable = c('age_class', 'sex', 'severity'),
         mod_fun = polr,
         family = NULL)

  ## full ordinal logit regression models built by polr

  multi_mod$full_models <- list(data = multi_mod$analysis_tbl,
                                response = multi_mod$responses,
                                indep_variable = multi_mod$var_list) %>%
    pmap(make_lm,
         mod_fun = polr,
         family = NULL)

  ## AIC-driven elimination

  multi_mod$step_models <- multi_mod$full_models %>%
    map(step.lm_analysis,
        step_fun = MASS::stepAIC,
        direction = 'backward')

# Assumption check -------

  insert_msg('Checking the assumptions')

  ## normality and homogeneity of residuals (Shapiro-Wilk and Levene test)
  ## odds-proportionality checked by Brant test

  multi_mod$assumptions <- multi_mod$step_models %>%
    map(summary, 'assumptions')

  ## plots of residuals

  multi_mod$resid_plots <- multi_mod$step_models %>%
    map(plot, cust_theme = globals$common_theme)

# LRT against age/sex/severity-only model -------

  insert_msg('LRT')

  multi_mod$lrt <-
    map2(multi_mod$step_models,
         multi_mod$conf_models,
         ~anova(.x$model, .y$model))

  multi_mod$lrt_summary <- multi_mod$lrt %>%
    map(as.data.frame) %>%
    map(~.x[2, ]) %>%
    compress(names_to = 'timepoint') %>%
    as_tibble

# Fit stats -------

  insert_msg('Fit stats')

  multi_mod$fit_stats <- multi_mod$step_models %>%
    map_dfr(summary, 'fit')

# Cross-validation -------

  insert_msg('Cross-validation')

  registerDoParallel(cores = 7)

  multi_mod$caret_models <- multi_mod$step_models %>%
    map(~train(form = formula(.x),
               data = model.frame(.x),
               method = 'polr',
               tuneGrid = data.frame(method = 'logistic'),
               trControl = trainControl(method = 'repeatedcv',
                                        number = 10,
                                        savePredictions = 'final',
                                        returnData = TRUE,
                                        returnResamp = 'final',
                                        classProbs = TRUE))) %>%
    map(as_caretx)

  stopImplicitCluster()

# Cross-validation stats -------

  insert_msg('Cross-validation stats')

  plan('multisession')

  multi_mod$caret_stats <- multi_mod$caret_models %>%
    future_map(summary.caretx,
               .options = furrr_options(seed = TRUE))

  plan('sequential')

  multi_mod$caret_stats <- multi_mod$caret_stats %>%
    map(compress, names_to = 'model_type') %>%
    compress(names_to = 'timepoint')

# Plots of the cross-validation stats -------

  insert_msg('Plots of the corss-validation stats')

  multi_mod$cv_plots <-
    list(fct = c('class_error',
                 'correct_rate',
                 'kappa'),
         title = c('Classification error',
                   'Accuracy',
                   "Cohen's \u03BA"),
         x_lab = list('missclassified cases, % of total',
                      'correctly classified, % of total',
                      expression(kappa))) %>%
    pmap(function(fct, title, x_lab, subtitle) multi_mod$caret_stats %>%
           filter(statistic == fct) %>%
           ggplot(aes(x = estimate,
                      y = timepoint,
                      fill = model_type)) +
           geom_bar(stat = 'identity',
                    color = 'black',
                    position = position_dodge(0.9)) +
           geom_text(aes(label = signif(estimate, 2)),
                     size = 2.6,
                     color = 'white',
                     hjust = 1.4,
                     position = position_dodge(0.9)) +
           scale_y_discrete(labels = c(V0 = '60d FUP',
                                       V1 = '100d FUP',
                                       V2 = '180d FUP',
                                       V3 = '360d FUP')) +
           scale_fill_manual(values = c(train = 'steelblue',
                                        cv = 'darkolivegreen4'),
                             labels = c(train = 'training',
                                        cv = 'cross-validation'),
                             name = '') +
           globals$common_theme +
           theme(axis.title.y = element_blank()) +
           labs(title = title,
                x = x_lab)) %>%
    set_names(c('class_error', 'correct_rate', 'kappa'))

# Confusion matrices --------

  insert_msg('Confusion matrices')

  multi_mod$conf_mtx <- list(x = multi_mod$caret_models,
                             plot_title = c(V0 = '60d FUP',
                                            V1 = '100d FUP',
                                            V2 = '180d FUP',
                                            V3 = '360d FUP')) %>%
    pmap(plot,
         type = 'confusion',
         cust_theme = globals$common_theme,
         scale = 'percent') %>%
    transpose

# Model inference ------

  insert_msg('Inference of the optimized models')

  multi_mod$inference <-  multi_mod$step_models %>%
    map(summary) %>%
    map(mutate,
        or = exp(estimate),
        lower_or = exp(lower_ci),
        upper_or = exp(upper_ci))

# Forest plot data -------

  insert_msg('Forest plot data')

  multi_mod$forest_data <- multi_mod$inference %>%
    map(filter, variable %in% multi_mod$variables) %>%
    map(mutate,
        source_var = factor(variable, multi_mod$variables),
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
                          variable),
        variable = stri_replace(variable,
                                fixed = ' severity',
                                replacement = '')) %>%
    map(~mutate(.x,
                variable = factor(variable, unique(.x$variable))))

# Forest plots -------

  insert_msg('Forest plots')

  multi_mod$forest_plots <-
    list(x = multi_mod$forest_data,
         plot_title = paste('Lung lesion severity',
                            c('60d FUP',
                              '100d FUP',
                              '180d FUP',
                              '360d FUP'),
                            sep = ', '),
         plot_subtitle = map(multi_mod$forest_data,
                             ~paste('complete: n =', .x$n_complete[1]))) %>%
    pmap(plot_forest,
         estimate = 'or',
         lower_ci = 'lower_or',
         upper_ci = 'upper_or',
         x_lab = 'OR, 95% CI',
         cutpoint = 1,
         estimate_hjust = 0.38,
         estimate_size = 2.6,
         cust_theme = globals$common_theme)

  ## the Y axis labels in the right order

  for(i in names(multi_mod$forest_plots)) {

    multi_mod$forest_plots[[i]]$data <- multi_mod$forest_plots[[i]]$data %>%
      mutate(y_ax = stri_replace(y_ax,
                                 fixed = ': \n',
                                 replacement = '\n'),
             plot_order = as.numeric(variable))

  }

  ## log X scale

  multi_mod$forest_plots <- multi_mod$forest_plots %>%
    map(~.x + scale_x_continuous(trans = 'log2'))

# END ------

  insert_tail()

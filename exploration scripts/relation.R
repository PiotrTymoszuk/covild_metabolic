# Relationship between the candidate modeling variables at the 60 day FUP
# and numeric explanatory variables
# (BMI, age, CRP, IL6, DDimer, TG, HDL-C, adiponectin and leptin) in order to
# discern the need for higher terms and smoothers
#
# Technically, the analysis utilizes the function 'make_lm()' from the lmqc
# package to construct the univariate linear model objects from which
# the response - variable plots are created by calling the 'plot()' method
#
# The analysis indicates that TG, FT and LEP may predict CTSS severity class
# better when passed as a smoother to the model

  insert_head()

# container --------

  resp_rel <- list()

# analysis globals --------

  insert_msg('Analysis globals')

  resp_rel$variables <-
    c('bmi', 'age', 'CRP_V0',
      'IL6_V0', 'DDimer_V0', 'FT_V0', 'TG_V0',
      'HDL_V0', 'ADIPOQ_V0', 'LEP_V0')

  ## analysis table
  ## adding log and sqrt transformations

  resp_rel$analysis_tbl <-
    covild$data[c('ctss_V0', 'opac_V0', 'ctss_class_V0',
                  resp_rel$variables)]

  for(i in resp_rel$variables) {

    resp_rel$analysis_tbl <- resp_rel$analysis_tbl %>%
      mutate(!!paste0('log_', i) := log(.data[[i]]),
             !!paste0('sqrt_', i) := sqrt(.data[[i]]))

  }

  ## constraining the table to the candidate responses
  ## and the variables with the normality-enhancing transformations

  resp_rel$variables <- expl_distr$best_normality$variable

  resp_rel$analysis_tbl <- resp_rel$analysis_tbl %>%
    select('ctss_V0', 'opac_V0', 'ctss_class_V0',
           all_of(resp_rel$variables)) %>%
    mutate(ctss_class_V0 = as.numeric(ctss_class_V0))

  ## modeling formulas for linear models, quadratic models and GAMs

  resp_rel$lm_formulas <- c(ctss_V0 = 'ctss_V0',
                            opac_V0 = 'opac_V0',
                            ctss_class_V0  = 'ctss_class_V0') %>%
    map(function(resp) resp_rel$variables %>%
          map(~paste(resp, .x, sep = ' ~ ')) %>%
          map(as.formula) %>%
          set_names(resp_rel$variables))

  resp_rel$quad_formulas <- c(ctss_V0 = 'ctss_V0',
                              opac_V0 = 'opac_V0',
                              ctss_class_V0  = 'ctss_class_V0') %>%
    map(function(resp) resp_rel$variables %>%
          map(~paste0(resp, ' ~ ',
                      .x,
                      '+ I(', .x, '^2) + I(', .x, '^3)')) %>%
          map(as.formula) %>%
          set_names(resp_rel$variables))

  resp_rel$gam_formulas <- c(ctss_V0 = 'ctss_V0',
                            opac_V0 = 'opac_V0',
                            ctss_class_V0  = 'ctss_class_V0') %>%
    map(function(resp) resp_rel$variables %>%
          map(~paste0(resp, ' ~ s(', .x, ', bs = "cs")')) %>%
          map(as.formula) %>%
          set_names(resp_rel$variables))

# Construction of linear models --------

  insert_msg('Model construction')

  ## linear models

  resp_rel$lm_models <- resp_rel$lm_formulas %>%
    map(~map(.x,
             ~make_lm(data = resp_rel$analysis_tbl,
                      formula = .x,
                      mod_fun = lm,
                      family = NULL)))

  ## second-order linear models

  resp_rel$quad_models <- resp_rel$quad_formulas %>%
    map(~map(.x,
             ~make_lm(data = resp_rel$analysis_tbl,
                      formula = .x,
                      mod_fun = lm,
                      family = NULL)))

  ## GAM models

  resp_rel$gam_models <- resp_rel$gam_formulas %>%
    map(~map(.x,
             ~make_lm(data = resp_rel$analysis_tbl,
                      formula = .x,
                      mod_fun = gam,
                      family = NULL)))

# Comparing the fit stats between the lm and gam models ------

  insert_msg('Comparing the fit stats between the lm and GAM models')

  ## actually the only parameter to profit from smoothing is log TG

  resp_rel$fit_stats$lm <- resp_rel$lm_models %>%
    map(~map(.x, summary, type = 'fit')) %>%
    map(compress, names_to = 'variable') %>%
    map(mutate, method = 'lm')

  resp_rel$fit_stats$quad <- resp_rel$quad_models %>%
    map(~map(.x, summary, type = 'fit')) %>%
    map(compress, names_to = 'variable') %>%
    map(mutate, method = 'lm')

  resp_rel$fit_stats$gam <- resp_rel$gam_models %>%
    map(~map(.x, summary, type = 'fit')) %>%
    map(compress, names_to = 'variable') %>%
    map(mutate, method = 'gam')

  resp_rel$fit_stats <- resp_rel$fit_stats %>%
    transpose

# Plotting the R-squares and RMSE values for the modeling scenarios -------

  insert_msg('Plotting the the fit stats for various model scenarios')

  ## plotting for the CTSS severity class as the most promising modeling
  ## candidate variable

  resp_rel$ctss_class_plots <-
    list(var = c('raw_rsq', 'rmse'),
         x_lab = list(expression('R'^2), 'RMSE')) %>%
    pmap(function(var, x_lab) resp_rel$fit_stats$ctss_class_V0 %>%
           compress(names_to = 'model_type') %>%
           mutate(base_var = stri_replace(variable,
                                          regex = '(log_)|(sqrt_)',
                                          replacement = ''),
                  transformation = stri_extract(variable,
                                                regex = '(log)|(sqrt)'),
                  var_lab = exchange(base_var,
                                     dict = globals$var_lexicon,
                                     key = 'variable',
                                     value = 'label'),
                  var_lab = ifelse(is.na(transformation),
                                   var_lab,
                                   paste(transformation, var_lab)),
                  model_type = factor(model_type,
                                      c('gam', 'quad', 'lm'))) %>%
           ggplot(aes(x = .data[[var]],
                      y = var_lab,
                      fill = model_type)) +
           geom_bar(stat = 'identity',
                    color = 'black',
                    position = position_dodge(0.9)) +
           scale_fill_brewer(type = 'div',
                             labels = c(lm = 'first order',
                                        quad = 'cubic',
                                        gam = 'GAM'),
                             name = 'Explanatory variable') +
           globals$common_theme +
           theme(axis.title.y = element_blank()) +
           labs(title = 'CTSS class - explanatory variable relationship',
                subtitle = 'Comparison of univariable fits',
                x = x_lab)) %>%
    set_names(c('rsq', 'rmse'))

# Plotting the relationships ------

  insert_msg('Plotting the relationships')

  resp_rel$rel_plots <- resp_rel$lm_models %>%
    map(~map(.x,
             plot,
             type = 'relationship',
             cust_theme = globals$common_theme,
             method = 'loess',
             point_hjitter = 0.05))

# END -----

  rm(i)

  insert_tail()

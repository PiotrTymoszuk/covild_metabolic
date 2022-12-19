# Checking the relationship between the CTSS and automatically determined
# percentage of opacity at consecutive study visit (acute visit is ignored)
#
# As indicated by the modeling results, the opacity: CTSS relationship is best
# characterized by a GAM-smoother but not by the linear or second-order function

  insert_head()

# container -----

  rater <- list()

# Analysis globals -------

  insert_msg('Analysis globals')

  ## analysis tables, split by the visit, in a long format

  rater$analysis_tbl <-
    list(V0 = c('ctss_V0', 'opac_V0', 'ctss_class_V0'),
         V1 = c('ctss_V1', 'opac_V1', 'ctss_class_V1'),
         V2 = c('ctss_V2', 'opac_V2', 'ctss_class_V2')) %>%
    map(~covild$data[c('patient_id', .x)]) %>%
    map(~filter(.x, complete.cases(.x))) %>%
    map(set_names, c('patient_id', 'ctss', 'opac', 'ctss_class'))

# Correlation analysis ------

  insert_msg('Correlation analysis')

  ## Spearman's correlation

  rater$cor_test <- rater$analysis_tbl %>%
    map_dfr(~correlate_variables(.x,
                                 variables = c('ctss', 'opac'),
                                 what = 'correlation',
                                 type = 'spearman',
                                 ci = TRUE,
                                 pub_styled = TRUE)) %>%
    mutate(plot_cap = stri_replace(eff_size,
                                   fixed = 'rho',
                                   replacement = '\u03C1'),
           plot_cap = paste(eff_size, significance, sep = ', '),
           plot_cap = paste(plot_cap, n, sep = ', n = '))

# Plotting of the correlations -------

  insert_msg('Plotting')

  rater$cor_plots <- list(data = rater$analysis_tbl,
                     plot_title = c('60d FUP',
                                    '100d FUP',
                                    '180d FUP'),
                     plot_subtitle =  rater$cor_$plot_cap) %>%
    pmap(plot_correlation,
         variables = c('ctss', 'opac'),
         type = 'correlation',
         point_hjitter = 0.1,
         point_wjitter = 0.1,
         cust_theme = globals$common_theme,
         y_lab = '% opactity',
         x_lab = 'CTSS',
         show_trend = FALSE,
         show_labels = FALSE) %>%
    map(~.x +
          scale_y_continuous(trans = 'pseudo_log') +
          geom_smooth(method = 'gam',
                      #formula = y ~ x + I(x^2),
                      se = TRUE))

# Second-order linear modeling of opacity as a function of CTSS ------

  insert_msg('Second order linear and GAM modeling of opacity')

  ## model objects

  rater$gam_models <- rater$analysis_tbl %>%
    map(~make_lm(data = .x,
                 formula = opac ~ s(ctss, bs = 'cs'),
                 mod_fun = gam,
                 family = NULL))

  rater$lm_models <- rater$analysis_tbl %>%
    map(~make_lm(data = .x,
                 formula = opac ~ ctss + I(ctss^2),
                 mod_fun = lm,
                 family = NULL))

  ## normality of the residuals: poor normality

  rater$gam_assumptions <- rater$gam_models %>%
    map(summary,
        type = 'assumptions',
        type.predict = 'response') %>%
    compress(names_to = 'timepoint')

  rater$lm_assumptions <- rater$lm_models %>%
    map(summary,
        type = 'assumptions',
        type.predict = 'response') %>%
    compress(names_to = 'timepoint')

  ## fit stats: substantially better for the GAM models

  rater$gam_fit_stats <- rater$gam_models %>%
    map(summary,
        type = 'fit') %>%
    compress(names_to = 'timepoint')

  rater$lm_fit_stats <- rater$lm_models %>%
    map(summary,
        type = 'fit') %>%
    compress(names_to = 'timepoint')

  ## inference

  rater$gam_inference <- rater$gam_models %>%
    map(summary, type = 'inference')

  rater$lm_inference <- rater$lm_models %>%
    map(summary, type = 'inference')

# Opacity in the CT severity classes, descriptive stats ------

  insert_msg('Descriptive stats')

  rater$desc_stats <- rater$analysis_tbl %>%
    map(~explore(.x,
                 split_factor = 'ctss_class',
                 variables = 'opac',
                 what = 'table',
                 pub_styled = TRUE)) %>%
    map(reduce,
        left_join,
        by = 'variable') %>%
    map(set_names,
        c('variable', levels(rater$analysis_tbl[[1]]$ctss_class))) %>%
    compress(names_to = 'timepoint')

# Testing for differences in percentage opacity between the CTSS classes -----

  insert_msg('Testing for % opacity differences between CTSS classes')

  ## Kruskal-Wallis test

  rater$class_test <- rater$analysis_tbl %>%
    map(~compare_variables(.x,
                           variables = 'opac',
                           split_factor = 'ctss_class',
                           what = 'eff_size',
                           types = 'kruskal_eta',
                           exact = FALSE,
                           ci = FALSE,
                           pub_styled = TRUE)) %>%
    compress(names_to = 'timepoint') %>%
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))

# Plots of the opacity in the CTSS classes -----

  insert_msg('Plots of opacity in the CTSS classes')

  rater$class_plots <-
    list(data = rater$analysis_tbl,
         title = c('60d FUP',
                   '100d FUP',
                   '180d FUP'),
         sub = rater$class_test$plot_cap) %>%
    pmap(function(data, title, sub) data %>%
           plot_variable(variable = 'opac',
                         split_factor = 'ctss_class',
                         type = 'box',
                         point_hjitter = 0,
                         cust_theme = globals$common_theme,
                         plot_title = title,
                         plot_subtitle = sub,
                         x_lab = 'CTSS class',
                         y_lab = 'Opacity, % of lung')) %>%
    map(~.x +
          scale_y_continuous(trans = 'pseudo_log') +
          scale_fill_brewer(type = 'div'))

# END -----

  insert_tail()

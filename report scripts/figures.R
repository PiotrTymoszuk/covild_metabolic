# Report figures

  insert_head()

# container ------

  figures <- list()

# Figure 1: Forest plots for CT lesion severity at the day 60 FUP, confounders -----

  insert_msg('Figure 1: CT severity modeling at 60d FUP, confounders')

  figures$confounder_V0 <- ctss_mod$conf_forest %>%
    as_figure(label = 'figure_1_ctss_modeling_counfounder',
              ref_name = 'confounder_V0',
              caption = paste('Results of logistic ordinal modeling of',
                              'chest CT abnormality severity at the',
                              '60-day follow-up as a function of age, sex',
                              'and COVID-19 severity'),
              w = 140,
              h = 80)

# Figure 2: Forest plots for CT lesion severity at 60d FUP, other variables ------

  insert_msg('Figure 2: CT severity modeling at 60d FUP, variables')

  figures$explanatory_V0 <- ctss_mod$expl_forest %>%
    map(~.x + theme(legend.position = 'none')) %>%
    c(list(get_legend(ctss_mod$expl_forest[[1]]))) %>%
    plot_grid(plotlist = .,
              ncol = 2,
              align = 'hv',
              axis = 'tblr',
              labels = c('A', 'B', 'C'),
              label_size = 10) %>%
    as_figure(label = 'figure_2_ctss_modeling_explanatory',
              ref_name = 'explanatory_V0',
              caption = paste('Results of logistic ordinal modeling of',
                              'chest CT abnormality severity at the',
                              '60-day follow-up as a function of inflammatory',
                              'and metabolic parameters.'),
              w = 180,
              h = 225)

# Figure 3: correlations ------

  insert_msg('Figure 3: correlations between inflammatory/metabolic paramaters')

  figures$correlations <- corr$bubble_plots %>%
    map(~.x + theme(legend.position = 'none')) %>%
    plot_grid(plotlist = .,
              ncol = 2,
              align = 'hv',
              axis = 'tblr',
              labels = LETTERS,
              label_size = 10) %>%
    plot_grid(get_legend(corr$bubble_plots[[1]] +
                           theme(legend.position = 'bottom')),
              nrow = 2,
              rel_heights = c(0.9, 0.1)) %>%
    as_figure(label = 'figure_3_correlation',
              ref_name = 'correlations',
              caption = paste('Correlation of metabolic and inflammatory',
                              'parameters at the 60-, 100-, 180-',
                              'and 360-day follow-up.'),
              w = 180,
              h = 180)

# Figure 4 - 7: time-course of inflammatory and metabolism ------

  insert_msg('Figures 4 - 7: time course of inflammation')

  figures[c('inflammation_tc_dyslipi',
            'metabolic_tc_dyslipi',
            'inflammation_tc_dysgly',
            'metabolic_tc_dysgly')] <-
    list(tc$box_plots$strat_dyslipi[c('CRP', 'IL6', 'DDimer', 'FT')] %>%
           map2(c('log2', 'log2', 'log2', 'identity'),
                ~.x +
                  scale_y_continuous(trans = .y,
                                     labels = function(x) signif(x, 2))),
         tc$box_plots$strat_dyslipi[c('TG', 'HDL', 'ADIPOQ', 'LEP')] %>%
           map2(c('log2', 'identity', 'log2', 'identity'),
                ~.x +
                  scale_y_continuous(trans = .y,
                                     labels = function(x) signif(x, 2))),
         tc$box_plots$strat_dysgly[c('CRP', 'IL6', 'DDimer', 'FT')] %>%
           map2(c('log2', 'log2', 'log2', 'identity'),
                ~.x +
                  scale_y_continuous(trans = .y,
                                     labels = function(x) signif(x, 2))),
         tc$box_plots$strat_dysgly[c('TG', 'HDL', 'ADIPOQ', 'LEP')] %>%
           map2(c('log2', 'identity', 'log2', 'identity'),
                ~.x +
                  scale_y_continuous(trans = .y,
                                     labels = function(x) signif(x, 2)))) %>%
    map(~map(.x, ~.x + theme(legend.position = 'none'))) %>%
    map(~plot_grid(plotlist = .x,
                   ncol = 2,
                   align = 'hv',
                   axis = 'tblr',
                   labels = LETTERS,
                   label_size = 10)) %>%
    map2(.,
         list(tc$box_plots$strat_dyslipi[[1]],
              tc$box_plots$strat_dyslipi[[1]],
              tc$box_plots$strat_dysgly[[1]],
              tc$box_plots$strat_dysgly[[1]]),
         ~plot_grid(.x,
                    get_legend(.y),
                    ncol = 2,
                    rel_widths = c(0.85, 0.15)))

  figures[c('inflammation_tc_dyslipi',
            'metabolic_tc_dyslipi',
            'inflammation_tc_dysgly',
            'metabolic_tc_dysgly')] <- figures[c('inflammation_tc_dyslipi',
                                                  'metabolic_tc_dyslipi',
                                                  'inflammation_tc_dysgly',
                                                  'metabolic_tc_dysgly')] %>%
    list(x = .,
         label = c('figure_4_inflammation_dyslipidemia',
                   'figure_5_metabolism_dyslipidemia',
                   'figure_6_inflammation_dysglycemia',
                   'figure_7_metabolism_dysglycemia'),
         ref_name = c('inflammation_tc_dyslipi',
                      'metabolic_tc_dyslipi',
                      'inflammation_tc_dysgly',
                      'metabolic_tc_dysgly'),
         caption = c(paste('Time course of inflammatory parameters',
                           'at the 60-, 100-, 180- and 360-day follow-up',
                           'in study participants with normal lipid',
                           'profile and dyslipidemia at the 60-day visit.'),
                     paste('Time course of metabolic parameters',
                           'at the 60-, 100-, 180- and 360-day follow-up',
                           'in study participants with normal lipid',
                           'profile and dyslipidemia at the 60-day visit.'),
                     paste('Time course of inflammatory parameters',
                           'at the 60-, 100-, 180- and 360-day follow-up',
                           'in study participants with normal glucose',
                           'and dysglycemia at the 60-day visit.'),
                     paste('Time course of metabolic parameters',
                           'at the 60-, 100-, 180- and 360-day follow-up',
                           'in study participants with normal glucose',
                           'and dysglycemia at the 60-day visit.'))) %>%
    pmap(as_figure,
         w = 180,
         h = 160)

# Figure 8: time course of CTSS -----

  insert_msg('Figure 8: CTSS kinetics')

  figures$ctss_tc <- list(tc$box_plots$strat_dyslipi$ctss,
                          tc$box_plots$strat_dysgly$ctss) %>%
    map(~.x + theme(legend.position = 'bottom')) %>%
    plot_grid(plotlist = .,
              ncol = 2,
              align = 'hv',
              labels = LETTERS,
              label_size = 10) %>%
    as_figure(label = 'figure_8_ctss_kinetic',
              ref_name = 'ctss_tc',
              caption = paste('Time course of chest CT severity score',
                              'at the 60-, 100-, 180- and 360-day follow-up',
                              'in study participants with normal metabolic',
                              'profile, dyslipidemia and dysglycemia',
                              'at the 60-day visit.'),
              w = 180,
              h = 100)

# Figure 9: fit stats of the multi-parameter models -------

  insert_msg('Figure 9: fit stats')

  figures$fit_stats <- multi_mod$cv_plots[c('class_error',
                                            'kappa')] %>%
    map(~.x + theme(legend.position = 'none')) %>%
    plot_grid(plotlist = .,
              ncol = 2,
              align = 'hv',
              labels = LETTERS,
              label_size = 10) %>%
    plot_grid(get_legend(multi_mod$cv_plots[[1]] +
                           theme(legend.position = 'bottom')),
              nrow = 2,
              rel_heights = c(0.9, 0.1)) %>%
    as_figure(label = 'figure_9_multi_model_fit_stats',
              ref_name = 'fit_stats',
              caption = paste('Accuracy of chest CT abnormality',
                              'severity prediction by multi-parameter ordinal',
                              'logistic modeling at 60-, 100-, 180-',
                              'and 360-day follow-up.'),
              w = 180,
              h = 90)

# Figure 10: backwards elimination modeling of CTSS -----

  insert_msg('Figure 10: backwards elimination modeling of CT severity')

  figures$backward <- multi_mod$forest_plots %>%
    map(~.x + theme(legend.position = 'none')) %>%
    plot_grid(plotlist = .,
              ncol = 2,
              align = 'hv',
              labels = LETTERS,
              label_size = 10) %>%
    plot_grid(get_legend(multi_mod$forest_plots[[1]] +
                           theme(legend.position = 'bottom')),
              nrow = 2,
              rel_heights = c(0.92, 0.08)) %>%
    as_figure(label = 'figure_10_multi_model_ors',
              ref_name = 'backward',
              caption = paste('Results of multi-parameter logistic ordinal',
                              'modeling of CT lung abnormality severity at the',
                              '60-, 100-, 180- and 360-day follow-up as a',
                              'function of age, sex, COVID-19 severity,',
                              'and inflammatory and metabolic parameters.'),
              w = 180,
              h = 210)

# Saving figures on the disc -------

  insert_msg('Saving figures on the disc')

  figures %>%
    walk(pickle,
         path = './report/figures pdf',
         format = 'pdf',
         device = cairo_pdf)

  figures %>%
    walk(pickle,
         path = './report/figures eps',
         format = 'eps',
         device = cairo_ps)

# END ------

  insert_tail()

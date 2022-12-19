# Correlation analysis of metabolic variables (TG, HDL, ADIPOQ, LEP) with
# markers of inflammation (IL6, DDimer, CRP, FT)

  insert_head()

# container ------

  corr <- list()

# globals ------

  insert_msg('Analysis globals')

  ## metabolic and inflammatory variables

  corr$met_vars <- c('TG', 'HDL', 'ADIPOQ', 'LEP')

  corr$infl_vars <- c('CRP', 'IL6', 'DDimer', 'FT')

  ## variable pairs

  corr$pairs <- corr$met_vars %>%
    map(function(x) corr$infl_vars %>%
          map(~c(x, .x))) %>%
    unlist(recursive = FALSE)

  ## analysis tables, one per timepoint

  corr$analysis_tables <- c(V0 = 'V0',
                            V1 = 'V1',
                            V2 = 'V2',
                            V3 = 'V3') %>%
    map(~paste(c(corr$met_vars, corr$infl_vars), .x, sep = '_')) %>%
    map(~covild$data[.x]) %>%
    map(~filter(.x, complete.cases(.x))) %>%
    map(set_names, c(corr$met_vars, corr$infl_vars))

# correlation analysis ------

  insert_msg('Correlation')

  ## done with Spearman's test, since multiple variables
  ## are non-normally distributed

  corr$test <- corr$analysis_tables %>%
    map(function(data) corr$pairs %>%
          map_dfr(~correlate_variables(data,
                                   variables = .x,
                                   what = 'correlation',
                                   type = 'spearman',
                                   ci = TRUE,
                                   pub_styled = FALSE))) %>%
    map(mutate,
        plot_txt = signif(estimate, 2),
        significant = ifelse(p_adjusted < 0.05, 'yes', 'no'),
        txt_face = ifelse(significant == 'yes', 'bold', 'plain'),
        variable1 = factor(variable1, corr$met_vars),
        variable2 = factor(variable2, corr$infl_vars))

# Bubble plots of correlation coefficients ------

  insert_msg('Bubble plots')

  corr$bubble_plots <- list(data = corr$test,
                            title = c('60d FUP',
                                      '100d FUP',
                                      '180d FUP',
                                      '360d FUP'),
                            subtitle = paste('n =',
                                             map_dbl(corr$analysis_tables,
                                                     nrow))) %>%
    pmap(function(data, title, subtitle) data %>%
           ggplot(aes(x = variable2,
                      y = variable1,
                      fill = estimate,
                      size = abs(estimate))) +
           geom_point(shape = 21) +
           geom_text(aes(label = plot_txt,
                         fontface = txt_face,
                         color = significant),
                     size = 2.6,
                     vjust = -1.4) +
           scale_size_area(limits = c(-0.45, 0.45)) +
           scale_fill_gradient2(low = 'steelblue',
                                mid = 'white',
                                high = 'firebrick',
                                midpoint = 0,
                                limits = c(-0.45, 0.45),
                                name = expression(rho)) +
           scale_color_manual(values = c(no = 'gray70',
                                         yes = 'black')) +
           scale_x_discrete(labels = set_names(globals$var_lexicon$label,
                                               globals$var_lexicon$variable)) +
           scale_y_discrete(labels = set_names(globals$var_lexicon$label,
                                               globals$var_lexicon$variable)) +
           guides(color = 'none',
                  size = 'none') +
           globals$common_theme +
           theme(axis.title = element_blank()) +
           labs(title = title,
                subtitle = subtitle))

# END -----

  insert_tail()

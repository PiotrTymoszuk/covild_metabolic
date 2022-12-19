# Accessory functions

  library(plyr)
  library(tidyverse)
  library(stringi)

# kinetic plot -------

  plot_kinetic <- function(data,
                           plot_var = 'median',
                           lower_err = 'lower_q',
                           upper_err = 'upper_q',
                           strata_var = 'strat_dyslipi',
                           title = NULL,
                           subtitle = NULL,
                           tag = NULL,
                           y_lab = plot_var,
                           show_errorbars = FALSE,
                           show_ribbons = TRUE,
                           post_hoc_labs = NULL,
                           txt_size = 2.75,
                           txt_offset = 1.05,
                           dodge_w = 0) {

    ## plots a summary statistic with error bars -----

    kin_plot <- data %>%
      ggplot(aes(x = time,
                 y = .data[[plot_var]],
                 color = .data[[strata_var]]))

    if(show_ribbons) {

      kin_plot <- kin_plot +
        geom_ribbon(aes(ymin = .data[[lower_err]],
                        ymax = .data[[upper_err]],
                        group = .data[[strata_var]],
                        fill = .data[[strata_var]]),
                    alpha = 0.15,
                    position = position_dodge(w = dodge_w))

    }

    if(show_errorbars) {

      kin_plot <- kin_plot +
        geom_errorbar(aes(ymin = .data[[lower_err]],
                          ymax = .data[[upper_err]]),
                      width = 0,
                      position = position_dodge(w = dodge_w))

    }

    kin_plot <- kin_plot +
      geom_line(aes(group = .data[[strata_var]]),
                position = position_dodge(w = dodge_w)) +
      geom_point(shape = 16,
                 size = 2,
                 position = position_dodge(w = dodge_w)) +
      globals$common_theme +
      labs(title = title,
           subtitle = subtitle,
           tag = tag,
           y = y_lab,
           x = 'Follow-up, days')

    ## post_hoc ------

    if(is.null(post_hoc_labs)) return(kin_plot)

    times <- levels(data$time)

    for(i in seq_along(post_hoc_labs)) {

      if(post_hoc_labs[[i]] == '') next

      x_pos <- i

      y_pos <- data %>%
        filter(time == times[[i]])

      y_pos <- max(y_pos[[upper_err]]) * txt_offset

      kin_plot <- kin_plot +
        annotate('text',
                 label = post_hoc_labs[[i]],
                 size = txt_size,
                 x = x_pos,
                 y = y_pos,
                 vjust = 0)

    }

    return(kin_plot)

  }

# box plot --------

  plot_box <- function(data,
                       plot_var = 'value',
                       strata_var = 'strat_dyslipi',
                       title = NULL,
                       subtitle = NULL,
                       tag = NULL,
                       y_lab = plot_var,
                       post_hoc_labs = NULL,
                       txt_size = 2.75,
                       txt_offset = 1.10,
                       line_offset = 1.08,
                       point_alpha = 0.5,
                       point_hjitter = 0,
                       dodge_w = 0.8) {

    ## box plot of the parameter kinetic ------

    box_plot <- data %>%
      ggplot(aes(x = time,
                 y = .data[[plot_var]],
                 fill = .data[[strata_var]])) +
      geom_boxplot(alpha = 0.5,
                   outlier.color = NA,
                   position = position_dodge(dodge_w)) +
      geom_point(aes(group = .data[[strata_var]]),
                 size = 2,
                 shape = 16,
                 alpha = point_alpha,
                 position = position_jitterdodge(jitter.width = 0.1,
                                                 jitter.height = point_hjitter,
                                                 dodge.width = dodge_w),
                 show.legend = FALSE) +
      globals$common_theme +
      labs(title = title,
           subtitle = subtitle,
           tag = tag,
           x = 'Follow-up, days',
           y = y_lab)

    ## post-hoc tests --------

    if(is.null(post_hoc_labs)) return(box_plot)

    times <- levels(data$time)

    for(i in seq_along(post_hoc_labs)) {

      if(post_hoc_labs[[i]] == '') next

      x_pos <- i

      y_pos <- data %>%
        filter(time == times[[i]])

      y_line <- max(y_pos[[plot_var]]) * line_offset

      y_txt <- max(y_pos[[plot_var]]) * txt_offset

      box_plot <- box_plot +
        annotate('segment',
                 x = x_pos - 0.2,
                 xend = x_pos + 0.2,
                 y = y_line,
                 yend = y_line,
                 size = 0.5) +
        annotate('text',
                 label = post_hoc_labs[[i]],
                 size = txt_size,
                 x = x_pos,
                 y = y_txt,
                 vjust = 0)

    }

    return(box_plot)

  }

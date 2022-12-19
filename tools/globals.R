# This script contains project globals

# libraries ----

  library(plyr)
  library(tidyverse)
  library(stringi)

# Data container ------

  globals <- list()

# graphics -----

  globals$common_text <- element_text(size = 8,
                                      face = 'plain',
                                      color = 'black')

  globals$common_margin <- ggplot2::margin(t = 5,
                                           l = 4,
                                           r = 2,
                                           unit = 'mm')

  globals$common_theme <-
    theme_classic() +
    theme(axis.text = globals$common_text,
          axis.title = globals$common_text,
          plot.title = element_text(size = 8,
                                    face = 'bold'),
          plot.subtitle = globals$common_text,
          plot.tag = element_text(size = 8,
                                  face = 'plain',
                                  color = 'black',
                                  hjust = 0,
                                  vjust = 1),
          plot.tag.position = 'bottom',
          legend.text = globals$common_text,
          legend.title = globals$common_text,
          strip.text = globals$common_text,
          strip.background = element_rect(fill = 'gray95',
                                          color = 'gray80'),
          plot.margin = globals$common_margin,
          panel.grid.major = element_line(color = 'gray90'))

# Variables and their labels ------

  insert_msg('Variables, labels and units')

  globals$var_lexicon <-
    list(sex = c('sex', NA),
         bmi = c('BMI', 'kg/m\u00B2'),
         bmi_class = c('body mass class', NA),
         age = c('age', 'years'),
         age_class = c('age', 'years'),
         severity = c('COVID-19 severity', NA),
         abnormal_weight = c('overweight or obsesity', NA),
         obesity = c('obesity', NA),
         diabetes = c('type II diabetes', NA),
         dyslipidemia_V0 = c('dyslipidemia, 60d FUP', NA),
         dyslipidemia_V1 = c('dyslipidemia, 100d FUP', NA),
         dyslipidemia_V2 = c('dyslipidemia, 180d FUP', NA),
         dyslipidemia_V3 = c('dyslipidemia, 360d FUP', NA),
         dysglycemia_V0 = c('dysglycemia, 60d FUP', NA),
         dysglycemia_V1 = c('dysglycemia, 100d FUP', NA),
         dysglycemia_V2 = c('dysglycemia, 180d FUP', NA),
         dysglycemia_V3 = c('dysglycemia, 360d FUP', NA),
         ctss_acute = c('CTSS, acute COVID-19', NA),
         ctss_V0 = c('CTSS, 60d FUP', NA),
         ctss_V1 = c('CTSS, 100d FUP', NA),
         ctss_V2 = c('CTSS, 180d FUP', NA),
         ctss_V3 = c('CTSS, 360d FUP', NA),
         ctss_class_acute = c('Lung lesions, acute COVID-19', NA),
         ctss_class_V0 = c('Lung lesions, 60d FUP', NA),
         ctss_class_V1 = c('Lung lesions, 100d FUP', NA),
         ctss_class_V2 = c('Lung lesions, 180d FUP', NA),
         ctss_class_V3 = c('Lung lesions, 360d FUP', NA),
         opac_acute = c('Opacity, acute COVID-19', '%'),
         opac_V0 = c('Opacity, 60d FUP', '%'),
         opac_V1 = c('Opacity, 100d FUP', '%'),
         opac_V2 = c('Opacity, 180d FUP', '%'),
         CRP_V0 = c('CRP, 60d FUP', 'mg/dL'),
         CRP_class_V0 = c('CRP, 60d FUP', '>0.5 mg/dL'),
         IL6_V0 = c('IL6, 60d FUP', 'pg/mL'),
         IL6_class_V0 = c('IL6, 60d FUP', '>7 pg/mL'),
         FT_V0 = c('FT, 60d FUP', 'ng/mL'),
         FT_class_V0 = c('FT, 60d FUP', '>340/>307 ng/mL'),
         DDimer_V0 = c('DDimer, 60d FUP', 'pg/mL'),
         DDimer_class_V0 = c('DDimer, 60 d FUP', '>500 pg/mL'),
         TG_V0 = c('TG, 60d FUP', 'mg/dL'),
         TG_class_V0 = c('TG, 60d FUP', '>150 mg/dL'),
         TG_V1 = c('TG, 100d FUP', 'mg/dL'),
         TG_class_V1 = c('TG, 100d FUP', '>150 mg/dL'),
         TG_V2 = c('TG, 180d FUP', 'mg/dL'),
         TG_class_V2 = c('TG, 180d FUP', '>150 mg/dL'),
         TG_V3 = c('TG, 360d FUP', 'mg/dL'),
         TG_class_V3 = c('TG, 360d FUP', '>150 mg/dL'),
         HDL_V0 = c('HDL, 60d FUP', 'mg/dL'),
         HDL_class_V0 = c('HDL, 60d FUP', '<40/<50 mg/dL'),
         HDL_V1 = c('HDL, 100d FUP', 'mg/dL'),
         HDL_class_V1 = c('HDL, 100d FUP', '<40/<50 mg/dL'),
         HDL_V2 = c('HDL, 180d FUP', 'mg/dL'),
         HDL_class_V2 = c('HDL, 180d FUP', '<40/<50 mg/dL'),
         HDL_V3 = c('HDL, 360d FUP', 'mg/dL'),
         HDL_class_V3 = c('HDL, 360d FUP', '<40/<50 mg/dL'),
         ADIPOQ_V0 = c('ADIPOQ, 60d FUP', 'ng/mL'),
         ADIPOQ_V1 = c('ADIPOQ, 100d FUP', 'ng/mL'),
         ADIPOQ_V2 = c('ADIPOQ, 180d FUP', 'ng/mL'),
         ADIPOQ_V3 = c('ADIPOQ, 360d FUP', 'ng/mL'),
         LEP_V0 = c('LEP, 60d FUP', 'ng/mL'),
         LEP_V1 = c('LEP, 100d FUP', 'ng/mL'),
         LEP_V2 = c('LEP, 180d FUP', 'ng/mL'),
         LEP_V3 = c('LEP, 360d FUP', 'ng/mL')) %>%
    compress(names_to = 'variable',
             values_to = 'lab_lst') %>%
    mutate(label = map_chr(lab_lst, ~.x[[1]]),
           unit = map_chr(lab_lst, ~.x[[2]]),
           axis_label = ifelse(is.na(unit),
                               label,
                               paste(label, unit, sep = ', ')))

# END -----

  insert_tail()

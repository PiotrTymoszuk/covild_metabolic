# Analysis of the study data as specified in the revised manuscript
#
# 1) Ordinal modeling of lung lesion severity at the day 60 visit in
# a univariable setting, with adjustment for age and sex and with adjustment
# for age, sex and CoV severity
#
# 2) Correlation analysis of the inflammatory (IL6, DDimer, CRP, FT)
# and metabolic markers (TG, HDL, ADIPOQ, LEP) for each time point
#
# 3) Analysis of time changes of thrombo-inflammation markers,
# metabolic markers and CTSS at consecutive
# time points in the dysglycemia and dyslipidemia strata. Friedman test to
# account for repeated measures
#
# 4) Multi-paramater ordinal regression with backwards elimination
# for the consecutive timepoints

# tools -------

  library(plyr)
  library(tidyverse)
  library(trafo)

  library(exda)
  library(rstatix)
  library(lmqc)
  library(MASS)
  library(mgcv)
  library(splines)
  library(caret)
  library(caretExtra)

  library(stringi)
  library(soucer)
  library(furrr)
  library(doParallel)

  select <- dplyr::select

  insert_head()

  c('./tools/globals.R',
    './tools/tools.R') %>%
    source_all(message = TRUE, crash = TRUE)

# analysis scripts --------

  insert_msg('Sourcing the analysis scripts')

  c('./analysis scripts/modeling.R',
    './analysis scripts/correlation.R',
    './analysis scripts/time_course.R',
    './analysis scripts/multi_modeling.R') %>%
    source_all(message = TRUE, crash = TRUE)

# END ------

  insert_tail()



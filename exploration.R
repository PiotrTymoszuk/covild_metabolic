# Exploratory data analysis. This comprises of the following tasks
#
# 1) Characteristic of the study cohort focused on metabolic and inflammatory
# features. The participants are analyzed as the entire collective and split
# by COVID-19 severity.
#
# 2) Relationship between the candidate modeling responses (opacity and
# CT severity score [CTSS], CTSS severity class, 60 day visit),
# to assess their consistency
#
# 3) Distribution testing for the candidate numeric modeling responses
# (opacity and CTSS) and numeric explanatory variables
# in respect to the sex and CoV severity (modeling confounders and known
# lung pathology risk factors)
#
# 4) Relationship between the candidate modeling responses
# (CTSS, opacity and CTSS severity class) and numeric explanatory variables
# to explore the need for higher-order terms
#
# 5) Distribution of serum markers of thrombo-inflammation in the dyslipidemia
# and dysglycemia strata at the consecutive visits

# tools -------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(rlang)

  library(exda)
  library(rstatix)
  library(lmqc)
  library(mgcv)

  library(stringi)
  library(soucer)
  library(furrr)

  select <- dplyr::select

  c('./tools/globals.R',
    './tools/tools.R') %>%
    source_all(message = TRUE, crash = TRUE)

# exploration scripts ------

  insert_msg('Exploration scripts')

  c('./exploration scripts/cohort.R',
    './exploration scripts/rater.R',
    './exploration scripts/mod_distribution.R',
    './exploration scripts/expl_distribution.R',
    './exploration scripts/relation.R',
    './exploration scripts/time_course.R') %>%
    source_all(message = TRUE, crash = TRUE)

# END -----

  insert_tail()

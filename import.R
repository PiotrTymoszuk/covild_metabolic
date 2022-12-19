# Import of the COVILD study data and wrangling

# tools ------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(stringi)
  library(readxl)

  library(soucer)

  insert_head()

  c('./tools/globals.R') %>%
    source_all(message = TRUE,
               crash = TRUE)

# container -------

  covild <- list()

# reading the raw dataset ------

  insert_msg('Reading the dataset')

  ## provided as an Excel table

  covild$raw <- read_xlsx('./data/Covid_metabolic_V0bisV3_PG_2.xlsx')

# wrangling ------

  insert_msg('Wrangling')

  covild$data <- covild$raw %>%
    transmute(patient_id = ID,
              sex = car::recode(gender, "0 = 'female'; 1 = 'male'"),
              sex = factor(sex, c('female', 'male')),
              bmi = BMI,
              bmi_class = cut(BMI,
                              c(-Inf, 25,30, Inf),
                              c('normal', 'overweight', 'obesity')),
              age = age,
              age_class = cut(age,
                              c(-Inf, 60, Inf),
                              c('<=60', '>60')),
              ## CoV severity, 3 level scale
              severity = car::recode(severity_3g ,
                                     "0 = 'mild';
                                     1 = 'moderate';
                                     2 = 'severe'"),
              severity = factor(severity, c('mild', 'moderate', 'severe')),
              ## metabolic diseases
              abnormal_weight = car::recode(bmi_class,
                                            "'normal' = 'no';
                                            'overweight' = 'yes';
                                            'obesity' = 'yes'"),
              abnormal_weight = factor(abnormal_weight, c('no', 'yes')),
              obesity = car::recode(bmi_class,
                                    "'normal' = 'no';
                                    'overweight' = 'no';
                                    'obesity' = 'yes'"),
              obesity = factor(obesity, c('no', 'yes')),
              diabetes = car::recode(diabetes,
                                     "1 = 'yes'; 0 = 'no'"),
              diabetes = factor(diabetes, c('no', 'yes')),
              dysglycemia_V0 = ifelse(HbA1c_V0 >= 5.7, 'yes', 'no'),
              dysglycemia_V0 = factor(dysglycemia_V0, c('no', 'yes')),
              dysglycemia_V1 = ifelse(HbA1c_V1 >= 5.7, 'yes', 'no'),
              dysglycemia_V1 = factor(dysglycemia_V1, c('no', 'yes')),
              dysglycemia_V2 = ifelse(HbA1c_V2 >= 5.7, 'yes', 'no'),
              dysglycemia_V2 = factor(dysglycemia_V2, c('no', 'yes')),
              dysglycemia_V3 = ifelse(HbA1c_V0 >= 5.7, 'yes', 'no'),
              dysglycemia_V3 = factor(dysglycemia_V3, c('no', 'yes')),
              ## CT severity scores (CTSS) for acute CoV and study visits
              ctss_acute = CT_sevScore_acute,
              ctss_V0 = CT_sevScore_V0,
              ctss_V1 = CT_sevScore_V1,
              ctss_V2 = CT_sevScore_V2,
              ctss_V3 = CT_sevScore_V3,
              ## classes of CTSS: 0, 1 - 5, 6 - 10 and 11+
              ## used for ordinal regression
              ctss_class_acute = cut(ctss_acute,
                                     c(-Inf, 0, 5, 10, Inf),
                                     c('none', 'mild', 'moderate', 'severe')),
              ctss_class_V0 = cut(ctss_V0,
                                  c(-Inf, 0, 5, 10, Inf),
                                  c('none', 'mild', 'moderate', 'severe')),
              ctss_class_V1 = cut(ctss_V1,
                                  c(-Inf, 0, 5, 10, Inf),
                                  c('none', 'mild', 'moderate', 'severe')),
              ctss_class_V2 = cut(ctss_V2,
                                  c(-Inf, 0, 5, 10, Inf),
                                  c('none', 'mild', 'moderate', 'severe')),
              ctss_class_V3 = cut(ctss_V3,
                                  c(-Inf, 0, 5, 10, Inf),
                                  c('none', 'mild', 'moderate', 'severe')),
              ## % opacity, automatically identified @CT images
              opac_acute = CT_percopac_acute,
              opac_V0 = CT_percopac_V0,
              opac_V1 = CT_percopac_V1,
              opac_V2 = CT_percopac_V2,
              ## manual filling of some missing % opacity values
              ## if no alterations could be identified by visual pathology
              ## assessment (CTSS = 0), the % opacity is set to 0 as well
              opac_V0 = ifelse(is.na(opac_V0),
                               ifelse(ctss_V0 == 0, 0, opac_V0),
                               opac_V0),
              opac_V1 = ifelse(is.na(opac_V1),
                               ifelse(ctss_V1 == 0, 0, opac_V1),
                               opac_V1),
              opac_V2 = ifelse(is.na(opac_V2),
                               ifelse(ctss_V2 == 0, 0, opac_V2),
                               opac_V2),
              ## inflammatory parameters, gender-specific cutoffs
              ## for ferritin
              CRP_V0 = CRP_V0,
              CRP_class_V0 = car::recode(CRP_V0_elv, "1 = 'yes'; 0 = 'no'"),
              CRP_class_V0 = factor(CRP_class_V0, c('no', 'yes')),
              CRP_V1 = CRP_V1,
              CRP_V2 = CRP_V2,
              CRP_V3 = CRP_V3,
              IL6_V0 = IL6_V0,
              IL6_class_V0 = car::recode(IL6_v0_elv, "1 = 'yes'; 0 = 'no'"),
              IL6_class_V0 = factor(IL6_class_V0, c('no', 'yes')),
              IL6_V1 = IL6_V1,
              IL6_V2 = IL6_V2,
              IL6_V3 = IL6_V3,
              FT_V0 = ferritin_V0,
              FT_class_V0 = ifelse(sex == 'male',
                                   cut(FT_V0,
                                       c(-Inf, 336, Inf)),
                                   cut(FT_V0,
                                       c(-Inf, 307, Inf))),
              FT_class_V0 = car::recode(FT_class_V0,
                                        "1 = 'no'; '2' = 'yes'"),
              FT_class_V0 = factor(FT_class_V0, c('no', 'yes')),
              FT_V1 = ferritin_V1,
              FT_V2 = ferritin_V2,
              FT_V3 = ferritin_V3,
              DDimer_V0 = DDimer_V0,
              DDimer_class_V0 = car::recode(Ddimer_v0_elv,
                                            "1 = 'yes'; 0 = 'no'"),
              DDimer_class_V0 = factor(DDimer_class_V0, c('no', 'yes')),
              DDimer_V1 = ddimer_V1,
              DDimer_V2 = ddimer_V2,
              DDimer_V3 = ddimer_V3,
              ## triglicerides, cutoff 200
              TG_V0 = triglceride_V0,
              TG_class_V0 = cut(TG_V0,
                                c(-Inf, 150, Inf),
                                c('no', 'yes')),
              TG_V1 = triglyceride_V1,
              TG_class_V1 = cut(TG_V1,
                                c(-Inf, 150, Inf),
                                c('no', 'yes')),
              TG_V2 = triglyceride_V2,
              TG_class_V2 = cut(TG_V2,
                                c(-Inf, 150, Inf),
                                c('no', 'yes')),
              TG_V3 = triglyceride_V3,
              TG_class_V3 = cut(TG_V3,
                                c(-Inf, 150, Inf),
                                c('no', 'yes')),
              ## HDL-C, gender specific cutoffs
              HDL_V0 = HDL_V0,
              HDL_class_V0 = ifelse(sex == 'male',
                                    cut(HDL_V0,
                                        c(-Inf, 40, Inf)),
                                    cut(HDL_V0,
                                        c(-Inf, 50, Inf))),
              HDL_class_V0 = car::recode(HDL_class_V0,
                                         "1 = 'yes'; 2 = 'no'"),
              HDL_class_V0 = factor(HDL_class_V0, c('no', 'yes')),
              HDL_V1 = HDL_V1,
              HDL_class_V1 = ifelse(sex == 'male',
                                    cut(HDL_V1,
                                        c(-Inf, 40, Inf)),
                                    cut(HDL_V1,
                                        c(-Inf, 50, Inf))),
              HDL_class_V1 = car::recode(HDL_class_V1,
                                         "1 = 'yes'; 2 = 'no'"),
              HDL_class_V1 = factor(HDL_class_V1, c('no', 'yes')),
              HDL_V2 = HDL_V2,
              HDL_class_V2 = ifelse(sex == 'male',
                                    cut(HDL_V2,
                                        c(-Inf, 40, Inf)),
                                    cut(HDL_V2,
                                        c(-Inf, 50, Inf))),
              HDL_class_V2 = car::recode(HDL_class_V2,
                                         "1 = 'yes'; 2 = 'no'"),
              HDL_class_V2 = factor(HDL_class_V2, c('no', 'yes')),
              HDL_V3 = HDL_V3,
              HDL_class_V3 = ifelse(sex == 'male',
                                    cut(HDL_V3,
                                        c(-Inf, 40, Inf)),
                                    cut(HDL_V3,
                                        c(-Inf, 50, Inf))),
              HDL_class_V3 = car::recode(HDL_class_V3,
                                         "1 = 'yes'; 2 = 'no'"),
              HDL_class_V3 = factor(HDL_class_V3, c('no', 'yes')),
              ## adiponectin and leptin
              ADIPOQ_V0 = Adiponectin_V0,
              ADIPOQ_V1 = Adiponectin_V1,
              ADIPOQ_V2 = Adiponectin_V2,
              ADIPOQ_V3 = Adiponectin_V3,
              LEP_V0 = Leptin_V0,
              LEP_V1 = Leptin_V1,
              LEP_V2 = Leptin_V2,
              LEP_V3 = Leptin_V3,
              ## definition of dyslipidemia
              dyslipidemia_V0 = ifelse(TG_class_V0 == 'yes' &
                                         HDL_class_V0 == 'yes',
                                       'yes', 'no'),
              dyslipidemia_V0 = factor(dyslipidemia_V0,
                                       c('no', 'yes')),
              dyslipidemia_V1 = ifelse(TG_class_V1 == 'yes' &
                                         HDL_class_V1 == 'yes',
                                       'yes', 'no'),
              dyslipidemia_V1 = factor(dyslipidemia_V1,
                                       c('no', 'yes')),
              dyslipidemia_V2 = ifelse(TG_class_V2 == 'yes' &
                                         HDL_class_V2 == 'yes',
                                       'yes', 'no'),
              dyslipidemia_V2 = factor(dyslipidemia_V2,
                                       c('no', 'yes')),
              dyslipidemia_V3 = ifelse(TG_class_V3 == 'yes' &
                                         HDL_class_V3 == 'yes',
                                       'yes', 'no'),
              dyslipidemia_V3 = factor(dyslipidemia_V3,
                                       c('no', 'yes')))

# Constraining the analysis table to participants with known severity -----

  insert_msg('Severity filtering')

  ## and sex and age

  covild$data <- covild$data %>%
    filter(!is.na(severity),
           !is.na(sex),
           !is.na(age))

# END -------

  insert_tail()

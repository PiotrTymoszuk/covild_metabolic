# Distribution of the modeling variables in respect to the sex and CoV severity
# (modeling confounders and known lung pathology risk factors)
#
# The normality and EOV testing shows clearly that either CTSS nor opacity
# (independently of the transformation) do not fulfill the normality
# and EOV assumptions
#
# Because of that, ordinal regression of CTSS severity classes seems to be
# the sole plausible modeling approach


  insert_head()

# container ------

  mod_distr <- list()

# globals -------

  insert_msg('Analysis globals')

  ## analysis table with the percent opacity, CTSS
  ## at the 60-day visit, sex and CoV severity
  ## implementing log and sqrt opacity variables

  mod_distr$analysis_tbl <- covild$data %>%
    select(patient_id,
           ctss_V0,
           opac_V0,
           sex,
           severity) %>%
    mutate(split_var = interaction(severity, sex),
           log_opac_V0 = log(opac_V0 + 1),
           sqrt_opac_V0 = sqrt(opac_V0)) %>%
    filter(!is.na(split_var))

# normality testing --------

  insert_msg('Normality testing')

  ## % opacity has, independently of the transformation
  ## quite severe deviations from normality
  ## in particular for mild COVID

  ## similar could be observed for CTSS

  mod_distr$normality <- mod_distr$analysis_tbl %>%
    explore(split_factor = 'split_var',
            variables = c('ctss_V0', 'opac_V0',
                          'log_opac_V0', 'sqrt_opac_V0'),
            what = 'normality',
            pub_styled = TRUE)

# EOV ------

  insert_msg('Variance homogeneity')

  ## there are quite large violations of the EOV assumption as well

  mod_distr$normality <- mod_distr$analysis_tbl %>%
    compare_variables(split_factor = 'split_var',
                      variables = c('ctss_V0', 'opac_V0',
                                    'log_opac_V0', 'sqrt_opac_V0'),
                      what = 'variance',
                      pub_styled = TRUE)

# END -------

  insert_tail()

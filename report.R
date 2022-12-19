# Analysis report tables and figures

# tools -----

  library(plyr)
  library(tidyverse)
  library(trafo)

  library(stringi)
  library(writexl)

  library(figur)
  library(cowplot)

  library(rmarkdown)
  library(flextable)
  library(knitr)
  library(bookdown)

  library(soucer)

  select <- dplyr::select

  insert_head()

  c('./tools/globals.R',
    './tools/tools.R') %>%
    source_all(message = TRUE, crash = TRUE)

# report scripts ------

  insert_msg('Report scripts')

  c('./report scripts/tables.R',
    './report scripts/figures.R',
    './report scripts/render.R') %>%
    source_all(message = TRUE, crash = TRUE)


# END ------

  insert_tail()

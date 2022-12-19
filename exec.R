# Source the complete pipeline

 library(soucer)

 print(source_all(c('import.R',
                    'exploration.R',
                    'analysis.R',
                    'report.R'),
                  message = TRUE, crash = TRUE))

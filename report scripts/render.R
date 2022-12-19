# Renders the report

insert_head()

# rendering the report -----

  insert_msg('Rendering the report')

  render('./report/markdown/report.Rmd',
         output_format = word_document2(number_sections = FALSE,
                                        reference_docx = 'ms_template.docx'),
         output_dir = './report')

# END -----

insert_tail()

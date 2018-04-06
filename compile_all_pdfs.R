rm(list = ls())

library(rmarkdown)

# compile all .rmd files in the required order

rmarkdown::render("wrangle_data.Rmd", 
                  output_format = "pdf_document", envir = new.env())

rmarkdown::render("simulate_dynamic.Rmd", 
                  output_format = "pdf_document", envir = new.env())

rmarkdown::render("fit_models.Rmd", 
                  output_format = "pdf_document", envir = new.env())

rmarkdown::render("analyse_results.Rmd", 
                  output_format = "pdf_document", envir = new.env())

rmarkdown::render("artificial_turf_predictive.Rmd", 
                  output_format = "pdf_document", envir = new.env())


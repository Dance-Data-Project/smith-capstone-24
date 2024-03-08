
##################################################################
# simple R script to knit all Rmarkdown file and generate outputs
# in the output_html directory within each file 
##################################################################

library(rmarkdown)
library(here)
library(tibble)
library(tidyverse)

# Infrastructure folder 
files <- tibble(full = dir(here("infrastructure_rmds"),
                           full.names = TRUE),
             partial = dir(here("infrastructure_rmds")))
             
output <- here("infrastructure_rmds", "output_html")

# order files to account for dependencies
# filter_out_filings depends on output of load_wrangle_filter
files_ordered <- tibble(partial = c(
  "load_wrangle_filter_data.Rmd",
           "loading_error_test.Rmd",
           "filter_out_filings.Rmd",
            "handle_discrepancies.Rmd",
           "howto_get_vars.Rmd",
           "data_dictionary.Rmd")) %>%
  left_join(files)

files <- files_ordered$full
  
for (i in files) {
  print(i)
  if(grepl(".Rmd", i)){render(i, output_dir = output, knit_root_dir=here())}
}


# exploration folder
files <- dir(here("explorations_rmds"),
             full.names = TRUE)
# will need to update 410uplabor once we have all prerequisite files 
files <- files[files != "410uplabor.Rmd"]
output <- here("explorations_rmds", "output_html")

for (i in files) {
  if(grepl(".Rmd", i)) { render(i, output_dir = output,  knit_root_dir=here())}
}


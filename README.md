# Dance Data Project®
# Smith College 2024 Capstone Project

## About

This project is done by Smith College's 2024 SDS Capstone group, sponsored by Dance Data Project®[“Dance Data Project”](“https://www.dancedataproject.com/”)], a company that works to promote and improve gender equity in dance. Most of the essential code in this repo is derived from the 2023 SDS Capstone group, whose own project and repo provided the groundwork and basis for the 2024 project.

## R Dependencies

All code in this repo should be run in the most recent version of R (4.3.3) Necessary packages can be installed using the `INSTALL_ALL.R` package, and each .Rmd file will automatically load any required packages when run from there. `RUN_ALL.R` will load any essential data into the environment, which is required for some files such as `m_calculations.Rmd`.

## Required Files

Some files will not be visible upon viewing and cloning the repo. Required files include an unzipped folder of .xml files that correspond to ballet company endowment information titled `990archivesfeb2024` which should be located in the base repo folder. Additionally, a file called `companies.csv` which contains company names and their corresponding EINs should be located in a folder called `data`. Another csv file called `SP500_historical.csv` should also be located in the `data` folder with a source linked in `quinn_images.Rmd` in the `final_report` folder.

## Readability

HTMLs of all files in `infrastructures_rmds` and some of `explorations_rmds` can be located in each folder's respective contained `output_html` folder. These show all code after being run, with tables and visualizations of the data being explored.
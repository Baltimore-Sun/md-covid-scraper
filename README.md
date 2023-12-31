# Scraping COVID-19 data in Maryland

This repository was created in August 2023 and uses R and Github actions to scrape data updated weekly on Tuesdays by the Maryland Department of Health about COVID-19. Data is used to populate the graphics on The Baltimore Sun's COVID-19 tracking page.

## The R Script

Packages required: tidyverse, RSocrata and zoo

The file [script.R](https://github.com/Baltimore-Sun/md-covid-scraper/blob/main/covid-repos/script.R) uses RSocrata to scrape data from the Maryland Department of Health's COVID-19 data on the [Open Data Portal](https://opendata.maryland.gov/). It then passes through several functions to clean and prepare for Datawrapper. 

### Github Actions

The [covid.yaml](https://github.com/Baltimore-Sun/md-covid-scraper/blob/main/.github/workflows/covid.yaml) file is scheduled to run every Tuesday at 2 p.m. and runs the script.R as well as commits the CSVs created from the R script. The URL's of all raw CSVs in Github are connected to the Datawrapper charts, which update hourly.

### Reproduction

If you use any of the CSVs created from this repository, please cite The Baltimore Sun as a contributing source.



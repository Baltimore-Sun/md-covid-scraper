# Scraping COVID-19 data in Maryland

This repository was created in August 2023 and uses R and Github actions to scrape data updated weekly by the Maryland Department of Health about COVID-19. Data is used to populate the graphics on The Baltimore Sun's COVID-19 tracker.

## The R Script

Packages required:

The file script.R uses RSocrata to scrape data from the MDH's COVID-19 data on the Open Data Portal. It then passes through several functions to clean and prepare for Datawrapper. 

### Github Actions

The covid.yaml file is scheduled to run every Tuesday at xxx and runs script.R as well as committs the CSVs created from the R script. The URL's of all raw CSVs in Github are connected to Github, which regularly updates every hour. 

### Reproduction

If you use any of the CSVs created from this repository, please cite The Baltimore Sun as a contributing source.

### Issues, questions, comments

Do you have feedback or questions about this repository or the COVID-19 tracking page? Fill out this form.

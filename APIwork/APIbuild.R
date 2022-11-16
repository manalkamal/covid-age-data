## Download data from COVerAGE-DB website via API 

## COVerAGE-DB Website: https://www.coverage-db.org/

require("dplyr")
require("stringr")
require("lubridate")
require("httr")
require("here")

# Examples:  
# test_2 <- "https://www.coverage-db.org/home/api/?dho=0&dh5=1&dh10=0&md=1&mc=1&mt=0&mv=0&c=New%20Zealand&csn=0&st=1&sf=1&sm=1&d1=2020-01-01&d2=2022-11-09&tsv=0"
# 
# GET(test_2, write_disk("download.zip"))

default <- "2020-01-01"

countries_df <- read.csv(here::here("Data", "countries.csv"))

## Function to build the API ## =====

build_api <- function(original, harmonized_5, harmonized_10, 
                      deaths, cases, tests, vac, 
                      country, country_subnational, 
                      sex_total, sex_female, sex_male, 
                      date_start = default, 
                      date_end, tsv){
  
  base <- "https://www.coverage-db.org/home/api/?"
  
  ## Type of data: 
  ## 1- Original (True = 1, False = 0): No Age harmonization, 
  ## 2- Harmonized 5-year-data (True = 1, False = 0): 5-Year Age harmonization
  ## 3- Harmonized 10-year-data (True = 1, False = 0): 10-Year Age harmonization
  
  original %in% c(0, 1) 
  harmonized_5 %in% c(0, 1) 
  harmonized_10 %in% c(0, 1)
  
  ## Measures of interest
  
  ## Deaths, Cases, Tests, Vaccinations: True = 1, False = 0 
  
  deaths %in% c(0, 1)
  cases %in% c(0, 1)
  tests %in% c(0, 1)
  vac %in% c(0, 1)
  
  ## Country/ies of interest, 
  ## This is extracted from input_DB, so probably will need an update every now & then
  
  
  country %in% countries_df$Country
  
  ## if the country name has space, replace with %20 - API adjustement.. 
  
  country <- dplyr::if_else(str_detect(country, " "), str_replace(country, " ", "%20"), country)
  
  if (length(country) > 1){
    country = paste(country, collapse = "&c=")
  }
  
  
  ## Sub-national data: True = 1, False = 0
  
  country_subnational %in% c(0, 1)
  
  ## Sex: both sexes, females, males: True = 1, False = 0
  
  sex_total %in% c(0, 1)
  sex_female %in% c(0, 1)
  sex_male %in% c(0, 1)
  
  ## Dates: default is "2020-01-01"
  
  date_start %in% format(seq(from = as.Date("2020/01/01"), to = today(), by = "1 days"), "%Y-%m-%d")
  date_end %in% format(seq(from = as.Date("2020/01/01"), to = today(), by = "1 days"), "%Y-%m-%d")
  
  ## either tsv file or csv: for tsv: 1, csv = 0
  
  tsv %in% c(0, 1)
  
  ## paste the base, with all the arguments 
  
  paste0(base, 
         "dho=", original, "&dh5=", harmonized_5, "&dh10=", harmonized_10, 
         "&md=", deaths, "&mc=", cases, "&mt=", tests, "&mv=", vac, 
         "&c=", country, 
         "&csn=", country_subnational,
         "&st=", sex_total, "&sf=", sex_female, "&sm=", sex_male, 
         "&d1=", date_start, "&d2=", date_end, "&tsv=", tsv)
  
}

## et voila, an example: 

build_api(original = 0, 
          harmonized_5 = 1, 
          harmonized_10 = 0, 
          deaths = 1, 
          cases = 1, 
          tests = 0, 
          vac = 0,
          country = c("Afghanistan", "New Zealand"), 
          country_subnational = 0, 
          sex_total = 1,
          sex_female = 1, 
          sex_male = 1, 
          date_start = default, 
          date_end = today(), 
          tsv = 0)

#END# 

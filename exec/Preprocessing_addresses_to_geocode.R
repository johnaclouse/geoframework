library(tidyverse)
library(RODBC)

# install.packages(
#   "l:/data store/data science team/r/packages/geoframework_0.1.0.tar.gz",
#   repos = NULL,
#   type="source")
#
# - OR -
#
# library(devtools)
# devtools::install_github(repo = 'johnaclouse/geoframework')

library(geoframework)




message("Docker must have access to c:/temp")
message("This script will create temporary files in c:/temp")

member_path <- "c:/temp/geocode_members/"
provider_path <- "c:/temp/geocode_providers/"

con <- odbcConnect("ADW")

remove_employees<- function(addresses){
  addresses %>%
    filter(!str_detect(tolower(street_address), pattern = "emp addr"))
}

clean_addresses <- function(addresses){
  addresses %>%
    mutate_if(is.character, iconv, "latin1", "ASCII", sub = "") %>%
    mutate(zip = gsub("*", "", zip, fixed = T))
}

add_fields <- function(addresses) {
  addresses %>%
    mutate(zip5 = substr("zip", 1, 5),
           address = paste0(str_trim(street_address), " ",
                            str_trim(city), ", ",
                            str_trim(state), " ",
                            zip5))
}



# Provider addresses ----

sql_command_members <- "SELECT distinct
                  Address_1 as street_address,
                  City as city,
                  State as state,
                  Zip as zip
                FROM Membs_Demo"

address_components <- sqlQuery(con,
                               sql_command_members,
                               stringsAsFactors = FALSE)

addresses <- remove_employees(address_components)
addresses <- clean_addresses(addresses)
addresses <- add_fields(addresses)
export_sequence_files(addresses, file_path = member_path)



# Provider addresses ----

sql_command_providers <- "SELECT DISTINCT
                  Address_Line_1 as street_address,
                  City as city,
                  State as state,
                  Zip as zip
                FROM Provider_Address
                WHERE Provider_Address_Type <> 'BIL'
                  AND Address_Line_1 <> ''
                  AND State IN ('OR', 'WA', 'AK')"

address_components <- sqlQuery(con,
                               sql_command_providers,
                               stringsAsFactors = FALSE)

addresses <- clean_addresses(address_components)
addresses <- add_fields(addresses)


geoframework::export_geocoding_sequence_files(addresses, file_path = provider_path)


odbcCloseAll()


# single command line syntax
# docker run --rm=TRUE -v c:/temp/geocode:/tmp degauss/geocoder 023.csv full_address
# -v is a virtual drive. The :/tmp specifies the internal mapping and cannot change.
# The part before the colon can change to match the chosen working directory on Windows
# The full_address agrument is actually just the name of the column containing the address
# to be geocoded

# with census and deprivation index
# docker run --rm=TRUE -v c:/temp/geocode:/tmp degauss/cchmc_batch_geocoder 1a.csv

# DOS batch file
# for /f %a IN ('dir /b /s "c:\temp\geocode\*.csv"') do echo docker run --rm=TRUE -v c:/temp/geocode:/tmp degauss/geocoder %a full_address
# for /f %a IN ('dir /b /s "c:\temp\geocode\*.csv"') do call docker run --rm=TRUE -v c:/temp/geocode:/tmp degauss/geocoder %a full_address

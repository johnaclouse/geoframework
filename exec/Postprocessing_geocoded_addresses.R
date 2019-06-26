library(tidyverse)

member_path <- "c:/temp/geocode_members/"
provider_path <- "c:/temp/geocode_providers/"
output_path <- "c:/temp/geocoded_addresses.csv"

column_types <- cols(
  .default = col_double(),
  street_address = col_character(),
  city = col_character(),
  state = col_character(),
  zip = col_character(),
  zip3 = col_character(),
  address = col_character(),
  bad_address = col_logical(),
  PO = col_logical(),
  precision = col_character(),
  precise_geocode = col_logical())

# Provider addresses ----

geocoded_member_files <- dir(member_path,
                      full.names = T,
                      pattern = "geocoded.csv")

geocoded_member_addresses <- geocoded_member_files %>% 
  map(read_csv, col_types = column_types) %>% 
  reduce(rbind)

# Provider addresses ----

geocoded_provider_files <- dir(provider_path,
                      full.names = T,
                      pattern = "geocoded.csv")

geocoded_provider_addresses <- geocoded_provider_files %>% 
  map(read_csv, col_types = column_types) %>% 
  reduce(rbind)

geocoded_addresses <- rbind(geocoded_member_addresses,
                            
                            geocoded_provider_addresses) %>% 
  filter(!is.na(city),
         !is.na(state),
         !is.na(zip)) %>% 
  mutate_if(is.character, iconv, "latin1", "ASCII", sub = "") %>%
  mutate(is_precise_geocode = ifelse(is.na(precise_geocode), F, precise_geocode),
         zip5 = substr(zip, 1, 5)) %>% 
  select(address,
         street_address,
         city,
         state,
         zip5,
         zip3,
         is_bad_address = bad_address,
         is_PO_box = PO,
         is_precise_geocode,
         precision,
         score,
         fips_tract_id,
         lon,
         lat) %>% 
  distinct()

cat("Column character widths:")
View(as_tibble(map(geocoded_addresses, function(x) max(nchar(as.character(x)), na.rm = T))))
            

write_csv(geocoded_addresses, 
          na = "",
          path = output_path)



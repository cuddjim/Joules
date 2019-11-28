

subject_matter_1 <- list.files(pattern = "*LoadingData.csv") %>% 
  lapply(read_csv) %>% 
  bind_rows %>%
  set_colnames(gsub(' ','_',names(.))) %>%
  mutate(indicator=case_when(!is.na(Fuel_consumed_for_electric_power_generation) ~ 'input',
                             !is.na(Cost_of_fuel_for_electric_power_generation) ~ 'price',
                             !is.na(Electricity_generated_from_fuels) ~ 'output'),
         commodity=case_when(!is.na(Fuel_consumed_for_electric_power_generation) ~ Fuel_consumed_for_electric_power_generation,
                             !is.na(Cost_of_fuel_for_electric_power_generation) ~ Cost_of_fuel_for_electric_power_generation,
                             !is.na(Electricity_generated_from_fuels) ~ Electricity_generated_from_fuels)) %>%
  select(REF_DATE,GEO,VALUE,indicator,commodity) %>%
  set_colnames(c('year','province','value','indicator','commodity')) %>%
  mutate(commodity=tolower(gsub(' ','_',commodity)),
         variable=str_c(commodity,'_',indicator,'_',year)) %>%
  select(province,variable,value) %>%
  spread(variable,value)

subject_matter_2 <- list.files(pattern = "*LoadingData.csv") %>%
  lapply(read_csv) %>%
  bind_rows %>%
  set_colnames(gsub(' ','_',names(.))) %>%
  mutate(indicator=case_when(!is.na(Fuel_consumed_for_electric_power_generation) ~ 'input',
                             !is.na(Cost_of_fuel_for_electric_power_generation) ~ 'price',
                             !is.na(Electricity_generated_from_fuels) ~ 'output'),
         commodity=case_when(!is.na(Fuel_consumed_for_electric_power_generation) ~ Fuel_consumed_for_electric_power_generation,
                             !is.na(Cost_of_fuel_for_electric_power_generation) ~ Cost_of_fuel_for_electric_power_generation,
                             !is.na(Electricity_generated_from_fuels) ~ Electricity_generated_from_fuels)) %>%
  select(REF_DATE,GEO,VALUE,indicator,commodity) %>%
  set_colnames(c('year','province','value','indicator','commodity')) %>%
  mutate(commodity=tolower(gsub(' ','_',commodity))) %>%
  spread(indicator,value)

# read map
prov_map = readOGR('Canada/Canada.shp',stringsAsFactors = FALSE)
prov_map <- spTransform(prov_map, CRS("+proj=longlat +datum=WGS84"))

prov_centroids = gCentroid(prov_map,byid=TRUE) %>% 
  data.frame() %>% 
  mutate(NAME=prov_map$NAME)

prov_map %<>% 
  left_join(subject_matter_1, by=c('NAME'='province')) %>%
  left_join(prov_centroids, by=c('NAME'))



convert_inputs_to_emissions = data.frame(
  
  commodity = c("wood","light_fuel_oil","heavy_fuel_oil","diesel","total_coal",
                  "natural_gas","uranium","methane","propane"),
  conversion_factor = c(423,2626,3145,2679,2430,1927,40,1900,1540)
  
)

subject_matter_3 <- list.files(pattern = "*LoadingData.csv") %>% 
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
  filter(!(commodity %in% c('Total petroleum products'))) %>%
  mutate(commodity=ifelse(commodity=='Total heavy fuel oil','heavy fuel oil',commodity),
         commodity=tolower(gsub(' ','_',commodity))) %>%
  left_join(convert_inputs_to_emissions, by='commodity') %>%
  mutate(emission = value*conversion_factor/1000) %>%
  filter(indicator=='input') %>%
  select(-indicator,-conversion_factor,-value) %>%
  mutate(indicator='emission',
         variable=str_c(commodity,'_',indicator,'_',year)) %>%
  rename(value=emission)

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
  filter(!(commodity %in% c('Total petroleum products'))) %>%
  mutate(commodity=ifelse(commodity=='Total heavy fuel oil','heavy fuel oil',commodity),
         commodity=tolower(gsub(' ','_',commodity)),
         variable=str_c(commodity,'_',indicator,'_',year)) %>%
  rbind(.,subject_matter_3) %>%
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
  filter(commodity != 'Total petroleum products') %>%
  mutate(commodity=ifelse(commodity=='Total heavy fuel oil','heavy fuel oil',commodity),
         commodity=tolower(gsub(' ','_',commodity))) %>%
  rbind(.,subject_matter_3 %>% select(-variable)) %>%
  spread(indicator,value)




# read map
prov_map = readOGR('Canada/Canada.shp',stringsAsFactors = FALSE)
prov_map <- spTransform(prov_map, CRS("+proj=longlat +datum=WGS84"))

prov_centroids = gCentroid(prov_map,byid=TRUE) %>% 
  data.frame() %>% 
  mutate(NAME=prov_map$NAME)

prov_centroids[which(prov_centroids$NAME=='Nunavut'),c('x','y')] = c(-97,64)

prov_map %<>% 
  left_join(subject_matter_1, by=c('NAME'='province')) %>%
  left_join(prov_centroids, by=c('NAME'))



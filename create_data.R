
convert_inputs_to_emissions = data.frame(
  
  commodity = c("wood","light_fuel_oil","heavy_fuel_oil","diesel","total_coal",
                  "natural_gas","uranium","methane","propane"),
  conversion_factor = c(423,2626,3145,2679,2430,1927,40,1900,1540)
  
)

convert_inputs_to_tj = data.frame(
  
  # commodity = c("Wood","Light fuel oil","Total heavy fuel oil","Diesel","Total coal",
  #               "Natural gas","Uranium","Methane","Propane"),
  commodity = c("wood","light_fuel_oil","heavy_fuel_oil","diesel","total_coal",
                "natural_gas","uranium","methane","propane"),
  conversion_factor_tj = c(0.018,0.0388,0.0425,0.0383,0.0264,0.0383,0.28,0.0399,0.02531)
  
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
  left_join(convert_inputs_to_tj, by='commodity') %>% 
  mutate(value = ifelse(indicator=='input',value*conversion_factor_tj,value)) %>% 
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
         commodity=tolower(gsub(' ','_',commodity))) %>% 
  left_join(convert_inputs_to_tj, by='commodity') %>% 
  mutate(value = ifelse(indicator=='input',value*conversion_factor_tj,value)) %>% 
  mutate(variable=str_c(commodity,'_',indicator,'_',year)) %>%
  rbind(.,subject_matter_3) %>%
  select(province,variable,value) %>%
  spread(variable,value)


story_frame = data.frame(province=c(rep('Ontario',4)),
                        year=c(2005,2012,2013,2014),
                        commodity=rep('total_coal',4),
                        story=c('Lakeview generating station closure (Capacity of 2,400 MWh)',
                                'Atikokan generating station closure (Capacity of 211 MWh)',
                                'Lambton and Nanticoke generating station closures (Total Capacity of 6,920 MWh)',
                                'Thunder Bay generating station closure (Capacity of 306 MWh)')) %>%
  mutate(story=as.character(story))

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
  left_join(convert_inputs_to_tj, by='commodity') %>% 
  mutate(value = ifelse(indicator=='input',value*conversion_factor_tj,value)) %>% 
  rbind(.,subject_matter_3 %>% select(-variable)) %>%
  spread(indicator,value) %>%
  left_join(story_frame,by=c('province','commodity','year')) %>%
  mutate(price=price/input)



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

story_frame <- data.frame(Story=c("Ontario eliminates coal",
                   "PEI wind electricity",
                   "Energy in the North"),
           Description=c("In 2001, Ontario had 5 coal fired generating stations with a capacity of 
                                          roughly 8,800 MWh. By 2014, all coal generating stations ceased operations
                                          to be replaced with a mixture of nuclear, natural gas fired, and non-hydro 
                                          renewable plants. The Atikokan and Thunder Bay generating stations are now 
                                          exclusively biomass based facilities",
                         "PEI has no sources of oil, natural gas, or other fuels used traditionally 
                                          for electricity generation.  Instead 99% of their electricity production 
                                          comes from wind mills.  However, wind production only is able to meet roughly 
                                          25% of PEI's demand for electricity.  The remainder is imported from 
                                          New Brunswick.  There is an ideal wind speed for wind generated electricity. 
                                          The wind needs to be fast enough to move the wind turbine (12-14 km/h), 
                                          but not too strong that the turbines need to be shut down in order to protect 
                                          them (roughly 90 km/h).  The ideal wind speed to for the turbines to be at 
                                          full capacity is between 50 to 60 km/h.",
                         "Unlike the rest of Canada where the major fuel used (except in transportation) 
                                          is natural gas, the North runs on diesel.  Energy options in the North are limited 
                                          because there is no infrastructure in place that allows electricity to be imported 
                                          from Southern Canada.  All electricity consumed must be generated locally.
                                          In Nunavut, 100% of electricity generation comes from diesel where in Yukon 
                                          the main type of electricity generation is hydro with diesel making up the difference. 
                                          In some communities in the North unsubsidized electricity costs are 10 times that of the 
                                          Canadian average on a per KWh basis whereas consumption is twice that national average."))



convert_inputs_to_emissions = data.frame(
  
  commodity = c("wood","heavy_fuel_oil","diesel","total_coal",
                  "natural_gas","uranium","propane"),
  conversion_factor = c(100.11,74.58,74.08,90.87,49.88,0.01,60.61)
  
)

convert_inputs_to_tj = data.frame(
  
  # commodity = c("Wood","Light fuel oil","Total heavy fuel oil","Diesel","Total coal",
  #               "Natural gas","Uranium","Methane","Propane"),
  commodity = c("wood","heavy_fuel_oil","diesel","total_coal",
                "natural_gas","uranium","propane"),
  conversion_factor_tj = c(0.018,0.0425,0.0383,0.0264,0.0383,700,0.02531)
  
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
  mutate(commodity = ifelse(commodity == 'Methane','Natural gas',ifelse(commodity == 'Light fuel oil','Diesel',commodity))) %>% 
  group_by(REF_DATE, GEO, indicator, commodity) %>% 
  summarise(VALUE = sum(VALUE)) %>% 
  data.frame() %>% 
  set_colnames(c('year','province','indicator','commodity','value')) %>%
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
  mutate(commodity = ifelse(commodity == 'Methane','Natural gas',ifelse(commodity == 'Light fuel oil','Diesel',commodity))) %>% 
  group_by(REF_DATE, GEO, indicator, commodity) %>% 
  summarise(VALUE = sum(VALUE)) %>% 
  data.frame() %>% 
  set_colnames(c('year','province','indicator','commodity','value')) %>%
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
  mutate(commodity = ifelse(commodity == 'Methane','Natural gas',ifelse(commodity == 'Light fuel oil','Diesel',commodity))) %>% 
  group_by(REF_DATE, GEO, indicator, commodity) %>% 
  summarise(VALUE = sum(VALUE)) %>% 
  data.frame() %>% 
  set_colnames(c('year','province','indicator','commodity','value')) %>%
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


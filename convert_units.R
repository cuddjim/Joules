

test <- list.files(pattern = "*LoadingData.csv") %>% 
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
         variable=str_c(commodity,'_',province,'_',year)) %>%
  select(variable,indicator,value,commodity,province,year) %>%
  spread(indicator,value) %>%
  left_join(convert_units,by='commodity') %>%
  mutate(efficiency=(input*xxx)/(output*xxx))

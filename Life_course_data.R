

# Loading some packages 
packages <- c('easypackages','readxl', 'tidyr', 'dplyr', 'readr', "rgdal", 'nomisr')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

local_store <- '~/Repositories/ICT_lifecourse'

ICT_areas <- c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove West', 'Brighton and Hove Central', 'Brighton and Hove', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden')

all_indicators_df <- read_csv(paste0(local_store, "/HEX_indicator_master_dataset.csv"))

Life_course_indicators <- read_csv(paste0(local_store, "/Life_course_indicators.csv"))

LC_data <- all_indicators_df %>% 
  filter(Indicator_short_name %in% Life_course_indicators$Indicator_short_name) %>% 
  left_join(Life_course_indicators, by = 'Indicator_short_name') %>% 
  arrange(Area_name, Position)

i = 17

LC_data %>% 
  filter(Area_name == ICT_areas[i]) %>% 
  select(Area_name, Section, Indicator_short_name, Indicator_label, Time_period, Value_label, At_ICT_level, Significance_England, Polarity) %>% 
  View()

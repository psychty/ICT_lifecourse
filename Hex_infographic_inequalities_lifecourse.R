# Hexagonal plots

# Load some packages ####
# These are the packages used in this script
packages <- c('easypackages', 'tidyr', 'ggplot2', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'rgdal', 'spdplyr', 'geojsonio', 'jsonlite', 'viridis', 'nomisr', 'ggstar', 'fingertipsR', 'readODS', 'lemon', 'packcircles')

# This command installs only those packages (from our object packages) which are not already installed.
install.packages(setdiff(packages, rownames(installed.packages())))

# This loads the packages
easypackages::libraries(packages)

# Task - looks like we're making seven Hex plots (2 x building blocks, 1 x starting, 2 x living, 1 x ageing, 1 x dying well), with about 9 indicators in each, for each ICT (there are 16 but we'll only be able to make 13 without manually cutting the B&H data)

# Thats 91 plots (or 112 if we get B&H ICTs rather than the whole UA).

# We're already making 16 lifecourse infographics.

# Thankfully we can automate the hex plots in R, it just requires a bit of looping and filtering and getting the data in the right place.

# Steps ####
# Get data for all indicators for all areas (about 800 rows)
# We will loop through each of the ICT areas, and plots, getting the data we need for each one, and exporting an image. 

# ICT areas 
# This version has the four ICTs in B&H but we cannot get fingertips data for these
ICT_areas <- c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove West', 'Brighton and Hove Central', 'Brighton and Hove', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden')

# A version including Brighton and Hove UA
# ICT_areas <- c('Brighton and Hove', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden')

# Some of these indicators are UTLA only, so which UTLA should be shown for each area?

ICT_areas_lookup <- data.frame(Area = ICT_areas) %>% 
  mutate(UTLA = ifelse(Area %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'), 'West Sussex', ifelse(Area %in% c('Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden'), 'East Sussex', ifelse(str_detect(Area, 'Brighton and Hove'), 'Brighton and Hove', Area)))) 

# At some points we need to look up to registered populations 
ICT_areas_lookup_2 <- ICT_areas_lookup %>% 
  mutate(Sub_ICB = ifelse(Area %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'), 'Sussex ICB - 70F ', ifelse(Area %in% c('Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden'), 'Sussex ICB - 97R ', ifelse(str_detect(Area, 'Brighton'), 'Sussex ICB - 09D ', NA))))

# At some points we need to look up to registered populations 
ICT_areas_lookup_3 <- ICT_areas_lookup %>% 
  mutate(CCG = ifelse(Area %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'), 'NHS West Sussex CCG', ifelse(Area %in% c('Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden'), 'NHS East Sussex CCG', ifelse(str_detect(Area, 'Brighton'), 'NHS Brighton And Hove CCG', NA))))

# Specify some colours - if we do this here, we can tweak the colours just once in the object which is referenced throughout the script (rather than finding and replacing every instance of #fadfdf.
hex_colours_worse <- '#fadfdf'
hex_colours_better <- '#cde8c5'
hex_colours_similar <- '#fff2ae'
hex_colours_na <- '#e5f4fb'

# We need a set of styling for our hex figures we can store in a theme rather than using the similar code over and over
theme_hex = function(){
  theme(
    plot.title = element_text(colour = "#000000", face = "bold", size = 20), # Here in bold
    plot.subtitle = element_text(colour = "#000000", size = 16),
    plot.caption = element_text(colour = "#000000", size = 13),
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(colour = "#000000", size = 13, face = 'bold'),
    strip.background = element_blank(),
    legend.title = element_text(colour = "#000000", size = 10, face = "bold"),
    legend.background = element_rect(fill = "#ffffff"),
    legend.key = element_rect(fill = "#ffffff", colour = "#ffffff"),
    legend.text = element_text(colour = "#000000", size = 10),
    legend.key.size = unit(.01, 'cm'), 
    legend.key.height = unit(.01, 'cm'),
    legend.position = 'bottom',
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    )
}

# We need a set of styling for our circle figures we can store in a theme rather than using the similar code over and over
theme_circles = function(){
  theme(
    plot.title = element_text(colour = "#000000", face = "bold", size = 20), # Here in bold
    plot.subtitle = element_text(colour = "#000000", size = 16),
    plot.caption = element_text(colour = "#000000", size = 13),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_blank(),
    legend.title = element_text(colour = "#000000", size = 13, face = "bold"),
    legend.background = element_rect(fill = "#ffffff"),
    legend.key = element_rect(fill = "#ffffff", colour = "#ffffff"),
    legend.text = element_text(colour = "#000000", size = 13),
    legend.position = 'bottom',
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
}

# I use a theme or template of styles for ggplot 
ph_theme = function(){
  theme(
    plot.title = element_text(colour = "#000000", face = "bold", size = 12), # Here in bold
    plot.subtitle = element_text(colour = "#000000", size = 11),
    plot.title.position = "plot",
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E7E7E7", size = .3),
    panel.grid.minor = element_blank(),
    strip.text = element_text(colour = "#000000", size = 12),
    strip.background = element_blank(),
    legend.title = element_text(colour = "#000000", size = 10, face = "bold"),
    legend.background = element_rect(fill = "#ffffff"),
    legend.key = element_rect(fill = "#ffffff", colour = "#ffffff"),
    legend.text = element_text(colour = "#000000", size = 10),
    legend.position = 'top',
    axis.ticks = element_line(colour = "#dbdbdb"),
    axis.text.y = element_text(colour = "#000000", size = 11),
    axis.text.x = element_text(colour = "#000000", angle = 90, hjust = 1, vjust = .5, size = 11),
    axis.title =  element_text(colour = "#000000", size = 11, face = "bold"),
    axis.line = element_line(colour = "#dbdbdb"))}

# File paths ####

# If you can sync the sharepoint folder to a local onedrive folder, you can access it that way.
# I couldn't directly link to the sharepoint directory for Sussex wide PH, so to work around, i have synced the directory to my onedrive (which i can access via R) and work there.
synced_sp_directory <- 'C:/Users/rtbp8900/West Sussex County Council/Sussex-wide PH Intelligence - Integrated Care Team ICT profiles'

# Specify where our outputs should go
local_output_directory <- '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Outputs'

# Specify where any data comes from 
local_data_directory <-  '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Data'

# The hex figures are drawn as a scatter plot in ggplot with x and y coordinates
# We match the hex_id from the indicators list - this has the x and y positions of each icon/hex/blockbuster
# We need a grid that covers 7 across by 4 rows
hex_positions <- data.frame(x = c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,1,2,3,4,5,6,7,1.5,2.5,3.5,4.5,5.5,6.5,7.5,1,2,3,4,5,6,7),
                            y = c(rep(1.5,7), rep(2, 7), rep(2.5,7), rep(3,7)),
                            Hex_ID = seq(1,28))

hex_indicators <- read_csv(paste0(synced_sp_directory, '/hex_indicators.csv')) %>% 
  left_join(hex_positions, by = 'Hex_ID')

# Brighton and Hove ICT level data has been compiled by Georgia Phillips and Jackie Barrie as it is not readily available in the public domain.
BH_local_data <- read_csv(paste0(synced_sp_directory, '/BH_ICT_data_October_2023.csv'))

#setdiff(BH_local_data$Indicator_short_name, hex_indicators$Indicator_short_name)

# Example grid ####

# This is a list of indicators for the hex/blockbuster figures, the Hex_ID is the position from bottom left to right upwards
# e.g.  7,8,9
#       4,5,6
#       1,2,3

# on the biggest grid...

#  22,23,24,25,26,27,28
#  15,16,17,18,19,20,21
#  8,9,10,11,12,13,14
#  1,2,3,4,5,6,7
# 
# ggplot(hex_positions,
#        aes(x = x,
#            y = y)) +
#   geom_star(starshape = 6,
#             size = 70,
#             aes(fill = sample(c('Better','Similar','Worse','Not applicable'), 28, replace = TRUE)),
#             colour = '#ffffff') +
#   scale_x_continuous(limits = c(0.5,7.5)) +
#   scale_y_continuous(limits = c(1,3.5)) +
#   scale_fill_manual(values = c('Worse' = hex_colours_worse, 'Better' = hex_colours_better, 'Similar' = hex_colours_similar, 'Not applicable' = hex_colours_na)) +
#   geom_text(aes(x - .4,
#                 y + .1,
#                 label = Hex_ID),
#             hjust = 0,
#             vjust = 0,
#             fontface = 'bold',
#             size = 7) +
#   labs(title = 'Test hex plot',
#        subtitle = 'Test area') +
#   geom_text(aes(x - .4,
#                 y - .2,
#                 label = 'line 1\nline 2\nline 3\nline 4'),
#             hjust = 0,
#             vjust = 0) +
#   # coord_equal()+
#   theme_hex() +
#   theme(legend.position = 'none')


# Data extract ####

# Building blocks 1 ####
# hex_indicators %>% 
#   filter(Figure_ID == 'Building blocks 1')

Working_age_out_of_work_LTLA <- fingertips_data(IndicatorID = 93097,
                                                AreaTypeID = 'All',
                                                categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Working_age_out_of_work') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%'))

# Is working_age_out_of_work available for B&H ICTs
BH_local_data %>% 
  filter(Indicator_short_name == 'Working_age_out_of_work')
# No, so we need to use

Working_age_out_of_work <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Working_age_out_of_work_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Working_age_out_of_work_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Working_age_out_of_work_LTLA)

Fuel_poverty_LTLA <- fingertips_data(IndicatorID = 93759,
                                                AreaTypeID = 'All',
                                                categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  # filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Fuel_poverty') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%'))

# # Is working_age_out_of_work available for B&H ICTs
# BH_local_data %>%
#   filter(Indicator_short_name == 'Fuel_poverty') %>%
#   mutate(Time_period = '2021') %>% 
#   mutate(Value_label = paste0(round(Value_label * 100, 1), '%'))

Fuel_poverty <- BH_local_data %>% 
  filter(Indicator_short_name == 'Fuel_poverty') %>% 
  select(Area_name, Indicator_short_name, Significance_England, Value_label) %>% 
  mutate(Value_label = paste0(round(Value_label * 100, 1), '%')) %>% 
  mutate(Time_period = '2021') %>% 
  bind_rows(Fuel_poverty_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])
  
rm(Fuel_poverty_LTLA)

# Housing_affordability_ratio
if(file.exists(paste0(local_data_directory, '/Housing_affordability_ratio.xlsx')) != TRUE){
  download.file('https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/ratioofhousepricetoresidencebasedearningslowerquartileandmedian/current/ratioofhousepricetoresidencebasedearnings.xlsx',
                paste0(local_data_directory, '/Housing_affordability_ratio.xlsx'),
                mode = 'wb')
}

England_housing_affordability_ratio <- read_excel(paste0(local_data_directory, '/Housing_affordability_ratio.xlsx'),
                                                  sheet = "2c", 
                                                  col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric"), 
                                                  skip = 1) %>% 
  filter(Name %in% c(ICT_areas, 'England')) %>% 
  select(Area_name = 'Name', Value = '2022') 

Housing_affordability_ratio_LTLA <- read_excel(paste0(local_data_directory, '/Housing_affordability_ratio.xlsx'),
           sheet = "6c", 
           col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric"), 
           skip = 1) %>% 
  filter(`Local authority name` %in% c(ICT_areas, 'England')) %>% 
  select(Area_name = 'Local authority name', Value = '2022') %>% 
  bind_rows(England_housing_affordability_ratio) %>% 
  mutate(Significance_England = 'not_applicable',
         Indicator_short_name = 'Housing_affordability_ratio') %>% 
  mutate(Value_label = paste0(round(Value, 1)))

# # Is Housing_affordability_ratio available for B&H ICTs
# BH_local_data %>%
#   filter(Indicator_short_name == 'Housing_affordability_ratio')
# No

Housing_affordability_ratio <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Housing_affordability_ratio_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name')]) %>% 
  bind_rows(Housing_affordability_ratio_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name')]) %>% 
  mutate(Time_period = '2022')

rm(England_housing_affordability_ratio, Housing_affordability_ratio_LTLA)

# Deprivation 
England_deprivation_quintile_1 <- read_csv(url('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv')) %>% 
  select(LSOA11CD = `LSOA code (2011)`,  LTLA = `Local Authority District name (2019)`, IMD_Decile = "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)", Numerator = "Total population: mid 2015 (excluding prisoners)") %>% 
  mutate(IMD_Decile = factor(ifelse(IMD_Decile == 1, 'Most deprived 10%',  ifelse(IMD_Decile == 2, 'Decile 2',  ifelse(IMD_Decile == 3, 'Decile 3',  ifelse(IMD_Decile == 4, 'Decile 4',  ifelse(IMD_Decile == 5, 'Decile 5',  ifelse(IMD_Decile == 6, 'Decile 6',  ifelse(IMD_Decile == 7, 'Decile 7',  ifelse(IMD_Decile == 8, 'Decile 8',  ifelse(IMD_Decile == 9, 'Decile 9',  ifelse(IMD_Decile == 10, 'Least deprived 10%', NA)))))))))), levels = c('Most deprived 10%', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 'Least deprived 10%'))) %>% 
  mutate(IMD_Quintile = factor(ifelse(IMD_Decile %in% c('Most deprived 10%', 'Decile 2'), 'Most deprived 20%', ifelse(IMD_Decile %in% c('Decile 3', 'Decile 4'), 'Quintile 2', ifelse(IMD_Decile %in% c('Decile 5', 'Decile 6'), 'Quintile 3', ifelse(IMD_Decile %in% c('Decile 7', 'Decile 8'), 'Quintile 4', ifelse(IMD_Decile %in% c('Decile 9', 'Least deprived 10%'), 'Least deprived 20%', NA))))), levels = c('Most deprived 20%', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Least deprived 20%'))) %>% 
  group_by(IMD_Quintile) %>% 
  summarise(Numerator = sum(Numerator)) %>% 
  ungroup() %>% 
  mutate(Denominator = sum(Numerator)) %>% 
  mutate(Value = Numerator/Denominator * 100) %>% 
  mutate(Area_name = 'England') %>% 
  filter(IMD_Quintile == 'Most deprived 20%') %>% 
  mutate(England_Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         England_Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100)

National_deprivation_quintile_1_LTLA <- read_csv(url('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv')) %>% 
  select(LSOA11CD = `LSOA code (2011)`,  LTLA = `Local Authority District name (2019)`, IMD_Decile = "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)", Numerator = "Total population: mid 2015 (excluding prisoners)") %>% 
  filter(LTLA %in% ICT_areas) %>% 
  mutate(LTLA = factor(LTLA, levels = ICT_areas)) %>% 
  mutate(IMD_Decile = factor(ifelse(IMD_Decile == 1, 'Most deprived 10%',  ifelse(IMD_Decile == 2, 'Decile 2',  ifelse(IMD_Decile == 3, 'Decile 3',  ifelse(IMD_Decile == 4, 'Decile 4',  ifelse(IMD_Decile == 5, 'Decile 5',  ifelse(IMD_Decile == 6, 'Decile 6',  ifelse(IMD_Decile == 7, 'Decile 7',  ifelse(IMD_Decile == 8, 'Decile 8',  ifelse(IMD_Decile == 9, 'Decile 9',  ifelse(IMD_Decile == 10, 'Least deprived 10%', NA)))))))))), levels = c('Most deprived 10%', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 'Least deprived 10%'))) %>% 
  mutate(IMD_Quintile = factor(ifelse(IMD_Decile %in% c('Most deprived 10%', 'Decile 2'), 'Most deprived 20%', ifelse(IMD_Decile %in% c('Decile 3', 'Decile 4'), 'Quintile 2', ifelse(IMD_Decile %in% c('Decile 5', 'Decile 6'), 'Quintile 3', ifelse(IMD_Decile %in% c('Decile 7', 'Decile 8'), 'Quintile 4', ifelse(IMD_Decile %in% c('Decile 9', 'Least deprived 10%'), 'Least deprived 20%', NA))))), levels = c('Most deprived 20%', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Least deprived 20%'))) %>% 
  group_by(LTLA, IMD_Quintile) %>% 
  summarise(Numerator = sum(Numerator)) %>% 
  unique() %>% 
  ungroup() %>% 
  complete(IMD_Quintile, LTLA, fill = list(Numerator = 0)) %>% 
  group_by(LTLA) %>% 
  mutate(Denominator = sum(Numerator)) %>% 
  mutate(Value = Numerator/Denominator * 100) %>%
  filter(IMD_Quintile == 'Most deprived 20%') %>%
  select(Area_name = LTLA, Value, Numerator, Denominator, IMD_Quintile) %>% 
  bind_rows(England_deprivation_quintile_1[c('Area_name', 'Value', 'Numerator', 'Denominator')]) %>% 
  mutate(Indicator_short_name = 'National_deprivation_quintile_1') %>% 
  select(-IMD_Quintile) %>% 
  mutate(Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  bind_cols(England_deprivation_quintile_1[c('England_Lower_CI', 'England_Upper_CI')]) %>% 
  mutate(Significance_England = ifelse(Area_name == 'England', 'not_applicable', ifelse(Lower_CI > England_Upper_CI, 'high', ifelse(Upper_CI < England_Lower_CI, 'low', 'similar')))) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  mutate(Time_period = '2019') %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  filter(!Area_name %in% c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West'))
  
# Is National_deprivation_quintile_1 available for B&H ICTs
# BH_local_data %>%
#   filter(Indicator_short_name == 'National_deprivation_quintile_1') %>% 
#   mutate(Value_label = paste0(round((Value_label * 100), 1), '%'))

National_deprivation_quintile_1 <- BH_local_data %>%
  filter(Indicator_short_name == 'National_deprivation_quintile_1') %>% 
  mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>% 
  mutate(Time_period = '2019') %>% 
  select(Area_name,Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(National_deprivation_quintile_1_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) 

rm(England_deprivation_quintile_1, National_deprivation_quintile_1_LTLA)

# Children_living_in_poverty
Children_living_in_poverty_LTLA <- fingertips_data(IndicatorID = 90630,
                                                   AreaTypeID = 501,
                                                   categorytype = FALSE) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Children_living_in_poverty') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%'))

# Is Children_living_in_poverty available for B&H ICTs
# BH_local_data %>%
  # filter(Indicator_short_name == 'Children_living_in_poverty')
# no 

Children_living_in_poverty <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Children_living_in_poverty_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>%   bind_rows(Children_living_in_poverty_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Children_living_in_poverty_LTLA)

# This could be updated if we used IDACI from IMD 2019  

# Older_people_living_in_poverty
Older_people_living_in_poverty_LTLA <- fingertips_data(IndicatorID = 93279,
                                                   AreaTypeID = 'All',
                                                   categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Older_people_living_in_poverty') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%'))

# Is Older_people_living_in_poverty available for B&H ICTs
# BH_local_data %>%
# filter(Indicator_short_name == 'Older_people_living_in_poverty')
# no 

Older_people_living_in_poverty <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Older_people_living_in_poverty_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Older_people_living_in_poverty_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Older_people_living_in_poverty_LTLA)

# Building blocks 1 hex figure ####
hex_building_blocks_1_df <- Working_age_out_of_work %>% 
  bind_rows(Fuel_poverty) %>% 
  bind_rows(Housing_affordability_ratio) %>% 
  bind_rows(National_deprivation_quintile_1) %>% 
  bind_rows(Children_living_in_poverty) %>% 
  bind_rows(Older_people_living_in_poverty) %>% 
  left_join(hex_indicators, by = 'Indicator_short_name') %>% 
  mutate(Significance_England = factor(ifelse(Polarity == 'Not applicable', 'Not applicable', ifelse(Significance_England == 'high' & Polarity == 'Lower is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Higher is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Lower is better', 'Better', ifelse(Significance_England == 'high' & Polarity == 'Higher is better', 'Better', ifelse(Significance_England %in% c('Not compared', 'Not applicable','not_applicable'), 'Not applicable', ifelse(Significance_England %in% c('similar', 'Similar', 'same'), 'Similar', Significance_England))))))), levels = c('Better', 'Similar', 'Worse', 'Not applicable'))) %>% 
  mutate(Indicator_label_2 = gsub('(.{1,27})(\\s|$)', '\\1\n', Indicator_label)) %>% # add line break every 25 characters (or at the end of the word if 25 falls in the middle)
  mutate(Indicator_label_2 = ifelse(str_count(Indicator_label_2, '\\n') == 1, paste0(Indicator_label_2, '\n\n\n'), ifelse(str_count(Indicator_label_2, '\\n') == 2, paste0(Indicator_label_2, '\n\n'),ifelse(str_count(Indicator_label_2, '\\n') == 3, paste0(Indicator_label_2, '\n'), Indicator_label_2)))) # I think we need to have four lines of text to make each hex icon text line up. otherwise the lines start at different places for shorter indicators.
  
eng_hex_building_blocks_1_df <- hex_building_blocks_1_df %>% 
  filter(Area_name == 'England') %>% 
  select(Indicator_short_name, Eng_value_label = Value_label)

hex_building_blocks_1_df <- hex_building_blocks_1_df %>% 
  left_join(eng_hex_building_blocks_1_df, by = 'Indicator_short_name')

# hex_building_blocks_1_df %>% 
  # select(Indicator_label_2) %>% 
  # mutate(Indicator_label_2 = ifelse(str_count(Indicator_label_2, '\\n') == 1, paste0(Indicator_label_2, '\n\n\n'), ifelse(str_count(Indicator_label_2, '\\n') == 2, paste0(Indicator_label_2, '\n\n'),ifelse(str_count(Indicator_label_2, '\\n') == 3, paste0(Indicator_label_2, '\n'), Indicator_label_2)))) %>% 
  # unique() %>% 
  # mutate(n_lines = str_count(Indicator_label_2, '\\n'))

for(i in 1:length(ICT_areas)){

ICT_x <- ICT_areas[i]
Parent_area_x = ICT_areas_lookup %>% filter(Area == ICT_x) %>% select(UTLA) %>% as.character()

hex_df_i <- hex_building_blocks_1_df %>% 
  filter(Area_name == ICT_x) %>% 
  mutate(Ring_colour = factor(ifelse(At_ICT_level %in% c('UTLA'), 'Higher_geography_only', ifelse(Area_name %in% c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West') & At_ICT_level == 'LTLA', 'Higher_geography_only', At_ICT_level)),
                              levels = c('Higher_geography_only', 'LTLA', 'ICT level')))

plot_x <- ggplot(hex_df_i,
                 aes(x = x,
                     y = y)) +
  geom_star(starshape = 6,
            size = 70,
            aes(fill = Significance_England,
                colour = Ring_colour),
            starstroke  = 2) +
  scale_x_continuous(limits = c(min(hex_df_i$x)-.4,max(hex_df_i$x)+.4)) +
  scale_y_continuous(limits = c(min(hex_df_i$y)-.3, max(hex_df_i$y)+.3)) +
  scale_fill_manual(values = c(hex_colours_better, hex_colours_similar, hex_colours_worse, hex_colours_na),
                    drop = FALSE,
                    name = 'Compared to England') +
  scale_colour_manual(values = c('Higher_geography_only' = 'maroon', 'ICT level' = '#ffffff', 'LTLA' = '#ffffff'),
                      breaks = c('Higher_geography_only'),
                      label = paste0('Available at a higher geography (', Parent_area_x, ' local authority) only'),
                      name = '',
                      drop = FALSE) + 
  geom_text(aes(x - .35,
                y + .1,
                label = Value_label),
            hjust = 0,
            vjust = 0,
            fontface = 'bold',
            size = 9) +
  labs(title = paste0(unique(hex_df_i$Figure_ID), ' - ', ICT_x, ifelse(str_detect(ICT_x, 'Brighton and Hove'), '', ' ICT'))) +
  geom_text(aes(x - .35,
                y - .15,
                label = Indicator_label_2),
            hjust = 0,
            vjust = 0) +
  geom_text(aes(x - .35,
                y - .15,
                label = paste0('England: ', Eng_value_label, ', ', Time_period)),
            fontface = 'bold',
            hjust = 0,
            vjust = 0) +
  theme_hex() +
  guides(fill = guide_legend(override.aes = list(size = 11, colour = '#ffffff')),
         colour = guide_legend(override.aes = list(size = 10, drop.legend = FALSE))) +
  theme(legend.box="vertical",
        legend.margin=margin())

# each figure will have its own set height and width

# a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
svg(filename = paste0(local_output_directory, '/Building_blocks_1_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
    width = 9,
    height = 6.6)
print(plot_x)
dev.off()

png(filename = paste0(local_output_directory, '/Building_blocks_1_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
    width = 880,
    height = 630,
    res = 95)
print(plot_x)
dev.off()

# a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
svg(filename = paste0(synced_sp_directory, '/Hex_figures/Building_blocks_1/Building_blocks_1_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
    width = 9,
    height = 6.6)
print(plot_x)
dev.off()

png(filename = paste0(synced_sp_directory, '/Hex_figures/Building_blocks_1/Building_blocks_1_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
    width = 880,
    height = 630,
    res = 95)
print(plot_x)
dev.off()

}

# Building blocks 2 ####

# hex_indicators %>% 
  # filter(Figure_ID == 'Building blocks 2')  # we add this ID so we can join the data with the hex indicators
 
# 2021 LSOA lookup
# This is an output area lookup 
# oa21_lookup <- read_csv('https://www.arcgis.com/sharing/rest/content/items/792f7ab3a99d403ca02cc9ca1cf8af02/data')

# We can also create an LSOA lookup by subsetting this dataframe
# lsoa21_lookup <- oa21_lookup %>% 
#   select(LSOA21CD = lsoa21cd, LSOA21NM = lsoa21nm, MSOA21CD = msoa21cd, MSOA21NM = msoa21nm, LTLA = lad22nm) %>% 
#   unique() %>% 
#   filter(LTLA %in% ICT_areas)

police_df <- read_csv(paste0(local_data_directory, '/police_data_ICT_level.csv')) %>% 
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2023') %>% 
  select(!Value)

#View(police_df)

# Overall_crime_rate
# Violent_crime_rate    
# ASB_rate              

# Good_level_development_reception
Good_level_development_reception_raw <- fingertips_data(IndicatorID = 90631,
                AreaTypeID = 'All',
                categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Good_level_development_reception') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

Good_level_development_reception <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Good_level_development_reception_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(Good_level_development_reception_raw)

# Average_attainment_8_score_16

Average_attainment_8_score_16_LTLA <-  fingertips_data(IndicatorID = 93378,
                                                  AreaTypeID = 'All',
                                                  categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Average_attainment_8_score_16') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Time_period) %>% 
  unique() %>% 
  mutate(Significance_England = 'Not applicable')

#BH_local_data %>%
 # filter(Indicator_short_name == 'Average_attainment_8_score_16')

Average_attainment_8_score_16 <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Average_attainment_8_score_16_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Average_attainment_8_score_16_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

# Average_attainment_8_score_16_in_care

Average_attainment_8_score_children_in_care_UTLA <-  fingertips_data(IndicatorID = 93381,
                                                     AreaTypeID = 'All',
                                                     categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Average_attainment_8_score_children_in_care') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Indicator_short_name) %>% 
  unique() %>% 
  mutate(Significance_England = 'Not applicable')

Average_attainment_8_score_16_in_care <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Average_attainment_8_score_children_in_care_UTLA, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(Average_attainment_8_score_16_LTLA, Average_attainment_8_score_children_in_care_UTLA)

# Working with Census 2021 data, nomis, and R

# This is a really useful function from the nomisr package to help identify the table id you want.
#nomis_tables <- nomis_data_info() %>% 
 # select(id, components.dimension, name.value) # We really only need three fields from this table

#census_nomis_tables <- nomis_tables %>% 
 # filter(str_detect(name.value, '^TS'))

# census_LTLA_population_age_groups <- nomis_get_data(id = 'NM_2027_1',
#                                      time = 'latest', 
#                                      measures = '20100',
#                                      geography = 'TYPE154') %>% 
#   select(Area_name = GEOGRAPHY_NAME, Age_group = C2021_AGE_102_NAME, Population = OBS_VALUE)
# 
# census_Eng_population_age_groups <- nomis_get_data(id = 'NM_2027_1',
#                                                    time = 'latest', 
#                                                    measures = '20100',
#                                                    geography = 'TYPE499') %>% 
#   select(Area_name = GEOGRAPHY_NAME, Age_group = C2021_AGE_102_NAME, Population = OBS_VALUE)
# 
# 
# census_England_population_16_plus_df <- census_Eng_population_age_groups %>% 
#   filter(Age_group %in% c("Aged 16 to 19 years",  "Aged 20 to 24 years", "Aged 25 to 34 years", "Aged 35 to 49 years", "Aged 50 to 64 years", "Aged 65 to 74 years", "Aged 75 to 84 years", "Aged 85 years and over")) %>% 
#   filter(Area_name %in% c(ICT_areas, 'England')) %>% 
#   group_by(Area_name) %>% 
#   summarise(Age_group = '16+ years',
#             Population = sum(Population, na.rm = TRUE))
# 
# census_LTLA_population_16_plus_df <- census_LTLA_population_age_groups %>% 
#   filter(Age_group %in% c("Aged 16 to 19 years",  "Aged 20 to 24 years", "Aged 25 to 34 years", "Aged 35 to 49 years", "Aged 50 to 64 years", "Aged 65 to 74 years", "Aged 75 to 84 years", "Aged 85 years and over")) %>% 
#   filter(Area_name %in% c(ICT_areas, 'England')) %>% 
#   group_by(Area_name) %>% 
#   summarise(Age_group = '16+ years',
#             Population = sum(Population, na.rm = TRUE))
# 
# census_population_16_plus <- census_LTLA_population_16_plus_df %>% 
#   bind_rows(census_England_population_16_plus_df) %>% 
#   ungroup()

# nomis_get_data(id = 'NM_2084_1',
#                time = 'latest', 
#                measures = '20100',
#                geography = 'TYPE499') %>% View()

# People_no_qualifications
People_no_qualifications_England <- nomis_get_data(id = 'NM_2084_1',
                                                time = 'latest', 
                                                measures = '20100',
                                                geography = 'TYPE499') %>% 
  select(Area_name = GEOGRAPHY_NAME, Highest_qualification = C2021_HIQUAL_8_NAME, Numerator = OBS_VALUE) %>% 
  filter(Highest_qualification %in% c('Total: All usual residents aged 16 years and over', 'No qualifications')) %>% 
  pivot_wider(names_from = 'Highest_qualification',
              values_from = 'Numerator') %>% 
  rename(Numerator = 'No qualifications',
         Denominator = 'Total: All usual residents aged 16 years and over') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(England_Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         England_Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2021')

People_no_qualifications_LTLA <- nomis_get_data(id = 'NM_2084_1',
                                                time = 'latest', 
                                                measures = '20100',
                                                geography = 'TYPE154') %>% 
  select(Area_name = GEOGRAPHY_NAME, Highest_qualification = C2021_HIQUAL_8_NAME, Numerator = OBS_VALUE) %>% 
  filter(Highest_qualification %in% c('Total: All usual residents aged 16 years and over', 'No qualifications')) %>% 
  pivot_wider(names_from = 'Highest_qualification',
              values_from = 'Numerator') %>% 
  rename(Numerator = 'No qualifications',
         Denominator = 'Total: All usual residents aged 16 years and over') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  bind_cols(People_no_qualifications_England[c('England_Lower_CI', 'England_Upper_CI')]) %>% 
  mutate(Significance_England = ifelse(Area_name == 'England', 'not_applicable', ifelse(Lower_CI > England_Upper_CI, 'high', ifelse(Upper_CI < England_Lower_CI, 'low', 'similar')))) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_rows(People_no_qualifications_England) %>% 
  mutate(Indicator_short_name = 'People_no_qualifications') %>% 
  mutate(Time_period = '2021')

# BH_local_data %>%
# filter(Indicator_short_name == 'People_no_qualifications') %>% 
  # mutate(Value_label = paste0(round((Value_label * 100), 1), '%'))

People_no_qualifications <- BH_local_data %>%
  filter(Indicator_short_name == 'People_no_qualifications') %>% 
  mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>% 
  mutate(Time_period = '2021') %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(People_no_qualifications_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(People_no_qualifications_England, People_no_qualifications_LTLA)

# nomis_get_data(id = 'NM_2083_1',
#                time = 'latest', 
#                measures = '20100',
#                geography = 'TYPE499') %>% View()

# Working_age_in_employment
Working_age_in_employment_England <- nomis_get_data(id = 'NM_2083_1',
                                                     time = 'latest', 
                                                     measures = '20100',
                                                     geography = 'TYPE499') %>% 
    select(Area_name = GEOGRAPHY_NAME, Economically_active = C2021_EASTAT_20_NAME, Numerator = OBS_VALUE) %>% 
    filter(Economically_active %in% c('Total: All usual residents aged 16 years and over', 'Economically active (excluding full-time students):In employment')) %>% 
    pivot_wider(names_from = 'Economically_active',
                values_from = 'Numerator') %>% 
    rename(Numerator = 'Economically active (excluding full-time students):In employment',
           Denominator = 'Total: All usual residents aged 16 years and over') %>% 
    filter(Area_name %in% c(ICT_areas, 'England')) %>% 
    mutate(Value = Numerator/ Denominator * 100) %>% 
    mutate(England_Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
           England_Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
    mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
    mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2021')

Working_age_in_employment_LTLA <- nomis_get_data(id = 'NM_2083_1',
                                                  time = 'latest', 
                                                  measures = '20100',
                                                  geography = 'TYPE154') %>% 
  select(Area_name = GEOGRAPHY_NAME, Economically_active = C2021_EASTAT_20_NAME, Numerator = OBS_VALUE) %>% 
  filter(Economically_active %in% c('Total: All usual residents aged 16 years and over', 'Economically active (excluding full-time students):In employment')) %>% 
  pivot_wider(names_from = 'Economically_active',
              values_from = 'Numerator') %>% 
  rename(Numerator = 'Economically active (excluding full-time students):In employment',
         Denominator = 'Total: All usual residents aged 16 years and over') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
    mutate(Value = Numerator/ Denominator * 100) %>% 
    mutate(Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
           Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
    bind_cols(Working_age_in_employment_England[c('England_Lower_CI', 'England_Upper_CI')]) %>% 
    mutate(Significance_England = ifelse(Area_name == 'England', 'not_applicable', ifelse(Lower_CI > England_Upper_CI, 'high', ifelse(Upper_CI < England_Lower_CI, 'low', 'similar')))) %>% 
    mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
    bind_rows(Working_age_in_employment_England) %>% 
    mutate(Indicator_short_name = 'Working_age_in_employment') %>% 
  mutate(Time_period = '2021')

# BH_local_data %>%
#   filter(Indicator_short_name == 'Working_age_in_employment') %>% 
#   mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>% 
#   mutate(Time_period = '2021') %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 


Working_age_in_employment <- BH_local_data %>%
  filter(Indicator_short_name == 'Working_age_in_employment') %>% 
  mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>% 
  mutate(Time_period = '2021') %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Working_age_in_employment_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Working_age_in_employment_England, Working_age_in_employment_LTLA)

# nomis_get_data(id = 'NM_2026_1',
#                time = 'latest', 
#                measures = '20100',
#                geography = 'TYPE499') %>%  View()

# Population_density
Population_density_England <- nomis_get_data(id = 'NM_2026_1',
                                          time = 'latest', 
                                          measures = '20100',
                                          geography = 'TYPE499') %>% 
  select(Area_name = GEOGRAPHY_NAME, Persons_per_square_kilometre = OBS_VALUE) %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Time_period = '2021')
  
Population_density_LTLA <- nomis_get_data(id = 'NM_2026_1',
                                          time = 'latest', 
                                          measures = '20100',
                                          geography = 'TYPE154') %>% 
  select(Area_name = GEOGRAPHY_NAME, Persons_per_square_kilometre = OBS_VALUE) %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  bind_rows(Population_density_England) %>% 
  mutate(Indicator_short_name = 'Population_density',
         Significance_England = 'Not applicable') %>% 
  mutate(Value_label = format(round(Persons_per_square_kilometre,0), big.mark = ',', trim = TRUE)) %>% 
  mutate(Time_period = '2021')

# BH_local_data %>%
#   filter(Indicator_short_name == 'Population_density') %>% 
#   mutate(Value_label = paste0(format(Value_label, big.mark = ',', trim = TRUE))) %>% 
#   mutate(Time_period = '2021') %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

Population_density <- BH_local_data %>%
  filter(Indicator_short_name == 'Population_density') %>% 
  mutate(Value_label = paste0(format(Value_label, big.mark = ',', trim = TRUE))) %>% 
  mutate(Time_period = '2021') %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Population_density_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Population_density_England, Population_density_LTLA)

# HH_overcrowding
# nomis_get_data(id = 'NM_2071_1',
#                time = 'latest', 
#                measures = '20100',
#                geography = 'TYPE499') %>% View()

HH_overcrowding_England <- nomis_get_data(id = 'NM_2071_1',
                 time = 'latest', 
                 measures = '20100',
                 geography = 'TYPE499') %>% 
  select(Area_name = GEOGRAPHY_NAME, Occupancy_status = C2021_OCCRAT_ROOMS_6_NAME, Numerator = OBS_VALUE) %>% 
  filter(Occupancy_status %in% c('Total: All households', 'Occupancy rating of rooms: -1', 'Occupancy rating of rooms: -2 or less')) %>% 
  pivot_wider(names_from = 'Occupancy_status',
              values_from = 'Numerator') %>%
  mutate(Overcrowding_rating = `Occupancy rating of rooms: -1` + `Occupancy rating of rooms: -2 or less`) %>% 
  rename(Numerator = 'Overcrowding_rating',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(England_Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         England_Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2021')

HH_overcrowding_LTLA <- nomis_get_data(id = 'NM_2071_1',
                                  time = 'latest', 
                                  measures = '20100',
                                  geography = 'TYPE154') %>% 
  select(Area_name = GEOGRAPHY_NAME, Occupancy_status = C2021_OCCRAT_ROOMS_6_NAME, Numerator = OBS_VALUE) %>% 
  filter(Occupancy_status %in% c('Total: All households', 'Occupancy rating of rooms: -1', 'Occupancy rating of rooms: -2 or less')) %>% 
  pivot_wider(names_from = 'Occupancy_status',
              values_from = 'Numerator') %>%
  mutate(Overcrowding_rating = `Occupancy rating of rooms: -1` + `Occupancy rating of rooms: -2 or less`) %>% 
  rename(Numerator = 'Overcrowding_rating',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_cols(HH_overcrowding_England[c('England_Lower_CI', 'England_Upper_CI')]) %>% 
  mutate(Significance_England = ifelse(Area_name == 'England', 'not_applicable', ifelse(Lower_CI > England_Upper_CI, 'high', ifelse(Upper_CI < England_Lower_CI, 'low', 'similar')))) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_rows(HH_overcrowding_England) %>% 
  mutate(Indicator_short_name = 'HH_overcrowding')%>% 
  mutate(Time_period = '2021')

# BH_local_data %>%
#   filter(Indicator_short_name == 'HH_overcrowding') %>% 
#   mutate(Value_label = paste0(round(Value_label * 100,1), '%')) %>% 
#   mutate(Time_period = '2021') %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

HH_overcrowding <- BH_local_data %>%
  filter(Indicator_short_name == 'HH_overcrowding') %>% 
  mutate(Value_label = paste0(round(Value_label * 100,1), '%')) %>% 
  mutate(Time_period = '2021') %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(HH_overcrowding_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(HH_overcrowding_England, HH_overcrowding_LTLA)

# HH_no_central_heating
# nomis_get_data(id = 'NM_2064_1',
#                time = 'latest', 
#                measures = '20100',
#                geography = 'TYPE499') %>% View()

HH_no_central_heating_England <- nomis_get_data(id = 'NM_2064_1',
                                          time = 'latest', 
                                          measures = '20100',
                                          geography = 'TYPE499') %>% 
  select(Area_name = GEOGRAPHY_NAME, Heating_status = C2021_HEATING_13_NAME, Numerator = OBS_VALUE) %>% 
  filter(Heating_status %in% c('Total: All households', 'No central heating')) %>% 
  pivot_wider(names_from = 'Heating_status',
              values_from = 'Numerator') %>%
  rename(Numerator = 'No central heating',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(England_Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         England_Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2021')

HH_no_central_heating_LTLA <- nomis_get_data(id = 'NM_2064_1',
                                        time = 'latest', 
                                        measures = '20100',
                                        geography = 'TYPE154') %>% 
  select(Area_name = GEOGRAPHY_NAME, Heating_status = C2021_HEATING_13_NAME, Numerator = OBS_VALUE) %>% 
  filter(Heating_status %in% c('Total: All households', 'No central heating')) %>% 
  pivot_wider(names_from = 'Heating_status',
              values_from = 'Numerator') %>%
  rename(Numerator = 'No central heating',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_cols(HH_no_central_heating_England[c('England_Lower_CI', 'England_Upper_CI')]) %>% 
  mutate(Significance_England = ifelse(Area_name == 'England', 'not_applicable', ifelse(Lower_CI > England_Upper_CI, 'high', ifelse(Upper_CI < England_Lower_CI, 'low', 'similar')))) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_rows(HH_no_central_heating_England) %>% 
  mutate(Indicator_short_name = 'HH_no_central_heating') %>% 
  mutate(Time_period = '2021')

# BH_local_data %>%
#   filter(Indicator_short_name == 'HH_no_central_heating') %>% 
#   mutate(Value_label = paste0(round(Value_label * 100,1), '%')) %>% 
#   mutate(Time_period = '2021') %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

HH_no_central_heating <- BH_local_data %>%
  filter(Indicator_short_name == 'HH_no_central_heating') %>% 
  mutate(Value_label = paste0(round(Value_label * 100,1), '%')) %>% 
  mutate(Time_period = '2021') %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(HH_no_central_heating_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(HH_no_central_heating_England, HH_no_central_heating_LTLA)

# HH_private_rented
# nomis_get_data(id = 'NM_2072_1',
#                time = 'latest', 
#                measures = '20100',
#                geography = 'TYPE499') %>% View()

HH_private_rented_England <- nomis_get_data(id = 'NM_2072_1',
                                                time = 'latest', 
                                                measures = '20100',
                                                geography = 'TYPE499') %>% 
  select(Area_name = GEOGRAPHY_NAME, Tenure_status = C2021_TENURE_9_NAME, Numerator = OBS_VALUE) %>% 
  filter(Tenure_status %in% c('Total: All households', 'Private rented: Private landlord or letting agency')) %>% 
  pivot_wider(names_from = 'Tenure_status',
              values_from = 'Numerator') %>%
  rename(Numerator = 'Private rented: Private landlord or letting agency',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(England_Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         England_Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2021')

HH_private_rented_LTLA <- nomis_get_data(id = 'NM_2072_1',
                                            time = 'latest', 
                                            measures = '20100',
                                            geography = 'TYPE154') %>% 
  select(Area_name = GEOGRAPHY_NAME, Tenure_status = C2021_TENURE_9_NAME, Numerator = OBS_VALUE) %>% 
  filter(Tenure_status %in% c('Total: All households', 'Private rented: Private landlord or letting agency')) %>% 
  pivot_wider(names_from = 'Tenure_status',
              values_from = 'Numerator') %>% 
  rename(Numerator = 'Private rented: Private landlord or letting agency',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_cols(HH_private_rented_England[c('England_Lower_CI', 'England_Upper_CI')]) %>% 
  mutate(Significance_England = ifelse(Area_name == 'England', 'not_applicable', ifelse(Lower_CI > England_Upper_CI, 'high', ifelse(Upper_CI < England_Lower_CI, 'low', 'similar')))) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_rows(HH_private_rented_England) %>% 
  mutate(Indicator_short_name = 'HH_private_rented') %>% 
  mutate(Time_period = '2021')

# BH_local_data %>%
#   filter(Indicator_short_name == 'HH_private_rented') %>% 
#   mutate(Value_label = paste0(round(Value_label * 100,1), '%')) %>% 
#   mutate(Time_period = '2021') %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

HH_private_rented <- BH_local_data %>%
  filter(Indicator_short_name == 'HH_private_rented') %>% 
  mutate(Value_label = paste0(round(Value_label * 100,1), '%')) %>% 
  mutate(Time_period = '2021') %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(HH_private_rented_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(HH_private_rented_England, HH_private_rented_LTLA)

# HH_at_least_one_resident

# nomis_get_data(id = 'NM_2037_1',
#                time = 'latest', 
#                measures = '20100',
#                geography = 'TYPE499') %>% View()

HH_at_least_one_resident_England <- nomis_get_data(id = 'NM_2037_1',
                                                   time = 'latest', 
                                                   measures = '20100',
                                                   geography = 'TYPE499') %>% 
  select(Area_name = GEOGRAPHY_NAME, Size = C2021_HHSIZE_10_NAME, Numerator = OBS_VALUE) %>% 
  pivot_wider(names_from = 'Size',
              values_from = 'Numerator') %>%
  mutate(Atleast_one = `1 person in household` + `2 people in household` + `3 people in household` + `4 people in household` + `5 people in household` + `6 people in household` + `7 people in household` + `8 or more people in household`) %>% 
  select(Area_name, Atleast_one, 'Total: All households') %>% 
  rename(Numerator = 'Atleast_one',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value_label = format(Numerator, big.mark = ',')) %>% 
  mutate(Time_period = '2021')

HH_at_least_one_resident_LTLA <- nomis_get_data(id = 'NM_2037_1',
                                      time = 'latest', 
                                      measures = '20100',
                                      geography = 'TYPE154') %>% 
  select(Area_name = GEOGRAPHY_NAME, Size = C2021_HHSIZE_10_NAME, Numerator = OBS_VALUE) %>% 
  pivot_wider(names_from = 'Size',
              values_from = 'Numerator') %>%
  mutate(Atleast_one = `1 person in household` + `2 people in household` + `3 people in household` + `4 people in household` + `5 people in household` + `6 people in household` + `7 people in household` + `8 or more people in household`) %>% 
  select(Area_name, Atleast_one, 'Total: All households') %>% 
  rename(Numerator = 'Atleast_one',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value_label = format(Numerator, big.mark = ',')) %>% 
  bind_rows(HH_at_least_one_resident_England) %>% 
  mutate(Indicator_short_name = 'HH_at_least_one_resident') %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2021')

# BH_local_data %>%
#   filter(Indicator_short_name == 'HH_at_least_one_resident') %>% 
#   mutate(Value_label = paste0(format(Value_label, big.mark = ',', trim = TRUE))) %>% 
#   mutate(Time_period = '2021') %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

HH_at_least_one_resident <- BH_local_data %>%
  filter(Indicator_short_name == 'HH_at_least_one_resident') %>% 
  mutate(Value_label = paste0(format(Value_label, big.mark = ',', trim = TRUE))) %>% 
  mutate(Time_period = '2021') %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(HH_at_least_one_resident_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(HH_at_least_one_resident_England, HH_at_least_one_resident_LTLA)

# HH_living_alone_under_66
# nomis_get_data(id = 'NM_2023_1',
#                time = 'latest', 
#                measures = '20100',
#                geography = 'TYPE499') %>% View()

HH_living_alone_under_66_England <- nomis_get_data(id = 'NM_2023_1',
               time = 'latest', 
               measures = '20100',
               geography = 'TYPE499') %>% 
  select(Area_name = GEOGRAPHY_NAME, Composition = C2021_HHCOMP_15_NAME, Numerator = OBS_VALUE) %>% 
  filter(Composition %in% c('Total: All households', 'One-person household', 'One-person household: Aged 66 years and over', 'One-person household: Other')) %>% 
  pivot_wider(names_from = 'Composition',
              values_from = 'Numerator') %>%
  rename(Numerator = 'One-person household: Other',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(England_Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         England_Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2021')

HH_living_alone_under_66_LTLA <- nomis_get_data(id = 'NM_2023_1',
                                    time = 'latest', 
                                    measures = '20100',
                                    geography = 'TYPE154') %>% 
  select(Area_name = GEOGRAPHY_NAME, Composition = C2021_HHCOMP_15_NAME, Numerator = OBS_VALUE) %>% 
  filter(Composition %in% c('Total: All households', 'One-person household', 'One-person household: Aged 66 years and over', 'One-person household: Other')) %>% 
  pivot_wider(names_from = 'Composition',
              values_from = 'Numerator') %>%
  rename(Numerator = 'One-person household: Other',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_cols(HH_living_alone_under_66_England[c('England_Lower_CI', 'England_Upper_CI')]) %>% 
  mutate(Significance_England = ifelse(Area_name == 'England', 'not_applicable', ifelse(Lower_CI > England_Upper_CI, 'high', ifelse(Upper_CI < England_Lower_CI, 'low', 'similar')))) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_rows(HH_living_alone_under_66_England) %>% 
  mutate(Indicator_short_name = 'HH_living_alone_under_66')  %>% 
  mutate(Time_period = '2021')

# BH_local_data %>%
#   filter(Indicator_short_name == 'HH_living_alone_under_66') %>% 
#   mutate(Value_label = paste0(round(Value_label * 100, 1), '%')) %>% 
#   mutate(Time_period = '2021') %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

HH_living_alone_under_66 <-BH_local_data %>%
  filter(Indicator_short_name == 'HH_living_alone_under_66') %>% 
  mutate(Value_label = paste0(round(Value_label * 100, 1), '%')) %>% 
  mutate(Time_period = '2021') %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(HH_living_alone_under_66_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(HH_living_alone_under_66_England, HH_living_alone_under_66_LTLA)

# HH_living_alone_over_66
HH_living_alone_over_66_England <- nomis_get_data(id = 'NM_2023_1',
                                                   time = 'latest', 
                                                   measures = '20100',
                                                   geography = 'TYPE499') %>% 
  select(Area_name = GEOGRAPHY_NAME, Composition = C2021_HHCOMP_15_NAME, Numerator = OBS_VALUE) %>% 
  filter(Composition %in% c('Total: All households', 'One-person household', 'One-person household: Aged 66 years and over', 'One-person household: Other')) %>% 
  pivot_wider(names_from = 'Composition',
              values_from = 'Numerator') %>%
  rename(Numerator = 'One-person household: Aged 66 years and over',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(England_Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         England_Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2021')

HH_living_alone_over_66_LTLA <- nomis_get_data(id = 'NM_2023_1',
                                           time = 'latest', 
                                           measures = '20100',
                                           geography = 'TYPE154') %>% 
  select(Area_name = GEOGRAPHY_NAME, Composition = C2021_HHCOMP_15_NAME, Numerator = OBS_VALUE) %>% 
  filter(Composition %in% c('Total: All households', 'One-person household', 'One-person household: Aged 66 years and over', 'One-person household: Other')) %>% 
  pivot_wider(names_from = 'Composition',
              values_from = 'Numerator') %>%
  rename(Numerator = 'One-person household: Aged 66 years and over',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_cols(HH_living_alone_over_66_England[c('England_Lower_CI', 'England_Upper_CI')]) %>% 
  mutate(Significance_England = ifelse(Area_name == 'England', 'not_applicable', ifelse(Lower_CI > England_Upper_CI, 'high', ifelse(Upper_CI < England_Lower_CI, 'low', 'similar')))) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_rows(HH_living_alone_over_66_England) %>% 
  mutate(Indicator_short_name = 'HH_living_alone_over_66') %>% 
  mutate(Time_period = '2021')

# BH_local_data %>%
#   filter(Indicator_short_name == 'HH_living_alone_over_66') %>% 
#   mutate(Value_label = paste0(round(Value_label * 100, 1), '%')) %>% 
#   mutate(Time_period = '2021') %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

HH_living_alone_over_66 <- BH_local_data %>%
  filter(Indicator_short_name == 'HH_living_alone_over_66') %>% 
  mutate(Value_label = paste0(round(Value_label * 100, 1), '%')) %>% 
  mutate(Time_period = '2021') %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(HH_living_alone_over_66_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(HH_living_alone_over_66_England, HH_living_alone_over_66_LTLA)

# HH_lone_parent
HH_lone_parent_England <- nomis_get_data(id = 'NM_2023_1',
                                                  time = 'latest', 
                                                  measures = '20100',
                                                  geography = 'TYPE499') %>% 
  select(Area_name = GEOGRAPHY_NAME, Composition = C2021_HHCOMP_15_NAME, Numerator = OBS_VALUE) %>% 
  filter(Composition %in% c('Total: All households', 'Single family household: Lone parent family: With dependent children')) %>% 
  pivot_wider(names_from = 'Composition',
              values_from = 'Numerator') %>%
  rename(Numerator = 'Single family household: Lone parent family: With dependent children',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(England_Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         England_Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2021')

HH_lone_parent_LTLA <- nomis_get_data(id = 'NM_2023_1',
                                          time = 'latest', 
                                          measures = '20100',
                                          geography = 'TYPE154') %>% 
  select(Area_name = GEOGRAPHY_NAME, Composition = C2021_HHCOMP_15_NAME, Numerator = OBS_VALUE) %>% 
  filter(Composition %in% c('Total: All households', 'Single family household: Lone parent family: With dependent children')) %>% 
  pivot_wider(names_from = 'Composition',
              values_from = 'Numerator') %>%
  rename(Numerator = 'Single family household: Lone parent family: With dependent children',
         Denominator = 'Total: All households') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value = Numerator/ Denominator * 100) %>% 
  mutate(Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_cols(HH_lone_parent_England[c('England_Lower_CI', 'England_Upper_CI')]) %>% 
  mutate(Significance_England = ifelse(Area_name == 'England', 'not_applicable', ifelse(Lower_CI > England_Upper_CI, 'high', ifelse(Upper_CI < England_Lower_CI, 'low', 'similar')))) %>% 
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  bind_rows(HH_lone_parent_England) %>% 
  mutate(Indicator_short_name = 'HH_lone_parent')  %>% 
  mutate(Time_period = '2021')

# BH_local_data %>%
#   filter(Indicator_short_name == 'HH_lone_parent') %>% 
#   mutate(Value_label = paste0(round(Value_label * 100, 1), '%')) %>% 
#   mutate(Time_period = '2021') %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

HH_lone_parent <- BH_local_data %>%
  filter(Indicator_short_name == 'HH_lone_parent') %>% 
  mutate(Value_label = paste0(round(Value_label * 100, 1), '%')) %>% 
  mutate(Time_period = '2021') %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(HH_lone_parent_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(HH_lone_parent_England, HH_lone_parent_LTLA)

# Building blocks 2 hex figure ####
hex_building_blocks_2_df <- police_df %>% 
  bind_rows(Good_level_development_reception) %>% 
  bind_rows(Average_attainment_8_score_16) %>% 
  bind_rows(Average_attainment_8_score_16_in_care) %>% 
  bind_rows(People_no_qualifications) %>% 
  bind_rows(Working_age_in_employment) %>% 
  bind_rows(Population_density) %>% 
  bind_rows(HH_overcrowding) %>% 
  bind_rows(HH_no_central_heating) %>% 
  bind_rows(HH_private_rented) %>% 
  bind_rows(HH_at_least_one_resident) %>% 
  bind_rows(HH_living_alone_under_66) %>% 
  bind_rows(HH_living_alone_over_66) %>% 
  bind_rows(HH_lone_parent) %>% 
  left_join(hex_indicators, by = 'Indicator_short_name') %>% 
  mutate(Significance_England = factor(ifelse(Polarity == 'Not applicable', 'Not applicable', ifelse(Significance_England == 'high' & Polarity == 'Lower is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Higher is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Lower is better', 'Better', ifelse(Significance_England == 'high' & Polarity == 'Higher is better', 'Better', ifelse(Significance_England %in% c('Not compared', 'Not applicable','not_applicable'), 'Not applicable', ifelse(Significance_England %in% c('similar', 'Similar', 'same'), 'Similar', Significance_England))))))), levels = c('Better', 'Similar', 'Worse', 'Not applicable'))) %>% 
  mutate(Indicator_label_2 = gsub('(.{1,27})(\\s|$)', '\\1\n', Indicator_label)) %>% # add line break every 25 characters (or at the end of the word if 25 falls in the middle)
  mutate(Indicator_label_2 = ifelse(str_count(Indicator_label_2, '\\n') == 1, paste0(Indicator_label_2, '\n\n\n'), ifelse(str_count(Indicator_label_2, '\\n') == 2, paste0(Indicator_label_2, '\n\n'),ifelse(str_count(Indicator_label_2, '\\n') == 3, paste0(Indicator_label_2, '\n'), Indicator_label_2)))) # I think we need to have four lines of text to make each hex icon text line up. otherwise the lines start at different places for shorter indicators.

eng_hex_building_blocks_2_df <- hex_building_blocks_2_df %>% 
  filter(Area_name == 'England') %>% 
  select(Indicator_short_name, Eng_value_label = Value_label)

hex_building_blocks_2_df <- hex_building_blocks_2_df %>% 
  left_join(eng_hex_building_blocks_2_df, by = 'Indicator_short_name')

for(i in 1:length(ICT_areas)){
  
  ICT_x <- ICT_areas[i]
  Parent_area_x = ICT_areas_lookup %>% filter(Area == ICT_x) %>% select(UTLA) %>% as.character()
  
  hex_df_i <- hex_building_blocks_2_df %>% 
    filter(Area_name == ICT_x) %>% 
    mutate(Ring_colour = factor(ifelse(At_ICT_level %in% c('UTLA'), 'Higher_geography_only', ifelse(Area_name %in% c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West') & At_ICT_level == 'LTLA', 'Higher_geography_only', At_ICT_level)),
                                levels = c('Higher_geography_only', 'LTLA', 'ICT level')))
plot_x <- ggplot(hex_df_i,
         aes(x = x,
             y = y)) +
    geom_star(starshape = 6,
              size = 70,
              aes(fill = Significance_England,
                  colour = Ring_colour),
              starstroke  = 2) +
    scale_x_continuous(limits = c(min(hex_df_i$x)-.4,max(hex_df_i$x)+.4)) +
    scale_y_continuous(limits = c(min(hex_df_i$y)-.3, max(hex_df_i$y)+.3)) +
    scale_fill_manual(values = c(hex_colours_better, hex_colours_similar, hex_colours_worse, hex_colours_na),
                      drop = FALSE,
                      name = 'Compared to England') +
    scale_colour_manual(values = c('Higher_geography_only' = 'maroon', 'ICT level' = '#ffffff', 'LTLA' = '#ffffff'),
                        breaks = c('Higher_geography_only'),
                        label = paste0('Available at a higher geography (', Parent_area_x, ' local authority) only'),
                        name = '',
                        drop = FALSE) + 
    geom_text(aes(x - .35,
                  y + .1,
                  label = Value_label),
              hjust = 0,
              vjust = 0,
              fontface = 'bold',
              size = 9) +
    labs(title = paste0(unique(hex_df_i$Figure_ID), ' - ', ICT_x, ifelse(str_detect(ICT_x, 'Brighton and Hove'), '', ' ICT'))) +
    geom_text(aes(x - .35,
                  y - .15,
                  label = Indicator_label_2),
              hjust = 0,
              vjust = 0) +
    geom_text(aes(x - .35,
                  y - .15,
                  label = paste0('England: ', Eng_value_label, ', ', Time_period)),
              fontface = 'bold',
              hjust = 0,
              vjust = 0) +
    theme_hex() +
    guides(fill = guide_legend(override.aes = list(size = 11, colour = '#ffffff')),
           colour = guide_legend(override.aes = list(size = 10, drop.legend = FALSE))) +
    theme(legend.box="vertical",
          legend.margin=margin())
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(local_output_directory, '/Building_blocks_2_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 14,
      height = 11)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(local_output_directory, '/Building_blocks_2_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1200,
      height = 1020,
      res = 90)
  print(plot_x)
  dev.off()
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(synced_sp_directory, '/Hex_figures/Building_blocks_2/Building_blocks_2_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 14,
      height = 11)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(synced_sp_directory, '/Hex_figures/Building_blocks_2/Building_blocks_2_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1200,
      height = 1020,
      res = 90)
  print(plot_x)
  dev.off()
  
}

# Life Expectancy ####

# hex_indicators %>% 
  # filter(Figure_ID == 'Life expectancy disease burden')  # we add this ID so we can join the data with the hex indicators

# HLE_males_65
HLE_males_65_raw  <- fingertips_data(IndicatorID = 93505,
                                                        AreaTypeID = 'All',
                                                        categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Male') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'HLE_males_65') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

HLE_males_65 <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(HLE_males_65_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(HLE_males_65_raw)

# HLE_females_65
HLE_females_65_raw  <- fingertips_data(IndicatorID = 93505 ,
                                     AreaTypeID = 'All',
                                     categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'HLE_females_65') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

HLE_females_65 <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(HLE_females_65_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(HLE_females_65_raw)

# HLE_males_at_birth
HLE_males_at_birth_raw <- fingertips_data(IndicatorID = 90362 ,
                                     AreaTypeID = 'All',
                                     categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Male') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'HLE_males_at_birth') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

HLE_males_at_birth <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(HLE_males_at_birth_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(HLE_males_at_birth_raw)

# HLE_females_at_birth
HLE_females_at_birth_raw  <- fingertips_data(IndicatorID = 90362 ,
                                       AreaTypeID = 'All',
                                       categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'HLE_females_at_birth') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

HLE_females_at_birth <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(HLE_females_at_birth_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(HLE_females_at_birth_raw)

# no data available for slope inequality for this time period
# 
# # SII_HLE_inequality_males_at_birth
# SII_HLE_inequality_males_at_birth_raw <- fingertips_data(IndicatorID = 92031,
#                                           AreaTypeID = 502,
#                                           categorytype = FALSE) %>%
#   filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
#   filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
#   filter(Sex == 'Male') %>% 
#   filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
#   select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
#   unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
#   mutate(Indicator_short_name = 'SII_HLE_inequality_males_at_birth') %>% # we add this ID so we can join the data with the hex indicators
#   mutate(Value_label = paste0(round(Value, 1))) %>% 
#   select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
#   unique()
# 
# SII_HLE_inequality_males_at_birth <- ICT_areas_lookup %>% 
#   bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
#   left_join(SII_HLE_inequality_males_at_birth_raw, by = c('UTLA' = 'Area_name')) %>% 
#   # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
#   rename(Area_name = Area) %>% 
#   select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)
# 
# rm(SII_HLE_inequality_males_at_birth_raw)

# LE_males_65
LE_males_65_LTLA  <- fingertips_data(IndicatorID = 91102 ,
                                     AreaTypeID = 'All',
                                     categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Male') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'LE_males_65') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1)))

LE_males_65 <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(LE_males_65_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(LE_males_65_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(LE_males_65_LTLA)

# LE_females_65
LE_females_65_LTLA  <- fingertips_data(IndicatorID = 91102 ,
                                     AreaTypeID = 'All',
                                     categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'LE_females_65') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1)))

LE_females_65 <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(LE_females_65_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(LE_females_65_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(LE_females_65_LTLA)

# SII_LE_inequality_males_65
SII_LE_inequality_males_65_LTLA  <- fingertips_data(IndicatorID = 93190 ,
                                     AreaTypeID = 'All',
                                     categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Male') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'SII_LE_inequality_males_65') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1)))

SII_LE_inequality_males_65 <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(SII_LE_inequality_males_65_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(SII_LE_inequality_males_65_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(SII_LE_inequality_males_65_LTLA)

# SII_LE_inequality_females_65
SII_LE_inequality_females_65_LTLA  <- fingertips_data(IndicatorID = 93190 ,
                                                    AreaTypeID = 'All',
                                                    categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'SII_LE_inequality_females_65') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1)))

SII_LE_inequality_females_65 <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(SII_LE_inequality_females_65_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(SII_LE_inequality_females_65_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(SII_LE_inequality_females_65_LTLA)

# LE_males_at_birth
LE_males_at_birth_LTLA  <- fingertips_data(IndicatorID = 90366 ,
                                     AreaTypeID = 'All',
                                     categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Male') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'LE_males_at_birth') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1)))

LE_males_at_birth <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(LE_males_at_birth_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(LE_males_at_birth_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(LE_males_at_birth_LTLA)

# LE_females_at_birth
LE_females_at_birth_LTLA  <- fingertips_data(IndicatorID = 90366 ,
                                           AreaTypeID = 'All',
                                           categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'LE_females_at_birth') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1)))

LE_females_at_birth <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(LE_females_at_birth_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(LE_females_at_birth_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(LE_females_at_birth_LTLA)

# SII_LE_inequality_males_at_birth
SII_LE_inequality_males_at_birth_LTLA  <- fingertips_data(IndicatorID = 92901,
                                           AreaTypeID = 'All',
                                           categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Male') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'SII_LE_inequality_males_at_birth') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1)))

SII_LE_inequality_males_at_birth <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(SII_LE_inequality_males_at_birth_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(SII_LE_inequality_males_at_birth_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(SII_LE_inequality_males_at_birth_LTLA)

# SII_LE_inequality_females_at_birth
SII_LE_inequality_females_at_birth_LTLA  <- fingertips_data(IndicatorID = 92901 ,
                                             AreaTypeID = 'All',
                                             categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'SII_LE_inequality_females_at_birth') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1)))

SII_LE_inequality_females_at_birth <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(SII_LE_inequality_females_at_birth_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(SII_LE_inequality_females_at_birth_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(SII_LE_inequality_females_at_birth_LTLA)

# Life expectancy hex figure ####
hex_life_expectancy_df <- HLE_males_65 %>% 
  bind_rows(HLE_females_65) %>% 
  bind_rows(HLE_males_at_birth) %>% 
  bind_rows(HLE_females_at_birth) %>% 
  bind_rows(LE_males_65) %>% 
  bind_rows(LE_females_65) %>% 
  bind_rows(SII_LE_inequality_males_65) %>% 
  bind_rows(SII_LE_inequality_females_65) %>% 
  bind_rows(LE_males_at_birth) %>% 
  bind_rows(LE_females_at_birth) %>% 
  bind_rows(SII_LE_inequality_males_at_birth) %>% 
  bind_rows(SII_LE_inequality_females_at_birth) %>% 
  left_join(hex_indicators, by = 'Indicator_short_name') %>% 
  mutate(Significance_England = factor(ifelse(Polarity == 'Not applicable', 'Not applicable', ifelse(Significance_England == 'high' & Polarity == 'Lower is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Higher is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Lower is better', 'Better', ifelse(Significance_England == 'high' & Polarity == 'Higher is better', 'Better', ifelse(Significance_England %in% c('Not compared', 'Not applicable','not_applicable'), 'Not applicable', ifelse(Significance_England %in% c('similar', 'Similar', 'same'), 'Similar', Significance_England))))))), levels = c('Better', 'Similar', 'Worse', 'Not applicable'))) %>% 
  mutate(Indicator_label_2 = gsub('(.{1,27})(\\s|$)', '\\1\n', Indicator_label)) %>% # add line break every 25 characters (or at the end of the word if 25 falls in the middle)
  mutate(Indicator_label_2 = ifelse(str_count(Indicator_label_2, '\\n') == 1, paste0(Indicator_label_2, '\n\n\n'), ifelse(str_count(Indicator_label_2, '\\n') == 2, paste0(Indicator_label_2, '\n\n'),ifelse(str_count(Indicator_label_2, '\\n') == 3, paste0(Indicator_label_2, '\n'), Indicator_label_2)))) # I think we need to have four lines of text to make each hex icon text line up. otherwise the lines start at different places for shorter indicators.

eng_hex_life_expectancy_df <- hex_life_expectancy_df %>% 
  filter(Area_name == 'England') %>% 
  select(Indicator_short_name, Eng_value_label = Value_label)

hex_life_expectancy_df <- hex_life_expectancy_df %>% 
  left_join(eng_hex_life_expectancy_df, by = 'Indicator_short_name')

for(i in 1:length(ICT_areas)){
  
  ICT_x <- ICT_areas[i]
  Parent_area_x = ICT_areas_lookup %>% filter(Area == ICT_x) %>% select(UTLA) %>% as.character()
  
  hex_df_i <- hex_life_expectancy_df %>% 
    filter(Area_name == ICT_x) %>% 
    mutate(Ring_colour = factor(ifelse(At_ICT_level %in% c('UTLA'), 'Higher_geography_only', ifelse(Area_name %in% c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West') & At_ICT_level == 'LTLA', 'Higher_geography_only', At_ICT_level)),
                                levels = c('Higher_geography_only', 'LTLA', 'ICT level')))
  plot_x <- ggplot(hex_df_i,
                   aes(x = x,
                       y = y)) +
    geom_star(starshape = 6,
              size = 70,
              aes(fill = Significance_England,
                  colour = Ring_colour),
              starstroke  = 2) +
    scale_x_continuous(limits = c(min(hex_df_i$x)-.4,max(hex_df_i$x)+.4)) +
    scale_y_continuous(limits = c(min(hex_df_i$y)-.3, max(hex_df_i$y)+.3)) +
    scale_fill_manual(values = c(hex_colours_better, hex_colours_similar, hex_colours_worse, hex_colours_na),
                      drop = FALSE,
                      name = 'Compared to England') +
    scale_colour_manual(values = c('Higher_geography_only' = 'maroon', 'ICT level' = '#ffffff', 'LTLA' = '#ffffff'),
                        breaks = c('Higher_geography_only'),
                        label = paste0('Available at a higher geography (', Parent_area_x, ' local authority) only'),
                        name = '',
                        drop = FALSE) + 
    geom_text(aes(x - .4,
                  y + .1,
                  label = paste0(Value_label, ' years')),
              hjust = 0,
              vjust = 0,
              fontface = 'bold',
              size = 9) +
    labs(title = paste0(unique(hex_df_i$Figure_ID), ' - ', ICT_x, ifelse(str_detect(ICT_x, 'Brighton and Hove'), '', ' ICT'))) +
    geom_text(aes(x - .4,
                  y - .18,
                  label = Indicator_label_2),
              hjust = 0,
              vjust = 0) +
    geom_text(aes(x - .4,
                  y - .15,
                  label = paste0('England: ', Eng_value_label, ', ', Time_period)),
              fontface = 'bold',
              hjust = 0,
              vjust = 0) +
    theme_hex() +
    guides(fill = guide_legend(override.aes = list(size = 11, colour = '#ffffff')),
           colour = guide_legend(override.aes = list(size = 10, drop.legend = FALSE))) +
    theme(legend.box="vertical",
          legend.margin=margin())
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(local_output_directory, '/Life_expectancy_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 11,
      height = 11)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(local_output_directory, '/Life_expectancy_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1020,
      height = 1020,
      res = 90)
  print(plot_x)
  dev.off()
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(synced_sp_directory, '/Hex_figures/Life_expectancy/Life_expectancy_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 11,
      height = 11)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(synced_sp_directory, '/Hex_figures/Life_expectancy/Life_expectancy_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1020,
      height = 1020,
      res = 90)
  print(plot_x)
  dev.off()
  
}

# Cause of death

# LE_segment_tool_df <- read_csv(paste0(local_data_directory, '/Segment_context_data.csv'))

LE_segment_tool_df <- read_csv(paste0(local_data_directory, '/Segment_breakdowns_data.csv'))

condition_colours <- data.frame(Condition_group = c('COVID-19', 'Cancer', 'Circulatory', 'Respiratory', 'Digestive', 'External causes', 'Mental and behavioural', 'Other', 'Deaths under 28 days'), Colour = c("#cc5561","#c65933","#c99145","#85903b","#62b357","#48b1a7","#6f7ccb","#b05cc6","#c55d93"))

Latest_UTLA_COD_segment <- LE_segment_tool_df %>% 
  filter(CategoryType == 'Broad cause') %>% 
  filter(Comparison == 'Within area') %>% 
  filter(TimePeriod == '2020-21') %>% 
  select(Area_name = GeographyName, Sex, Time_period = TimePeriod, Condition = Category, Proportion = PercentageContribution) %>% 
  mutate(Proportion = Proportion / 100) %>% 
  mutate(Condition = factor(Condition, levels = c('COVID-19', 'Cancer', 'Circulatory', 'Respiratory', 'Digestive', 'External causes', 'Mental and behavioural', 'Other', 'Deaths under 28 days')))

Sussex_UTLA_plot_1 <- Latest_UTLA_COD_segment %>% 
  filter(Area_name %in% c(ICT_areas, 'Brighton and Hove', 'East Sussex', 'West Sussex', 'England')) %>% 
  mutate(Area_name = factor(Area_name, levels = c('Brighton and Hove', 'East Sussex', 'West Sussex', 'England'))) %>% 
  ggplot(aes(x = Sex,
             y = Proportion,
             fill = Condition)) +
  geom_bar(stat = 'identity',
           position = 'stack',
           colour = '#ffffff',
           size = 1) + 
  labs(title = paste0('Causes of death contributing most to inequalities in life expectancy between the most and least deprived quintiles of each area.'),
       subtitle = 'Data for 2020 and 2021 combined.',
       x = '',
       y = 'Proportion contributing to\nlife expectancy gap',
       caption = 'Data for lower tier local authorities has been included for 2014 to 2016 and 2017 to 2019, but have not been included for 2020 to 2021\nas the breakdowns based on 2 years of data are not robust due to small numbers.') +
  scale_fill_manual(values = condition_colours$Colour) +
  scale_y_continuous(labels = percent,
                     limits = c(0,1.01),
                     breaks = seq(0,1,.1),
                     expand = c(0,0.01)) +
  facet_rep_wrap(~Area_name,
                 nrow = 1) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 0, 
                                   hjust = .5),
        legend.position = 'right')

# a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
svg(filename = paste0(local_output_directory, '/Life_expectancy_segment_tool_Sussex_UTLAs.svg'),
    width = 11,
    height = 11)
print(Sussex_UTLA_plot_1)
dev.off()

png(filename = paste0(local_output_directory, '/Life_expectancy_segment_tool_Sussex_UTLAs.png'),
    width = 1000,
    height = 1000,
    res = 90)
print(Sussex_UTLA_plot_1)
dev.off()

# a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
svg(filename = paste0(synced_sp_directory, '/Hex_figures/Life_expectancy/Life_expectancy_segment_tool_Sussex_UTLAs.svg'),
    width = 11,
    height = 11)
print(Sussex_UTLA_plot_1)
dev.off()

png(filename = paste0(synced_sp_directory, '/Hex_figures/Life_expectancy/Life_expectancy_segment_tool_Sussex_UTLAs.png'),
    width = 1000,
    height = 1000,
    res = 90)
print(Sussex_UTLA_plot_1)
dev.off()

UTLAs <- c('Brighton and Hove', 'East Sussex', 'West Sussex')

for(i in 1:3){

UTLA_x <- UTLAs[i]  
  
Sussex_UTLA_plot_x <- Latest_UTLA_COD_segment %>% 
  filter(Area_name %in% UTLA_x) %>% 
  ggplot(aes(x = Sex,
             y = Proportion,
             fill = Condition)) +
  geom_bar(stat = 'identity',
           position = 'stack',
           colour = NA,
           size = 1) + 
  labs(title = paste0('Causes of death contributing most to inequalities in life expectancy\nbetween the most and least deprived quintiles of each area.'),
       subtitle = paste0(UTLA_x, '; data for 2020 and 2021 combined.'),
       x = '',
       y = 'Proportion contributing to\nlife expectancy gap',
       caption = 'Data for lower tier local authorities has been included for 2014 to 2016 and\n2017 to 2019, but have not been included for 2020 to 2021 as the breakdowns\nbased on 2 years of data are not robust due to small numbers.') +
  scale_fill_manual(values = viridis(length(levels(Latest_UTLA_COD_segment$Condition)))) +
  scale_y_continuous(labels = percent,
                     limits = c(0,1.01),
                     breaks = seq(0,1,.1),
                     expand = c(0,0.01)) +
  facet_rep_wrap(~Area_name,
                 nrow = 1) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 0, 
                                   hjust = .5),
        legend.position = 'right')


# a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
svg(filename = paste0(local_output_directory, '/Life_expectancy_segment_tool_', gsub(' ', '_', tolower(UTLA_x)) ,'.svg'),
    width = 11,
    height = 11)
print(Sussex_UTLA_plot_x)
dev.off()

png(filename = paste0(local_output_directory, '/Life_expectancy_segment_tool_', gsub(' ', '_', tolower(UTLA_x)) ,'.png'),
    width = 800,
    height = 800,
    res = 120)
print(Sussex_UTLA_plot_x)
dev.off()

# a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
svg(filename = paste0(synced_sp_directory, '/Hex_figures/Life_expectancy/Life_expectancy_segment_tool_', gsub(' ', '_', tolower(UTLA_x)) ,'.svg'),
    width = 11,
    height = 11)
print(Sussex_UTLA_plot_x)
dev.off()

png(filename = paste0(synced_sp_directory, '/Hex_figures/Life_expectancy/Life_expectancy_segment_tool_', gsub(' ', '_', tolower(UTLA_x)) ,'.png'),
    width = 800,
    height = 800,
    res = 120)
print(Sussex_UTLA_plot_x)
dev.off()

}

# Targeting the causes of death which contribute most to the life expectancy gap should have the biggest impact on reducing inequalities.

Sussex_UTLA_plot_2 <- Latest_UTLA_COD_segment %>% 
  filter(Area_name %in% c(ICT_areas, 'Brighton and Hove', 'East Sussex', 'West Sussex', 'England')) %>% 
  mutate(Area_name = factor(Area_name, levels = c('Brighton and Hove', 'East Sussex', 'West Sussex', 'England'))) %>% 
  ggplot(aes(x = Area_name,
             y = Proportion,
             fill = Condition)) +
  geom_bar(stat = 'identity',
           position = 'stack',
           colour = NA,
           size = 1) + 
  labs(title = paste0('Causes of death contributing most to inequalities in life expectancy between the most and least deprived quintiles of each area.'),
       subtitle = 'Data for 2020 and 2021 combined.',
       x = '',
       y = 'Proportion contributing to life expectancy gap',
       caption = 'Data for lower tier local authorities has been included for 2014 to 2016 and 2017 to 2019, but have not been included for 2020 to 2021\nas the breakdowns based on 2 years of data are not robust due to small numbers.') +
  scale_fill_manual(values = viridis(length(levels(Latest_UTLA_COD_segment$Condition))),
                    expand = c(0,0.01) ) +
  scale_y_continuous(labels = percent,
                     limits = c(0,1.01),
                     breaks = seq(0,1,.1),
                     expand = c(0,0.01)) +
  coord_flip() +
  facet_rep_wrap(~Sex,
                 nrow = 2,
                 repeat.tick.labels = TRUE) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 0, 
                                   hjust = .5),
        legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
svg(filename = paste0(local_output_directory, '/Life_expectancy_segment_tool_Sussex_UTLAs_plot_2.svg'),
    width = 13,
    height = 6)
print(Sussex_UTLA_plot_2)
dev.off()

png(filename = paste0(local_output_directory, '/Life_expectancy_segment_tool_Sussex_UTLAs_plot_2.png'),
    width = 1200,
    height = 600,
    res = 90)
print(Sussex_UTLA_plot_2)
dev.off()

# a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
svg(filename = paste0(synced_sp_directory, '/Hex_figures/Life_expectancy/Life_expectancy_segment_tool_Sussex_UTLAs_plot_2.svg'),
    width = 13,
    height = 6)
print(Sussex_UTLA_plot_2)
dev.off()

png(filename = paste0(synced_sp_directory, '/Hex_figures/Life_expectancy/Life_expectancy_segment_tool_Sussex_UTLAs_plot_2.png'),
    width = 1200,
    height = 600,
    res = 90)
print(Sussex_UTLA_plot_2)
dev.off()

# COD circle packing ####
Latest_LTLA_COD_segment <- LE_segment_tool_df %>%
  filter(CategoryType == 'Detailed cause') %>%
  filter(Comparison == 'Within area') %>%
  filter(TimePeriod == '2017-19') %>%
  select(Area_name = GeographyName, Sex, Time_period = TimePeriod, Condition = Category, Deaths = TotalDeaths) %>%
  mutate(Condition_group = factor(ifelse(Condition %in% c("Accidental poisoning",  "Land transport accidents",  "Other external",   "Suicide & injury of undetermined intent"), 'External causes', ifelse(Condition %in% c("Breast cancer (female only)",  "Colorectal cancer", "Leukaemia & lymphoma","Lung cancer",  "Prostate cancer" ,  "Other cancer"), 'Cancer', ifelse(Condition %in% c("Chronic lower respiratory diseases",  "Flu & pneumonia",  "Other respiratory"), 'Respiratory',  ifelse(Condition %in% c("Cirrhosis & liver disease",  "Other digestive"), 'Digestive', ifelse(Condition %in% c( "Dementia & Alzheimer's disease", "Other mental and behavioural"), 'Mental and behavioural', ifelse(Condition %in% c( "Heart disease",  "Stroke", "Other circulatory"), 'Circulatory', Condition)))))), levels = c('COVID-19','Cancer', 'Circulatory', 'Respiratory', 'Digestive', 'External causes', 'Mental and behavioural', 'Other', 'Deaths under 28 days'))) %>% 
  group_by(Area_name, Time_period, Condition, Condition_group) %>% 
  summarise(Deaths = sum(Deaths, na.rm = TRUE),
            Sex = 'Persons') %>% 
  filter(Area_name %in% ICT_areas) %>% 
  filter(Condition != 'COVID-19') %>% 
  ungroup()

for(i in 5:length(ICT_areas)){
ICT_x <- ICT_areas[i]

ICT_COD_x <- Latest_LTLA_COD_segment %>% 
  filter(Area_name == ICT_x) %>% 
  mutate(id = row_number())
  
# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(ICT_COD_x$Deaths, 
                                   sizetype = 'area')

# We can add these packing information to the initial data frame
ICT_COD_x <- ICT_COD_x %>% 
  bind_cols(packing)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
# plot(data$radius, data$value)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, 
                               npoints = 50) %>% 
  left_join(ICT_COD_x[c('id', 'Condition', 'Condition_group')], by = 'id')

# Make the plot
cause_of_death_plot_x <- ggplot() + 
  geom_polygon(data = dat.gg, 
               aes(x, 
                   y, 
                   group = id, 
                   fill = Condition_group),
               colour = '#ffffff') +
  scale_fill_manual(values = condition_colours$Colour,
                      drop = FALSE,
                      expand = c(0,0.01),
                    labels = c('COVID-19 (0 deaths before 2020)','Cancer (lung, prostate, colorectal,\nleukaemia, breast, and others)', 'Circulatory (heart disease,\nstroke, and others)', 'Respiratory (COPD, influenza\nand pneumonia and others)', 'Digestive (cirrhosis and other\nliver diseases and others)', 'External causes (accidental\npoisoning suicide and injury,\nland transport accidents and others)', "Mental and behavioural\n(dementia and Alzheimer's\nand others)", 'Other', 'Deaths under 28 days')) +
  labs(title = paste0('Causes of death; ', ICT_x),
       subtitle = 'Data for 2017 to 2019 combined.',
       caption = 'Labels provided for causes where the total number of deaths is 20 or more.\nConditions are grouped and coloured into broad causes (sub causes are given in brackets in the legend).') +
  geom_text(data = subset(ICT_COD_x, 
                          Deaths >= 20
                          ),
            aes(x,
                y,
                label = paste0(gsub('(.{1,15})(\\s|$)', '\\1\n', Condition),  Deaths))) +
  scale_size_continuous(range = c(1,4)) +
  theme_circles() + 
  theme(legend.position = 'right') +
  coord_equal() +
  guides(fill = guide_legend(override.aes = list(size = 10))) +
  theme(legend.box="vertical",
        legend.margin=margin())


# a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
svg(filename = paste0(local_output_directory, '/Cause_of_death_2017_2019_segment_tool_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
    width = 12,
    height = 9)
print(cause_of_death_plot_x)
dev.off()

png(filename = paste0(local_output_directory, '/Cause_of_death_2017_2019_segment_tool_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
    width = 1000,
    height = 800,
    res = 90)
print(cause_of_death_plot_x)
dev.off()

# a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
svg(filename = paste0(synced_sp_directory, '/Hex_figures/Life_expectancy/Cause_of_death_2017_2019_segment_tool_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
    width = 12,
    height = 9)
print(cause_of_death_plot_x)
dev.off()

png(filename = paste0(synced_sp_directory, '/Hex_figures/Life_expectancy/Cause_of_death_2017_2019_segment_tool_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
    width = 1000,
    height = 800,
    res = 90)
print(cause_of_death_plot_x)
dev.off()

}

# Starting well ####

hex_indicators %>% 
  filter(Figure_ID == 'Starting well')  # we add this ID so we can join the data with the hex indicators

# NEET
NEET_raw <- fingertips_data(IndicatorID = 93203 ,
                            AreaTypeID = 'All',
                            categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'NEET') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

NEET <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(NEET_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(NEET_raw)

# Care_leavers_in_EET
Care_leavers_in_EET_raw <- read_csv(paste0(local_data_directory, '/LG_inform_Care_leavers_EET.csv')) %>% 
  mutate(Value_label = paste0(Value, '%')) %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  filter(Area_name %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England'))

Care_leavers_in_EET <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Care_leavers_in_EET_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(!Value) %>% 
  select(!UTLA)

rm(Care_leavers_in_EET_raw)

# Hospital_admissions_uninentional_deliberate_harm_under_five
Hospital_admissions_uninentional_deliberate_harm_under_five_LTLA <-  fingertips_data(IndicatorID = 90832,
                                                       AreaTypeID = 'All',
                                                       categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Hospital_admissions_uninentional_deliberate_harm_under_five') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

Hospital_admissions_uninentional_deliberate_harm_under_five <- BH_local_data %>%
  filter(Indicator_short_name == 'Hospital_admissions_uninentional_deliberate_harm_under_five') %>% 
  mutate(Value_label = paste0(round(Value_label, 1))) %>% 
  mutate(Time_period = unique(Hospital_admissions_uninentional_deliberate_harm_under_five_LTLA$Time_period)) %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Hospital_admissions_uninentional_deliberate_harm_under_five_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Hospital_admissions_uninentional_deliberate_harm_under_five_LTLA)

# Hospital_admissions_asthma_under_19
Hospital_admissions_asthma_under_19_raw <-  fingertips_data(IndicatorID = 90810,
                                                            AreaTypeID = 402,
                                                            categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Hospital_admissions_asthma_under_19') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

#BH_local_data %>%
 # filter(Indicator_short_name == 'Hospital_admissions_asthma_under_19') %>% 

Hospital_admissions_asthma_under_19 <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Hospital_admissions_asthma_under_19_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(Hospital_admissions_asthma_under_19_raw)

# Hospital_admissions_self_harm_10_24
Hospital_admissions_self_harm_10_24_raw <-  fingertips_data(IndicatorID = 90813,
                                                            AreaTypeID = 'All',
                                                            categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
    filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
    filter(Sex == 'Persons') %>% 
    filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
    select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
    unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
    mutate(Indicator_short_name = 'Hospital_admissions_self_harm_10_24') %>% # we add this ID so we can join the data with the hex indicators
    mutate(Value_label = paste0(round(Value, 1))) %>% 
    select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
    unique()
  
Hospital_admissions_self_harm_10_24 <- ICT_areas_lookup %>% 
    bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
    left_join(Hospital_admissions_self_harm_10_24_raw, by = c('UTLA' = 'Area_name')) %>% 
    # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
    rename(Area_name = Area) %>% 
    select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) %>% 
  filter(!Area_name %in% c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove West', 'Brighton and Hove Central')) %>% 
  bind_rows(BH_local_data %>%
              filter(Indicator_short_name == 'Hospital_admissions_self_harm_10_24') %>% 
              mutate(Value_label = paste0(round(Value_label, 1))) %>% 
              mutate(Time_period = unique(Hospital_admissions_self_harm_10_24_raw$Time_period)) %>% 
              select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period))
  
rm(Hospital_admissions_self_harm_10_24_raw)

# Breastfeeding_six_weeks 

Breastfeeding_six_weeks_raw <-  fingertips_data(IndicatorID = 92517,
                                                  AreaTypeID = 'All',
                                                  categorytype = FALSE) %>%
    filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
    filter(Timeperiod == '2019/20') %>% 
    filter(Sex == 'Persons') %>% 
    filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
    select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
    unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
    mutate(Indicator_short_name = 'Breastfeeding_six_weeks') %>% # we add this ID so we can join the data with the hex indicators
    mutate(Value_label = paste0(round(Value, 1), '%*')) %>% 
    select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
    unique()
  
# BH_local_data %>%
#   filter(Indicator_short_name == 'Breastfeeding_six_weeks') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(Breastfeeding_six_weeks_raw$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Breastfeeding_six_weeks <- ICT_areas_lookup %>% 
    bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
    left_join(Breastfeeding_six_weeks_raw, by = c('UTLA' = 'Area_name')) %>% 
    # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
    rename(Area_name = Area) %>% 
    select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)
  
rm(Breastfeeding_six_weeks_raw)
  
# MMR_two_dose_5_years
MMR_two_dose_5_years_raw <-  fingertips_data(IndicatorID = 30311,
                                            AreaTypeID = 'All',
                                            categorytype = FALSE) %>%

  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'MMR_two_dose_5_years') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()
  
# BH_local_data %>%
#   filter(Indicator_short_name == 'MMR_two_dose_5_years') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(MMR_two_dose_5_years$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

MMR_two_dose_5_years <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(MMR_two_dose_5_years_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)
  
rm(MMR_two_dose_5_years_raw)
  
# Expected_communication_at_2
Expected_communication_at_2_raw <-  fingertips_data(IndicatorID = 93431,
                                             AreaTypeID = 'All',
                                             categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Expected_communication_at_2') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Expected_communication_at_2') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(Expected_communication_at_2_raw$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Expected_communication_at_2 <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Expected_communication_at_2_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(Expected_communication_at_2_raw)

# Obvious_tooth_decay_at_5

# Here there is only data for Brighton and Hove and England for the most recent time period due to small numbers suppression, i have made the decision to use the most recently available data for all areas but this is more out of date.
# Alternatively I can use the 2021/22 data but it would be South East region 

Obvious_tooth_decay_at_5_LTLA <-  fingertips_data(IndicatorID = 93563,
                                                  AreaTypeID = 'All',
                                                  categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  # filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Timeperiod == '2016/17') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Obvious_tooth_decay_at_5') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%*')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Obvious_tooth_decay_at_5') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(Obvious_tooth_decay_at_5_LTLA$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Obvious_tooth_decay_at_5 <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Obvious_tooth_decay_at_5_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Obvious_tooth_decay_at_5_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Obvious_tooth_decay_at_5_LTLA)

# Healthy_weight_year_6
Healthy_weight_year_6_LTLA <-  fingertips_data(IndicatorID = 90321,
                                                  AreaTypeID = 'All',
                                                  categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  # filter(Timeperiod == '2016/17') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Healthy_weight_year_6') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Healthy_weight_year_6') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(Healthy_weight_year_6_LTLA$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Healthy_weight_year_6 <- BH_local_data %>%
  filter(Indicator_short_name == 'Healthy_weight_year_6') %>% 
  mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>% 
  mutate(Time_period = unique(Healthy_weight_year_6_LTLA$Time_period)) %>% 
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Healthy_weight_year_6_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Healthy_weight_year_6_LTLA)
  
# Infant_mortality_rate
Infant_mortality_rate_LTLA <-  fingertips_data(IndicatorID = 92196,
                                               AreaTypeID = 'All',
                                               categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  # filter(Timeperiod == '2016/17') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Infant_mortality_rate') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()
# 
# BH_local_data %>%
#   filter(Indicator_short_name == 'Infant_mortality_rate') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(Infant_mortality_rate_LTLA$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Infant_mortality_rate <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Infant_mortality_rate_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Infant_mortality_rate_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Infant_mortality_rate_LTLA)

# SATOD
SATOD_LTLA <-  fingertips_data(IndicatorID = 93085,
                               AreaTypeID = 'All',
                               categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  # filter(Timeperiod == '2016/17') %>% 
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'SATOD') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'SATOD') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(SATOD_LTLA$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

SATOD <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(SATOD_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(SATOD_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(SATOD_LTLA)

# Premature_birth_rate
Premature_birth_rate_LTLA <-  fingertips_data(IndicatorID = 91743,
                               AreaTypeID = 401,
                               categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  # filter(Timeperiod == '2016/17') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Premature_birth_rate') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 0))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Premature_birth_rate') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(Premature_birth_rate_LTLA$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Premature_birth_rate <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Premature_birth_rate_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Premature_birth_rate_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Premature_birth_rate_LTLA)

# Low_birthweight
Low_birthweight_LTLA <-  fingertips_data(IndicatorID = 20101,
                               AreaTypeID = 401,
                               categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  # filter(Timeperiod == '2016/17') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Low_birthweight') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Low_birthweight') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(Low_birthweight_LTLA$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Low_birthweight <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Low_birthweight_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Low_birthweight_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Low_birthweight_LTLA)

# Starting well hex figure ####
hex_starting_well_df <- NEET %>% 
  bind_rows(Care_leavers_in_EET) %>% 
  bind_rows(Hospital_admissions_uninentional_deliberate_harm_under_five) %>% 
  bind_rows(Hospital_admissions_asthma_under_19) %>% 
  bind_rows(Hospital_admissions_self_harm_10_24) %>% 
  bind_rows(Breastfeeding_six_weeks) %>% 
  bind_rows(MMR_two_dose_5_years) %>% 
  bind_rows(Expected_communication_at_2) %>% 
  bind_rows(Obvious_tooth_decay_at_5) %>% 
  bind_rows(Healthy_weight_year_6) %>% 
  bind_rows(Infant_mortality_rate) %>% 
  bind_rows(SATOD) %>% 
  bind_rows(Premature_birth_rate) %>% 
  bind_rows(Low_birthweight) %>% 
  left_join(hex_indicators, by = 'Indicator_short_name') %>% 
  mutate(Significance_England = factor(ifelse(Polarity == 'Not applicable', 'Not applicable', ifelse(Significance_England == 'high' & Polarity == 'Lower is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Higher is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Lower is better', 'Better', ifelse(Significance_England == 'high' & Polarity == 'Higher is better', 'Better', ifelse(Significance_England %in% c('Not compared', 'Not applicable','not_applicable'), 'Not applicable', ifelse(Significance_England %in% c('similar', 'Similar', 'same'), 'Similar', Significance_England))))))), levels = c('Better', 'Similar', 'Worse', 'Not applicable'))) %>% 
  mutate(Indicator_label_2 = gsub('(.{1,27})(\\s|$)', '\\1\n', Indicator_label)) %>% # add line break every 25 characters (or at the end of the word if 25 falls in the middle)
  mutate(Indicator_label_2 = ifelse(str_count(Indicator_label_2, '\\n') == 1, paste0(Indicator_label_2, '\n\n\n'), ifelse(str_count(Indicator_label_2, '\\n') == 2, paste0(Indicator_label_2, '\n\n'),ifelse(str_count(Indicator_label_2, '\\n') == 3, paste0(Indicator_label_2, '\n'), Indicator_label_2)))) # I think we need to have four lines of text to make each hex icon text line up. otherwise the lines start at different places for shorter indicators.

eng_hex_starting_well_df <- hex_starting_well_df %>% 
  filter(Area_name == 'England') %>% 
  select(Indicator_short_name, Eng_value_label = Value_label)

hex_starting_well_df <- hex_starting_well_df %>% 
  left_join(eng_hex_starting_well_df, by = 'Indicator_short_name')

for(i in 1:length(ICT_areas)){
  
  ICT_x <- ICT_areas[i]
  Parent_area_x = ICT_areas_lookup %>% filter(Area == ICT_x) %>% select(UTLA) %>% as.character()
  
  hex_df_i <- hex_starting_well_df %>% 
    filter(Area_name == ICT_x) %>% 
    mutate(Ring_colour = factor(ifelse(Indicator_short_name %in% c('Hospital_admissions_self_harm_10_24') & Area_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden'), 'Higher_geography_only', ifelse(At_ICT_level %in% c('UTLA'), 'Higher_geography_only', ifelse(Area_name %in% c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West') & At_ICT_level == 'LTLA', 'Higher_geography_only', At_ICT_level))),
                                levels = c('Higher_geography_only', 'LTLA', 'ICT level')))
  
plot_x <- ggplot(hex_df_i,
                   aes(x = x,
                       y = y)) +
    geom_star(starshape = 6,
              size = 70,
              aes(fill = Significance_England,
                  colour = Ring_colour),
              starstroke  = 2) +
    scale_x_continuous(limits = c(min(hex_df_i$x)-.4,max(hex_df_i$x)+.4)) +
    scale_y_continuous(limits = c(min(hex_df_i$y)-.3, max(hex_df_i$y)+.3)) +
    scale_fill_manual(values = c(hex_colours_better, hex_colours_similar, hex_colours_worse, hex_colours_na),
                      drop = FALSE,
                      name = 'Compared to England') +
    scale_colour_manual(values = c('Higher_geography_only' = 'maroon', 'ICT level' = '#ffffff', 'LTLA' = '#ffffff'),
                        breaks = c('Higher_geography_only'),
                        label = paste0('Available at a higher geography (', Parent_area_x, ' local authority) only'),
                        name = '',
                        drop = FALSE) + 
    geom_text(aes(x - .375,
                  y + .1,
                  label = paste0(Value_label)),
              hjust = 0,
              vjust = 0,
              fontface = 'bold',
              size = 9) +
    labs(title = paste0(unique(hex_df_i$Figure_ID), ' - ', ICT_x, ifelse(str_detect(ICT_x, 'Brighton and Hove'), '', ' ICT')),
         caption = '* Note: due to data quality issues for these indicators in recent years, data are shown for an older time period\nthan is currently available for higher geographies (e.g. regionally and nationally)') +
    geom_text(aes(x - .375,
                  y - .15,
                  label = Indicator_label_2),
              hjust = 0,
              vjust = 0) +
    geom_text(aes(x - .375,
                  y - .15,
                  label = paste0('England: ', Eng_value_label, ', ', Time_period)),
              fontface = 'bold',
              hjust = 0,
              vjust = 0) +
    theme_hex() +
    guides(fill = guide_legend(override.aes = list(size = 11, colour = '#ffffff')),
           colour = guide_legend(override.aes = list(size = 10, drop.legend = FALSE))) +
    theme(legend.box="vertical",
          legend.margin=margin())
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(local_output_directory, '/Starting_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 14,
      height = 9.5)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(local_output_directory, '/Starting_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1250,
      height = 850,
      res = 90)
  print(plot_x)
  dev.off()
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(synced_sp_directory, '/Hex_figures/Starting_well/Starting_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 14,
      height = 10)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(synced_sp_directory, '/Hex_figures/Starting_well/Starting_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1250,
      height = 850,
      res = 90)
  print(plot_x)
  dev.off()
  
}
  
# Living well 1 ####

# Flu_65_plus
Flu_65_plus_raw <-  fingertips_data(IndicatorID = 30314,
                                         AreaTypeID = 'All',
                                         categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Flu_65_plus') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Flu_65_plus') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(Flu_65_plus_raw$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Flu_65_plus <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Flu_65_plus_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(Flu_65_plus_raw)

# Flu_at_risk
Flu_at_risk_raw <-  fingertips_data(IndicatorID = 30315,
                                    AreaTypeID = 'All',
                                    categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Flu_at_risk') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Flu_at_risk') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(Flu_at_risk_raw$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Flu_at_risk <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Flu_at_risk_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(Flu_at_risk_raw)

# PPV_immunisation
PPV_immunisation_raw <-  fingertips_data(IndicatorID = 30313,
                                            AreaTypeID = 'All',
                                            categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'PPV_immunisation') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'PPV_immunisation') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(PPV_immunisation_raw$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

PPV_immunisation <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(PPV_immunisation_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(PPV_immunisation_raw)

# COVID_19_spring_booster

area_codes <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumCasesByPublishDate&format=csv') %>% 
  select(Area_code = areaCode, Area_name = areaName) %>% 
  filter(Area_name %in% ICT_areas) %>% 
  unique() 

COVID_19_spring_booster_Eng <- read_csv(paste0('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=vaccinationsAgeDemographics&format=csv'),
                                      col_types = cols(cumVaccinationSpring23UptakeByVaccinationDatePercentage = col_number())) %>% 
  filter(age == '75+') %>% 
  select(Area_name = areaName, Time_period = date, Value = 'cumVaccinationSpring23UptakeByVaccinationDatePercentage') %>% 
  filter(Area_name == 'England') %>% 
  filter(Time_period == max(Time_period)) %>% 
  mutate(Time_period = paste0(ordinal(as.numeric(format(Time_period, '%d'))), format(Time_period, ' %b %Y')))

for(i in 1:nrow(area_codes)){

if(i == 1){COVID_19_spring_booster_LTLA = data.frame(Area_name = character(), Time_period = character(), Value = numeric())}

COVID_19_spring_booster_x <- read_csv(paste0('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&areaCode=', area_codes$Area_code[i], '&metric=vaccinationsAgeDemographics&format=csv'),
                                             col_types = cols(cumVaccinationSpring23UptakeByVaccinationDatePercentage = col_number())) %>% 
  filter(age == '75+') %>% 
  select(Area_name = areaName, Time_period = date, Value = 'cumVaccinationSpring23UptakeByVaccinationDatePercentage') %>% 
  filter(Time_period == max(Time_period)) %>% 
  mutate(Time_period = paste0(ordinal(as.numeric(format(Time_period, '%d'))), format(Time_period, ' %b %Y')))

COVID_19_spring_booster_LTLA <- COVID_19_spring_booster_LTLA %>% 
  bind_rows(COVID_19_spring_booster_x)
}

# BH_local_data %>%
#   filter(Indicator_short_name == 'COVID_19_spring_booster') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(COVID_19_spring_booster_LTLA$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

COVID_19_spring_booster <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(COVID_19_spring_booster_LTLA, Area_name == 'Brighton and Hove')[c('Value', 'Time_period')]) %>% 
  bind_rows(COVID_19_spring_booster_LTLA) %>% 
  bind_rows(COVID_19_spring_booster_Eng) %>% 
  mutate(Significance_England = 'Not applicable',
         Value_label = paste0(Value, '%'),
         Indicator_short_name = 'COVID_19_spring_booster') %>% 
  mutate(Time_period = paste0('\n as at ', Time_period)) %>% 
  select(!Value)

rm(COVID_19_spring_booster_Eng, COVID_19_spring_booster_LTLA, COVID_19_spring_booster_x)

# HPV_coverage_13_14_female_2_doses
HPV_coverage_13_14_female_2_doses_raw <-  fingertips_data(IndicatorID = 92896,
                                                          AreaTypeID = 'All',
                                                          categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'HPV_coverage_13_14_female_2_doses') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'HPV_coverage_13_14_female_2_doses') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(HPV_coverage_13_14_female_2_doses_raw$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

HPV_coverage_13_14_female_2_doses <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(HPV_coverage_13_14_female_2_doses_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(HPV_coverage_13_14_female_2_doses_raw)

# HPV_coverage_13_14_male_2_doses
HPV_coverage_13_14_male_2_doses_raw <-  fingertips_data(IndicatorID = 92896,
                                                          AreaTypeID = 'All',
                                                          categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Male') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'HPV_coverage_13_14_male_2_doses') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'HPV_coverage_13_14_male_2_doses') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(HPV_coverage_13_14_male_2_doses_raw$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

HPV_coverage_13_14_male_2_doses <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(HPV_coverage_13_14_male_2_doses_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(HPV_coverage_13_14_male_2_doses_raw)

# Adult_smoking_prevalence
Adult_smoking_prevalence_LTLA <-  fingertips_data(IndicatorID = 92443,
                               AreaTypeID = 'All',
                               categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == '18+ yrs') %>% 
  # filter(Timeperiod == '2016/17') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Adult_smoking_prevalence') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Adult_smoking_prevalence') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(Adult_smoking_prevalence_LTLA$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Adult_smoking_prevalence <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Adult_smoking_prevalence_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Adult_smoking_prevalence_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Adult_smoking_prevalence_LTLA)

# QOF_hypertension_prevalence
QOF_hypertension_prevalence_LTLA <-  fingertips_data(IndicatorID = 219,
                                                  AreaTypeID = 'All',
                                                  categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  # filter(TimeperiodSortable == max(TimeperiodSortable)) #%>%
  filter(Timeperiod == '2021/22') %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'QOF_hypertension_prevalence') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%*')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'QOF_hypertension_prevalence') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(QOF_hypertension_prevalence_LTLA$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

QOF_hypertension_prevalence <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(QOF_hypertension_prevalence_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(QOF_hypertension_prevalence_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(QOF_hypertension_prevalence_LTLA)

# QOF_COPD_prevalence
QOF_COPD_prevalence_LTLA <-  fingertips_data(IndicatorID = 253,
                                                     AreaTypeID = 'All',
                                                     categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  # filter(TimeperiodSortable == max(TimeperiodSortable)) #%>%
  filter(Timeperiod == '2021/22') %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'QOF_COPD_prevalence') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%*')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'QOF_COPD_prevalence') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(QOF_COPD_prevalence_LTLA$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

QOF_COPD_prevalence <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(QOF_COPD_prevalence_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(QOF_COPD_prevalence_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(QOF_COPD_prevalence_LTLA)

# QOF_depression_prevalence
QOF_depression_prevalence_LTLA <-  fingertips_data(IndicatorID = 848,
                                             AreaTypeID = 'All',
                                             categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  # filter(TimeperiodSortable == max(TimeperiodSortable)) #%>%
  filter(Timeperiod == '2021/22') %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'QOF_depression_prevalence') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%*')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'QOF_depression_prevalence') %>% 
#   mutate(Value_label = paste0(round(Value_label, 1))) %>% 
#   mutate(Time_period = unique(QOF_depression_prevalence_LTLA$Time_period)) %>% 
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

QOF_depression_prevalence <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(QOF_depression_prevalence_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(QOF_depression_prevalence_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(QOF_depression_prevalence_LTLA)

# Low_happiness_score
if(file.exists(paste0(local_data_directory, '/ONS_personal_wellbeing.xlsx')) == FALSE){
download.file('https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/wellbeing/datasets/headlineestimatesofpersonalwellbeing/april2021tomarch2022/annualpersonalwellbeingestimatesapril2021tomarch20221.xlsx',
              paste0(local_data_directory, '/ONS_personal_wellbeing.xlsx'),
              mode = 'wb')
}

Low_happiness_score_LTLA <- read_excel("//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Data/ONS_personal_wellbeing.xlsx", 
                                     sheet = "8 Happiness thresholds",
                                     skip = 13) %>% 
  select(Area_name = 'Area Names', Value = "April 2021 to March 2022\r\nLow\r\n(score 0 to 4)\r\n%") %>% 
  mutate(Time_period = '2021/22') %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Indicator_short_name = 'Low_happiness_score') %>% 
  mutate(Value_label = ifelse(Value == '[c]', '-', paste0(round(as.numeric(Value),1), '%'))) %>% 
  mutate(Area_name = ifelse(Area_name == 'ENGLAND', 'England', Area_name)) %>% 
  filter(Area_name %in% c(ICT_areas, 'England'))

# BH_local_data %>%
#   filter(Indicator_short_name == 'Low_happiness_score') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Low_happiness_score_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Low_happiness_score <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Low_happiness_score_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Low_happiness_score_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Low_happiness_score_LTLA)

High_anxiety_score_LTLA <- read_excel("//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Data/ONS_personal_wellbeing.xlsx", 
                                     sheet = "11 Anxiety thresholds",
                                     skip = 13) %>% 
  select(Area_name = 'Area Names', Value = "April 2021 to March 2022\r\nHigh\r\n(score 6 to 10)\r\n%") %>% 
  mutate(Time_period = '2021/22') %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Indicator_short_name = 'High_anxiety_score') %>% 
  mutate(Value_label = ifelse(Value == '[c]', '-', paste0(round(as.numeric(Value),1), '%'))) %>% 
  mutate(Area_name = ifelse(Area_name == 'ENGLAND', 'England', Area_name)) %>% 
  filter(Area_name %in% c(ICT_areas, 'England'))

# BH_local_data %>%
#   filter(Indicator_short_name == 'High_anxiety_score') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(High_anxiety_score_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

High_anxiety_score <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(High_anxiety_score_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(High_anxiety_score_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(High_anxiety_score_LTLA)

# Obesity_18_plus
Obesity_18_plus_LTLA <-  fingertips_data(IndicatorID = 93881,
                                             AreaTypeID = 'All',
                                             categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == '18+ yrs') %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Obesity_18_plus') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Obesity_18_plus') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Obesity_18_plus_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Obesity_18_plus <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Obesity_18_plus_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Obesity_18_plus_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Obesity_18_plus_LTLA)

# Physically_active_adults
Physically_active_adults_LTLA <-  fingertips_data(IndicatorID = 93014,
                                         AreaTypeID = 'All',
                                         categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == '19+ yrs') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Physically_active_adults') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Physically_active_adults') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Physically_active_adults_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)  

Physically_active_adults <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Physically_active_adults_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Physically_active_adults_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Physically_active_adults_LTLA)

# Physically_inactive_adults
Physically_inactive_adults_LTLA <-  fingertips_data(IndicatorID = 93015,
                                                  AreaTypeID = 'All',
                                                  categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == '19+ yrs') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Physically_inactive_adults') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Physically_inactive_adults') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Physically_inactive_adults_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)  

Physically_inactive_adults <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Physically_inactive_adults_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Physically_inactive_adults_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Physically_inactive_adults_LTLA)

# Walking_for_travel_adults
Walking_for_travel_adults_LTLA <- fingertips_data(IndicatorID = 93439,
                                                    AreaTypeID = 'All',
                                                    categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == '16+ yrs') %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Walking_for_travel_adults') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Walking_for_travel_adults') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Walking_for_travel_adults_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)  

Walking_for_travel_adults <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Walking_for_travel_adults_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Walking_for_travel_adults_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Walking_for_travel_adults_LTLA)

# Cycling_for_travel_adults
Cycling_for_travel_adults_LTLA <- fingertips_data(IndicatorID = 93440,
                                                  AreaTypeID = 'All',
                                                  categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == '16+ yrs') %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Cycling_for_travel_adults') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Cycling_for_travel_adults') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Cycling_for_travel_adults_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)  

Cycling_for_travel_adults <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Cycling_for_travel_adults_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Cycling_for_travel_adults_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Cycling_for_travel_adults_LTLA)

# Health_index_score

if(file.exists(paste0(local_data_directory, '/ONS_Health_Index.xlsx')) == FALSE){
  download.file('https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/datasets/healthindexscoresengland/current/healthindexscoresengland.xlsx',
                paste0(local_data_directory, '/ONS_Health_Index.xlsx'),
                mode = 'wb')
}

Health_index_score_LTLA <- read_excel("//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Data/ONS_Health_Index.xlsx", 
                               sheet = "Table_2_Index_scores", skip = 2) %>% 
  select(Area_name = 'Area Name', Value_label = '2021') %>% 
  mutate(Area_name = ifelse(Area_name == 'ENGLAND', 'England', Area_name)) %>% 
  mutate(Time_period = '2021') %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  filter(Area_name %in% c(ICT_areas, 'England')) %>% 
  mutate(Value_label = paste0(round(Value_label, 1))) %>% 
  mutate(Indicator_short_name = 'Health_index_score')

# BH_local_data %>%
#   filter(Indicator_short_name == 'Health_index_score') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Health_index_score_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)  
  
Health_index_score <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Health_index_score_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Health_index_score_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Health_index_score_LTLA)

# Air_pollution_attributable_mortality
Air_pollution_attributable_mortality_LTLA <- fingertips_data(IndicatorID = 93861,
                                                  AreaTypeID = 'All',
                                                  categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Air_pollution_attributable_mortality') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Air_pollution_attributable_mortality') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Air_pollution_attributable_mortality_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)  

Air_pollution_attributable_mortality <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Air_pollution_attributable_mortality_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Air_pollution_attributable_mortality_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Air_pollution_attributable_mortality_LTLA)

# Alcohol_specific_admissions
Alcohol_specific_admissions_LTLA <- fingertips_data(IndicatorID = 92906,
                                                             AreaTypeID = 401,
                                                             categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Alcohol_specific_admissions') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Alcohol_specific_admissions') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Alcohol_specific_admissions_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

Alcohol_specific_admissions <- BH_local_data %>%
  filter(Indicator_short_name == 'Alcohol_specific_admissions') %>%
  mutate(Value_label = paste0(round(Value_label, 1))) %>%
  mutate(Time_period = unique(Alcohol_specific_admissions_LTLA$Time_period)) %>%
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Alcohol_specific_admissions_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Alcohol_specific_admissions_LTLA)

# Alcohol_consumption_over_14_units
Alcohol_consumption_over_14_units_raw <-  fingertips_data(IndicatorID = 92778,
                                         AreaTypeID = 'All',
                                         categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Alcohol_consumption_over_14_units') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Alcohol_consumption_over_14_units') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Alcohol_consumption_over_14_units_raw$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

Alcohol_consumption_over_14_units <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Alcohol_consumption_over_14_units_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(Alcohol_consumption_over_14_units_raw)

# Living well 1 hex figure ####
hex_living_well_1_df <- Flu_65_plus %>% 
  bind_rows(Flu_at_risk) %>% 
  bind_rows(PPV_immunisation) %>% 
  bind_rows(COVID_19_spring_booster) %>%
  bind_rows(HPV_coverage_13_14_female_2_doses) %>% 
  bind_rows(HPV_coverage_13_14_male_2_doses) %>% 
  bind_rows(Adult_smoking_prevalence) %>% 
  bind_rows(QOF_hypertension_prevalence) %>% 
  bind_rows(QOF_COPD_prevalence) %>% 
  bind_rows(QOF_depression_prevalence) %>% 
  bind_rows(Low_happiness_score) %>% 
  bind_rows(High_anxiety_score) %>% 
  bind_rows(Obesity_18_plus) %>% 
  bind_rows(Physically_active_adults) %>% 
  bind_rows(Physically_inactive_adults) %>% 
  bind_rows(Walking_for_travel_adults) %>% 
  bind_rows(Cycling_for_travel_adults) %>% 
  bind_rows(Health_index_score) %>% 
  bind_rows(Air_pollution_attributable_mortality) %>% 
  bind_rows(Alcohol_specific_admissions) %>% 
  bind_rows(Alcohol_consumption_over_14_units) %>% 
  left_join(hex_indicators, by = 'Indicator_short_name') %>% 
  mutate(Significance_England = factor(ifelse(Polarity == 'Not applicable', 'Not applicable', ifelse(Significance_England == 'high' & Polarity == 'Lower is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Higher is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Lower is better', 'Better', ifelse(Significance_England == 'high' & Polarity == 'Higher is better', 'Better', ifelse(Significance_England %in% c('Not compared', 'Not applicable','not_applicable'), 'Not applicable', ifelse(Significance_England %in% c('similar', 'Similar', 'same'), 'Similar', Significance_England))))))), levels = c('Better', 'Similar', 'Worse', 'Not applicable'))) %>% 
  mutate(Indicator_label_2 = gsub('(.{1,27})(\\s|$)', '\\1\n', Indicator_label)) %>% # add line break every 25 characters (or at the end of the word if 25 falls in the middle)
  mutate(Indicator_label_2 = ifelse(str_count(Indicator_label_2, '\\n') == 1, paste0(Indicator_label_2, '\n\n\n'), ifelse(str_count(Indicator_label_2, '\\n') == 2, paste0(Indicator_label_2, '\n\n'),ifelse(str_count(Indicator_label_2, '\\n') == 3, paste0(Indicator_label_2, '\n'), Indicator_label_2)))) # I think we need to have four lines of text to make each hex icon text line up. otherwise the lines start at different places for shorter indicators.

eng_hex_living_well_1_df <- hex_living_well_1_df %>% 
  filter(Area_name == 'England') %>% 
  select(Indicator_short_name, Eng_value_label = Value_label)

hex_living_well_1_df <- hex_living_well_1_df %>% 
  left_join(eng_hex_living_well_1_df, by = 'Indicator_short_name')

for(i in 1:length(ICT_areas)){
  
ICT_x <- ICT_areas[i]
Parent_area_x = ICT_areas_lookup %>% filter(Area == ICT_x) %>% select(UTLA) %>% as.character()
  
hex_df_i <- hex_living_well_1_df %>% 
    filter(Area_name == ICT_x) %>% 
    mutate(Ring_colour = factor(ifelse(At_ICT_level %in% c('UTLA'), 'Higher_geography_only', ifelse(Area_name %in% c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West') & At_ICT_level == 'LTLA', 'Higher_geography_only', At_ICT_level)),
                                levels = c('Higher_geography_only', 'LTLA', 'ICT level')))
plot_x <- ggplot(hex_df_i,
                   aes(x = x,
                       y = y)) +
    geom_star(starshape = 6,
              size = 70,
              aes(fill = Significance_England,
                  colour = Ring_colour),
              starstroke  = 2) +
    scale_x_continuous(limits = c(min(hex_df_i$x)-.4,max(hex_df_i$x)+.4)) +
    scale_y_continuous(limits = c(min(hex_df_i$y)-.3, max(hex_df_i$y)+.3)) +
    scale_fill_manual(values = c(hex_colours_better, hex_colours_similar, hex_colours_worse, hex_colours_na),
                      drop = FALSE,
                      name = 'Compared to England') +
    scale_colour_manual(values = c('Higher_geography_only' = 'maroon', 'ICT level' = '#ffffff', 'LTLA' = '#ffffff'),
                        breaks = c('Higher_geography_only'),
                        label = paste0('Available at a higher geography (', Parent_area_x, ' local authority) only'),
                        name = '',
                        drop = FALSE) + 
    geom_text(aes(x - .4,
                  y + .1,
                  label = paste0(Value_label)),
              hjust = 0,
              vjust = 0,
              fontface = 'bold',
              size = 9) +
    labs(title = paste0(unique(hex_df_i$Figure_ID), ' - ', ICT_x, ifelse(str_detect(ICT_x, 'Brighton and Hove'), '', ' ICT')),
         caption = '* Note: due to data quality and availability for these indicators in recent years, data are shown for an older time period\nthan is currently available for higher geographies (e.g. regionally and nationally)') +
    geom_text(aes(x - .4,
                  y - .18,
                  label = Indicator_label_2),
              hjust = 0,
              vjust = 0) +
    geom_text(aes(x - .4,
                  y - .15,
                  label = paste0('England: ', Eng_value_label, ', ', Time_period)),
              fontface = 'bold',
              hjust = 0,
              vjust = 0) +
    theme_hex() +
    guides(fill = guide_legend(override.aes = list(size = 11, colour = '#ffffff')),
           colour = guide_legend(override.aes = list(size = 10, drop.legend = FALSE))) +
    theme(legend.box="vertical",
          legend.margin=margin())
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(local_output_directory, '/Living_well_1_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 16.5,
      height = 11)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(local_output_directory, '/Living_well_1_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1500,
      height = 1050,
      res = 90)
  print(plot_x)
  dev.off()
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(synced_sp_directory, '/Hex_figures/Living_well_1/Living_well_1_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 16.5,
      height = 11)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(synced_sp_directory, '/Hex_figures/Living_well_1/Living_well_1_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1500,
      height = 1050,
      res = 90)
  print(plot_x)
  dev.off()
  
}

# Living well 2 ####

# Under_75_CVD_mortality
Under_75_CVD_mortality_LTLA <-  fingertips_data(IndicatorID = 40401,
                                                  AreaTypeID = 401,
                                                  categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Under_75_CVD_mortality') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Under_75_CVD_mortality') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Under_75_CVD_mortality_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

Under_75_CVD_mortality <- BH_local_data %>%
  filter(Indicator_short_name == 'Under_75_CVD_mortality') %>%
  mutate(Value_label = paste0(round(Value_label, 1))) %>%
  mutate(Time_period = unique(Under_75_CVD_mortality_LTLA$Time_period)) %>%
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Under_75_CVD_mortality_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Under_75_CVD_mortality_LTLA)

# Under_75_respiratory_mortality
Under_75_respiratory_mortality_LTLA <-  fingertips_data(IndicatorID = 40701,
                                                AreaTypeID = 401,
                                                categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Under_75_respiratory_mortality') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Under_75_respiratory_mortality') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Under_75_respiratory_mortality_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) 

Under_75_respiratory_mortality <- BH_local_data %>%
  filter(Indicator_short_name == 'Under_75_respiratory_mortality') %>%
  mutate(Value_label = paste0(round(Value_label, 1))) %>%
  mutate(Time_period = unique(Under_75_respiratory_mortality_LTLA$Time_period)) %>%
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Under_75_respiratory_mortality_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Under_75_respiratory_mortality_LTLA)

# Under_75_cancer_mortality
Under_75_cancer_mortality_LTLA <-  fingertips_data(IndicatorID = 40501,
                                                        AreaTypeID = 401,
                                                        categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Under_75_cancer_mortality') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Under_75_cancer_mortality') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Under_75_cancer_mortality_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Under_75_cancer_mortality <- BH_local_data %>%
  filter(Indicator_short_name == 'Under_75_cancer_mortality') %>%
  mutate(Value_label = paste0(round(Value_label, 1))) %>%
  mutate(Time_period = unique(Under_75_cancer_mortality_LTLA$Time_period)) %>%
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Under_75_cancer_mortality_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Under_75_cancer_mortality_LTLA)

# Under_75_excess_deaths_SMI
Under_75_excess_deaths_SMI_raw <-  fingertips_data(IndicatorID = 93582,
                                                          AreaTypeID = 'All',
                                                          categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Age == '18-74 yrs') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Under_75_excess_deaths_SMI') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Under_75_excess_deaths_SMI') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Under_75_excess_deaths_SMI_raw$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Under_75_excess_deaths_SMI <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Under_75_excess_deaths_SMI_raw, by = c('UTLA' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(Under_75_excess_deaths_SMI_raw)

# Drug_related_deaths
Drug_related_deaths_LTLA <-  fingertips_data(IndicatorID = 92432,
                                                   AreaTypeID = 401,
                                                   categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Drug_related_deaths') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = ifelse(is.na(Value), '-', paste0(round(Value, 1)))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Drug_related_deaths') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Drug_related_deaths_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Drug_related_deaths <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Drug_related_deaths_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Drug_related_deaths_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Drug_related_deaths_LTLA)

# Suicide_undetermined_injury_mortality
Suicide_undetermined_injury_mortality_LTLA <-  fingertips_data(IndicatorID = 41001,
                                             AreaTypeID = 'All',
                                             categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Suicide_undetermined_injury_mortality') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Suicide_undetermined_injury_mortality') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Suicide_undetermined_injury_mortality_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Suicide_undetermined_injury_mortality <- BH_local_data %>%
  filter(Indicator_short_name == 'Suicide_undetermined_injury_mortality') %>%
  mutate(Value_label = paste0(round(Value_label, 1))) %>%
  mutate(Time_period = unique(Suicide_undetermined_injury_mortality_LTLA$Time_period)) %>%
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Suicide_undetermined_injury_mortality_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Suicide_undetermined_injury_mortality_LTLA)

# Cancer_screening_breast
Cancer_screening_breast_LTLA <-  fingertips_data(IndicatorID = 22001,
                                             AreaTypeID = 'All',
                                             categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Cancer_screening_breast') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Cancer_screening_breast') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Cancer_screening_breast_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Cancer_screening_breast <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Cancer_screening_breast_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Cancer_screening_breast_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Cancer_screening_breast_LTLA)

# Cancer_screening_bowel
Cancer_screening_bowel_LTLA <-  fingertips_data(IndicatorID = 91720,
                                                 AreaTypeID = 'All',
                                                 categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Cancer_screening_bowel') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Cancer_screening_bowel') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Cancer_screening_bowel_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Cancer_screening_bowel <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Cancer_screening_bowel_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Cancer_screening_bowel_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Cancer_screening_bowel_LTLA)

# Lung check 

# TWW_new_cancer_diagnoses
TWW_new_cancer_diagnoses_Sub_ICB <-  fingertips_data(IndicatorID = 91347,
                                                AreaTypeID = 'All',
                                                categorytype = FALSE) %>% 
  filter(AreaType %in% c('ICB sub-locations', 'England')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England', 'Sussex ICB - 09D ', 'Sussex ICB - 70F ', 'Sussex ICB - 97R ')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'TWW_new_cancer_diagnoses') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'TWW_new_cancer_diagnoses') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(TWW_new_cancer_diagnoses_Sub_ICB$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

TWW_new_cancer_diagnoses <- ICT_areas_lookup_2 %>% 
    bind_rows(data.frame(Area = 'England', Sub_ICB = 'England')) %>% 
    left_join(TWW_new_cancer_diagnoses_Sub_ICB, by = c('Sub_ICB' = 'Area_name')) %>% 
    rename(Area_name = Area) %>% 
    select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)
  
rm(TWW_new_cancer_diagnoses_Sub_ICB)

# Cancers_diagnosed_at_stage_1_and_2
Cancers_diagnosed_at_stage_1_and_2_LTLA <-  fingertips_data(IndicatorID = 93671,
                                                AreaTypeID = 'All',
                                                categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Cancers_diagnosed_at_stage_1_and_2') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Cancers_diagnosed_at_stage_1_and_2') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Cancers_diagnosed_at_stage_1_and_2_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Cancers_diagnosed_at_stage_1_and_2 <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Cancers_diagnosed_at_stage_1_and_2_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Cancers_diagnosed_at_stage_1_and_2_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Cancers_diagnosed_at_stage_1_and_2_LTLA)

# Cancer_screening_cervical_cancer_2549
Cancer_screening_cervical_cancer_2549_LTLA <-  fingertips_data(IndicatorID = 93560,
                                                            AreaTypeID = 'All',
                                                            categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Cancer_screening_cervical_cancer_2549') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Cancer_screening_cervical_cancer_2549') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Cancer_screening_cervical_cancer_2549_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Cancer_screening_cervical_cancer_2549 <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Cancer_screening_cervical_cancer_2549_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Cancer_screening_cervical_cancer_2549_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Cancer_screening_cervical_cancer_2549_LTLA)

# NHS_Health_Check_uptake
NHS_Health_Check_uptake_raw <-  fingertips_data(IndicatorID = 91734,
                                                   AreaTypeID = 'All',
                                                   categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'NHS_Health_Check_uptake') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'NHS_Health_Check_uptake') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(NHS_Health_Check_uptake_raw$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

NHS_Health_Check_uptake <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(NHS_Health_Check_uptake_raw, by = c('UTLA' = 'Area_name')) %>% 
  # mutate(Value_label = ifelse(UTLA != 'England', paste0(Value_label, ' (', UTLA, ')'), Value_label))  %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) 

rm(NHS_Health_Check_uptake_raw)

# Health_Check_uptake_LD

# Number of patients recorded on their general practice’s QOF learning disabilities register aged 14 years or over who had a learning disability health check under the learning disabilities Enhanced Service in the 12 months up to and including the end of the reporting period.

# if(file.exists(paste0(local_data_directory, '/Health and Care of People with Learning Disabilities SICBL data 2021-22.csv')) == FALSE){
# 
# download.file('https://files.digital.nhs.uk/BC/016738/health_care_ld_sicbl_data_1718_2122.zip',
#               paste0(local_data_directory,  '/LD_health_checks.zip'),
#               mode = 'wb')
# 
# unzip(paste0(local_data_directory,  '/LD_health_checks.zip'),
#       exdir = local_data_directory)
# }
# 
# Health_Check_uptake_LD <- read_csv(paste0(local_data_directory, '/Health and Care of People with Learning Disabilities SICBL data 2021-22.csv'))
# 
# # LDOB079 

# Health_Check_uptake_LD <- read_csv('https://files.digital.nhs.uk/8C/554281/learning-disabilities-health-check-scheme-eng-Aug-2023.csv')
# 
# Health_Check_uptake_LD %>% 
#   filter(IND_CODE == 'LDHC021') %>% 
#   filter(SUB_ICB_LOC_CODE %in% c('09D', '70F', '97R')) %>% 
#   View()

# I cannot collate this with no denominators available - cant sum percentages at GP practice level.

if(file.exists(paste0(local_data_directory, '/LD_health_checks_sept_2020.xlsx')) == FALSE){

download.file('https://www.england.nhs.uk/wp-content/uploads/2020/12/learning-disability-annual-health-checks-sept-2020-p2.xlsx',
              paste0(local_data_directory,  '/LD_health_checks_sept_2020.xlsx'),
              mode = 'wb')
}

Health_Check_uptake_LD_CCG <- read_excel("//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Data/LD_health_checks_sept_2020.xlsx", 
           sheet = "AHCs", 
           skip = 6) %>% 
  select(Area_name = '...1', Value = '% of those on GP Learning Disability Register who had an AHC in year2...3') %>% 
  filter(Area_name %in% c('NHS BRIGHTON AND HOVE CCG', 'NHS EAST SUSSEX CCG', 'NHS WEST SUSSEX CCG', 'England')) %>% 
  mutate(Area_name = ifelse(Area_name == 'NHS BRIGHTON AND HOVE CCG', 'NHS Brighton And Hove CCG', ifelse(Area_name == 'NHS EAST SUSSEX CCG', 'NHS East Sussex CCG', ifelse(Area_name == 'NHS WEST SUSSEX CCG', 'NHS West Sussex CCG', Area_name)))) %>% 
  mutate(Value_label = paste0(round(Value * 100,1), '%')) %>% 
  mutate(Time_period = '2020',
         Indicator_short_name = 'Health_Check_uptake_LD',
         Significance_England = 'Not applicable')

# BH_local_data %>%
#   filter(Indicator_short_name == 'Health_Check_uptake_LD') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Health_Check_uptake_LD_CCG$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Health_Check_uptake_LD <- ICT_areas_lookup_3 %>% 
  bind_rows(data.frame(Area = 'England', CCG = 'England')) %>% 
  left_join(Health_Check_uptake_LD_CCG, by = c('CCG' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(Health_Check_uptake_LD_CCG)

# Health_Check_uptake_SMI
if(file.exists(paste0(local_data_directory, '/Health_Check_uptake_SMI_Q4_2223.xlsx')) == FALSE){
  download.file('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/05/Physical-Health-Checks-SMI-Q4-2022-23-1.xlsx',
                paste0(local_data_directory, '/Health_Check_uptake_SMI_Q4_2223.xlsx'),
                mode = 'wb')
}

Health_Check_uptake_SMI_Eng <- read_excel("//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Data/Health_Check_uptake_SMI_Q4_2223.xlsx", 
                                              sheet = "Eng and Region - health checks",
                                              skip = 14) %>% 
  select(Area_name = 'Region Name', Numerator = 'Numerator (total number to get full check)', Denominator = 'SMI register') %>% 
  filter(Area_name == 'England') %>% 
  mutate(Numerator = as.numeric(Numerator)) %>% 
  mutate(Value_label = paste0(round((Numerator / Denominator * 100), 1), '%')) %>% 
  mutate(England_Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         England_Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  mutate(Significance_England = 'Not applicable')

Health_Check_uptake_SMI_Sub_ICB <- read_excel("//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Data/Health_Check_uptake_SMI_Q4_2223.xlsx", 
           sheet = "Sub ICB - health checks",
           skip = 14) %>% 
  select(Area_name = 'Sub ICB location Name', Numerator = 'Numerator (total number to get full check)', Denominator = 'SMI register') %>% 
  filter(str_detect(Area_name, 'SUSSEX|BRIGHTON')) %>% 
  mutate(Area_name = ifelse(Area_name == 'NHS BRIGHTON AND HOVE (SUB ICB LOCATION)', 'Sussex ICB - 09D ', ifelse(Area_name == 'NHS EAST SUSSEX (SUB ICB LOCATION)', 'Sussex ICB - 97R ', ifelse(Area_name == 'NHS WEST SUSSEX (SUB ICB LOCATION)', 'Sussex ICB - 70F ', NA)))) %>% 
  mutate(Value_label = paste0(round((Numerator / Denominator * 100), 1), '%')) %>% 
  mutate(Lower_CI = PHEindicatormethods:::wilson_lower(Numerator, Denominator, confidence = .95) * 100,
         Upper_CI = PHEindicatormethods:::wilson_upper(Numerator, Denominator, confidence = .95) * 100) %>% 
  bind_cols(Health_Check_uptake_SMI_Eng[c('England_Lower_CI', 'England_Upper_CI')]) %>% 
  mutate(Significance_England = ifelse(Area_name == 'England', 'not_applicable', ifelse(Lower_CI > England_Upper_CI, 'high', ifelse(Upper_CI < England_Lower_CI, 'low', 'similar')))) %>% 
  bind_rows(Health_Check_uptake_SMI_Eng) %>% 
  mutate(Time_period = '2022/23',
         Indicator_short_name = 'Health_Check_uptake_SMI') %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

# BH_local_data %>%
#   filter(Indicator_short_name == 'Health_Check_uptake_SMI') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Health_Check_uptake_SMI_Sub_ICB$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Health_Check_uptake_SMI <- ICT_areas_lookup_2 %>% 
  bind_rows(data.frame(Area = 'England', Sub_ICB = 'England')) %>% 
  left_join(Health_Check_uptake_SMI_Sub_ICB, by = c('Sub_ICB' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) 

# Emergency_admissions_self_harm
Emergency_admissions_self_harm_LTLA <-  fingertips_data(IndicatorID = 21001,
                                                               AreaTypeID = 'All',
                                                               categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Emergency_admissions_self_harm') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Emergency_admissions_self_harm') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Emergency_admissions_self_harm_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Emergency_admissions_self_harm <- BH_local_data %>%
  filter(Indicator_short_name == 'Emergency_admissions_self_harm') %>%
  mutate(Value_label = paste0(round(Value_label, 1))) %>%
  mutate(Time_period = unique(Emergency_admissions_self_harm_LTLA$Time_period)) %>%
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Emergency_admissions_self_harm_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Emergency_admissions_self_harm_LTLA)

# Emergency_admissions_COPD
Emergency_admissions_COPD_LTLA <-  fingertips_data(IndicatorID = 92302,
                                                        AreaTypeID = 'All',
                                                        categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Female') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Emergency_admissions_COPD') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Emergency_admissions_COPD') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Emergency_admissions_COPD_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Emergency_admissions_COPD <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Emergency_admissions_COPD_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Emergency_admissions_COPD_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Emergency_admissions_COPD_LTLA)

# Emergency_admissions_pneumonia
Emergency_admissions_pneumonia_CCG <-  fingertips_data(IndicatorID = 93574,
                                                           AreaTypeID = 'All',
                                                           categorytype = FALSE) %>% 
  filter(AreaType %in% c('CCGs (from Apr 2021)', 'England')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England', 'NHS Brighton And Hove CCG', 'NHS East Sussex CCG', 'NHS West Sussex CCG')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Emergency_admissions_pneumonia') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Emergency_admissions_pneumonia') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Emergency_admissions_pneumonia_CCG$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Emergency_admissions_pneumonia <- ICT_areas_lookup_3 %>% 
  bind_rows(data.frame(Area = 'England', CCG = 'England')) %>% 
  left_join(Emergency_admissions_pneumonia_CCG, by = c('CCG' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) %>% 
  mutate(Significance_England = gsub(' 95%', '', gsub(' 99.8%', '', Significance_England)))
  
rm(Emergency_admissions_pneumonia_CCG)

# Emergency_admissions_asthma
Emergency_admissions_asthma_CCG <-  fingertips_data(IndicatorID = 93573,
                                                       AreaTypeID = 'All',
                                                       categorytype = FALSE) %>% 
  filter(AreaType %in% c('CCGs (from Apr 2021)', 'England')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England', 'NHS Brighton And Hove CCG', 'NHS East Sussex CCG', 'NHS West Sussex CCG')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Emergency_admissions_asthma') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Emergency_admissions_asthma') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Emergency_admissions_asthma_CCG$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Emergency_admissions_asthma <- ICT_areas_lookup_3 %>% 
  bind_rows(data.frame(Area = 'England', CCG = 'England')) %>% 
  left_join(Emergency_admissions_asthma_CCG, by = c('CCG' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) %>% 
  mutate(Significance_England = gsub(' 95%', '', gsub(' 99.8%', '', Significance_England)))

rm(Emergency_admissions_asthma_CCG)

# Living well 2 hex figure ####
hex_living_well_2_df <- Under_75_CVD_mortality %>% 
  bind_rows(Under_75_respiratory_mortality) %>% 
  bind_rows(Under_75_cancer_mortality) %>% 
  bind_rows(Under_75_excess_deaths_SMI) %>% 
  bind_rows(Drug_related_deaths) %>% 
  bind_rows(Suicide_undetermined_injury_mortality) %>% 
  bind_rows(Cancer_screening_breast) %>% 
  bind_rows(Cancer_screening_bowel) %>% 
  # bind_rows(Lung_check) %>% 
  bind_rows(TWW_new_cancer_diagnoses) %>% 
  bind_rows(Cancers_diagnosed_at_stage_1_and_2) %>% 
  bind_rows(Cancer_screening_cervical_cancer_2549) %>% 
  bind_rows(NHS_Health_Check_uptake) %>% 
  bind_rows(Health_Check_uptake_LD) %>% 
  bind_rows(Health_Check_uptake_SMI) %>% 
  bind_rows(Emergency_admissions_self_harm) %>% 
  bind_rows(Emergency_admissions_COPD) %>% 
  bind_rows(Emergency_admissions_pneumonia) %>% 
  bind_rows(Emergency_admissions_asthma) %>% 
  left_join(hex_indicators, by = 'Indicator_short_name') %>% 
  mutate(Significance_England = factor(ifelse(Polarity == 'Not applicable', 'Not applicable', ifelse(Significance_England %in% c('high', 'Higher') & Polarity == 'Lower is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Higher is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Lower is better', 'Better', ifelse(Significance_England %in% c('high', 'Higher') & Polarity == 'Higher is better', 'Better', ifelse(Significance_England %in% c('Not compared', 'Not applicable','not_applicable'), 'Not applicable', ifelse(Significance_England %in% c('similar', 'Similar', 'same'), 'Similar', Significance_England))))))), levels = c('Better', 'Similar', 'Worse', 'Not applicable'))) %>% 
  mutate(Indicator_label_2 = gsub('(.{1,27})(\\s|$)', '\\1\n', Indicator_label)) %>% # add line break every 25 characters (or at the end of the word if 25 falls in the middle)
  mutate(Indicator_label_2 = ifelse(str_count(Indicator_label_2, '\\n') == 1, paste0(Indicator_label_2, '\n\n\n'), ifelse(str_count(Indicator_label_2, '\\n') == 2, paste0(Indicator_label_2, '\n\n'),ifelse(str_count(Indicator_label_2, '\\n') == 3, paste0(Indicator_label_2, '\n'), Indicator_label_2)))) # I think we need to have four lines of text to make each hex icon text line up. otherwise the lines start at different places for shorter indicators.

eng_hex_living_well_2_df <- hex_living_well_2_df %>% 
  filter(Area_name == 'England') %>% 
  select(Indicator_short_name, Eng_value_label = Value_label)

hex_living_well_2_df <- hex_living_well_2_df %>% 
  left_join(eng_hex_living_well_2_df, by = 'Indicator_short_name')

for(i in 1:length(ICT_areas)){
  
  ICT_x <- ICT_areas[i]
  Parent_area_x = ICT_areas_lookup %>% filter(Area == ICT_x) %>% select(UTLA) %>% as.character()
  Parent_icb_area_x = trimws(ICT_areas_lookup_2 %>% filter(Area == ICT_x) %>% select(Sub_ICB) %>% as.character(), 'right')
  Parent_ccg_area_x = trimws(ICT_areas_lookup_3 %>% filter(Area == ICT_x) %>% select(CCG) %>% as.character(), 'right')
  
hex_df_i <- hex_living_well_2_df %>% 
    filter(Area_name == ICT_x) %>% 
    mutate(Ring_colour = factor(ifelse(At_ICT_level %in% c('Sub_ICB', 'CCG'), 'Registered_patients_geography_only', ifelse(At_ICT_level %in% c('UTLA'), 'Higher_geography_only', ifelse(Area_name %in% c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West') & At_ICT_level == 'LTLA', 'Higher_geography_only', At_ICT_level))),
                                levels = c('Registered_patients_geography_only', 'Higher_geography_only', 'LTLA', 'ICT level')))
plot_x <- ggplot(hex_df_i,
                   aes(x = x,
                       y = y)) +
    geom_star(starshape = 6,
              size = 70,
              aes(fill = Significance_England,
                  colour = Ring_colour),
              starstroke  = 2) +
    scale_x_continuous(limits = c(min(hex_df_i$x)-.4,max(hex_df_i$x)+.4)) +
    scale_y_continuous(limits = c(min(hex_df_i$y)-.3, max(hex_df_i$y)+.3)) +
    scale_fill_manual(values = c(hex_colours_better, hex_colours_similar, hex_colours_worse, hex_colours_na),
                      drop = FALSE,
                      name = 'Compared to England') +
    scale_colour_manual(values = c('Higher_geography_only' = 'maroon', 'Registered_patients_geography_only' = '#2e2d88', 'ICT level' = '#ffffff', 'LTLA' = '#ffffff'),
                        breaks = c('Higher_geography_only', 'Registered_patients_geography_only'),
                        label = c(paste0('Available at a higher geography (', Parent_area_x, ' local authority) only'), paste0('Indicators are published at primary care or other organisation level (',Parent_ccg_area_x, ' and ', Parent_icb_area_x, ')\nrepresenting registered patients rather than residents')),
                        name = '',
                        drop = FALSE) + 
    geom_text(aes(x - .4,
                  y + .11,
                  label = paste0(Value_label)),
              hjust = 0,
              vjust = 0,
              fontface = 'bold',
              size = 9) +
    labs(title = paste0(unique(hex_df_i$Figure_ID), ' - ', ICT_x, ifelse(str_detect(ICT_x, 'Brighton and Hove'), '', ' ICT'))) +
    geom_text(aes(x - .4,
                  y - .15,
                  label = Indicator_label_2),
              hjust = 0,
              vjust = 0) +
    geom_text(aes(x - .4,
                  y - .15,
                  label = paste0('England: ', Eng_value_label, ', ', Time_period)),
              fontface = 'bold',
              hjust = 0,
              vjust = 0) +
    theme_hex() +
    guides(fill = guide_legend(override.aes = list(size = 11, colour = '#ffffff')),
           colour = guide_legend(override.aes = list(size = 10, drop.legend = FALSE))) +
    theme(legend.box="vertical",
          legend.margin=margin())
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(local_output_directory, '/Living_well_2_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 15,
      height = 11)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(local_output_directory, '/Living_well_2_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1400,
      height = 1000,
      res = 90)
  print(plot_x)
  dev.off()
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(synced_sp_directory, '/Hex_figures/Living_well_2/Living_well_2_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 15,
      height = 11)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(synced_sp_directory, '/Hex_figures/Living_well_2/Living_well_2_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1400,
      height = 1000,
      res = 90)
  print(plot_x)
  dev.off()
  
}

# Ageing well ####

# hex_indicators %>% 
  # filter(Figure_ID == 'Ageing well') 

# ASC_social_contact
ASC_social_contact_raw <-  fingertips_data(IndicatorID = 90280,
                                           AreaTypeID = 'All',
                                           categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Age == '18+ yrs') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'ASC_social_contact') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'ASC_social_contact') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(ASC_social_contact_raw$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

ASC_social_contact <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(ASC_social_contact_raw, by = c('UTLA' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) 

rm(ASC_social_contact_raw)

# ASC_carers_social_contact
ASC_carers_social_contact_raw <-  fingertips_data(IndicatorID = 90638,
                                           AreaTypeID = 'All',
                                           categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Age == '18+ yrs') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'ASC_carers_social_contact') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'ASC_carers_social_contact') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(ASC_carers_social_contact_raw$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

ASC_carers_social_contact <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(ASC_carers_social_contact_raw, by = c('UTLA' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) 

rm(ASC_carers_social_contact_raw)

# ASC_long_term_positive_qol_score
# ASC_carers_positive_qol_score


if(file.exists(paste0(local_data_directory, '/ASCFR_2021_22.xlsx')) == FALSE){
download.file('https://files.digital.nhs.uk/AF/4C9A4F/ASCFR%20and%20SALT%20Data%20Tables%202021-22.xlsx',
              paste0(local_data_directory, '/ASCFR_2021_22.xlsx'),
              mode = 'wb')
}

ASC_denominator_Eng <- read_excel(paste0(local_data_directory, '/ASCFR_2021_22.xlsx'),
                              sheet = "T36", 
                              col_types = c("text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), 
                              skip = 8) %>% 
  select(Area_name = '...5',  Clients_per_100 = '...9', Clients = '...10', Denominator = '...11') %>% 
  filter(Area_name == 'England') %>% 
  mutate(Age_group = '65+')

ASC_denominator <- read_excel(paste0(local_data_directory, '/ASCFR_2021_22.xlsx'),
                              sheet = "T36", 
                              col_types = c("text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), 
                              skip = 20) %>% 
  select(Area_code = '...1', Area_name = '...3', Clients_per_100 = '...9', Clients = '...10', Denominator = '...11') %>% 
  mutate(Age_group = '65+') %>% 
  bind_rows(ASC_denominator_Eng)

ASC_rate_df_Eng <- read_excel(paste0(local_data_directory, '/ASCFR_2021_22.xlsx'),
                          sheet = "T34", 
                          skip = 8) %>% 
  select(Area_name = '...5', Nursing = '...16', Residential = '...17', Community_direct_payment = '...18', Community_part_direct_payment = '...19', Community_CASSR_managed_budget = '...20', Community_CASSR_commissioned_support = '...21', Total = '...24') %>% 
  filter(Area_name %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  mutate(Nursing = replace_na(as.numeric(Nursing),0)) %>% 
  mutate(Residential = replace_na(as.numeric(Residential),0)) %>% 
  mutate(Community_direct_payment = replace_na(as.numeric(Community_direct_payment),0)) %>% 
  mutate(Community_part_direct_payment = replace_na(as.numeric(Community_part_direct_payment),0)) %>% 
  mutate(Community_CASSR_managed_budget = replace_na(as.numeric(Community_CASSR_managed_budget),0)) %>% 
  mutate(Community_CASSR_commissioned_support = replace_na(as.numeric(Community_CASSR_commissioned_support), 0)) %>% 
  mutate(Total = as.numeric(Total)) %>% 
  mutate(Community = Community_direct_payment + Community_part_direct_payment + Community_CASSR_managed_budget + Community_CASSR_commissioned_support) %>% 
  select(Area_name, Nursing, Residential, Community, Total)

ASC_rate_df_raw <- read_excel(paste0(local_data_directory, '/ASCFR_2021_22.xlsx'),
           sheet = "T34", 
           skip = 20) %>% 
  select(Area_name = '...3', Nursing = '...16', Residential = '...17', Community_direct_payment = '...18', Community_part_direct_payment = '...19', Community_CASSR_managed_budget = '...20', Community_CASSR_commissioned_support = '...21', Total = '...23') %>% 
  filter(Area_name %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  mutate(Nursing = replace_na(as.numeric(Nursing),0)) %>% 
  mutate(Residential = replace_na(as.numeric(Residential),0)) %>% 
  mutate(Community_direct_payment = replace_na(as.numeric(Community_direct_payment),0)) %>% 
  mutate(Community_part_direct_payment = replace_na(as.numeric(Community_part_direct_payment),0)) %>% 
  mutate(Community_CASSR_managed_budget = replace_na(as.numeric(Community_CASSR_managed_budget),0)) %>% 
  mutate(Community_CASSR_commissioned_support = replace_na(as.numeric(Community_CASSR_commissioned_support), 0)) %>% 
  mutate(Total = as.numeric(Total)) %>% 
  mutate(Community = Community_direct_payment + Community_part_direct_payment + Community_CASSR_managed_budget + Community_CASSR_commissioned_support) %>% 
  select(Area_name, Nursing, Residential, Community, Total) %>% 
  bind_rows(ASC_rate_df_Eng) %>% 
  left_join(ASC_denominator, by = 'Area_name')
  
rm(ASC_denominator, ASC_denominator_Eng, ASC_rate_df_Eng)

# ASC_nursing_rate
ASC_nursing_rate_raw <- ASC_rate_df_raw %>% 
  mutate(Value_label = paste0(format(round((Nursing / Denominator * 100000), 0), big.mark = ','),'*')) %>% 
  mutate(Indicator_short_name = 'ASC_nursing_rate',
         Significance_England = 'Not applicable') %>% 
  select(Area_name, Value_label, Indicator_short_name, Significance_England) %>% 
  mutate(Time_period = '2021/22')

ASC_nursing_rate <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(ASC_nursing_rate_raw, by = c('UTLA' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) 

rm(ASC_nursing_rate_raw)

# ASC_residential_rate
ASC_residential_rate_raw <- ASC_rate_df_raw %>% 
  mutate(Value_label = paste0(format(round((Residential / Denominator * 100000), 0), big.mark = ','),'*')) %>% 
  mutate(Indicator_short_name = 'ASC_residential_rate',
         Significance_England = 'Not applicable') %>% 
  select(Area_name, Value_label, Indicator_short_name, Significance_England) %>% 
  mutate(Time_period = '2021/22')

ASC_residential_rate <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(ASC_residential_rate_raw, by = c('UTLA' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) 

rm(ASC_residential_rate_raw)

# ASC_community_rate
ASC_community_rate_raw <- ASC_rate_df_raw %>% 
  mutate(Value_label = paste0(format(round((Community / Denominator * 100000), 0), big.mark = ','),'*')) %>% 
  mutate(Indicator_short_name = 'ASC_community_rate',
         Significance_England = 'Not applicable') %>% 
  select(Area_name, Value_label, Indicator_short_name, Significance_England) %>% 
  mutate(Time_period = '2021/22')

ASC_community_rate <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(ASC_community_rate_raw, by = c('UTLA' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) 

rm(ASC_community_rate_raw)

rm(ASC_rate_df_raw)

ASC_long_term_rate_Eng <- read_excel(paste0(local_data_directory, '/ASCFR_2021_22.xlsx'),
                                     sheet = "T36", 
                                     col_types = c("text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), 
                                     skip = 8) %>% 
  select(Area_name = '...5', Value_label = '...9') %>% 
  mutate(Value_label = format(round(Value_label, 0), big.mark = ',')) %>% 
  filter(Area_name == 'England') %>% 
  mutate(Indicator_short_name = 'ASC_long_term_rate') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2021/22')

ASC_long_term_rate_raw <- read_excel(paste0(local_data_directory, '/ASCFR_2021_22.xlsx'),
                                     sheet = "T36", 
                                     col_types = c("text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), 
                                     skip = 20) %>% 
  select(Area_name = '...3', Value_label = '...9') %>% 
  mutate(Value_label = format(round(Value_label, 0), big.mark = ',')) %>% 
  filter(Area_name %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  mutate(Indicator_short_name = 'ASC_long_term_rate') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Time_period = '2021/22') %>% 
  bind_rows(ASC_long_term_rate_Eng)

ASC_long_term_rate <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(ASC_long_term_rate_raw, by = c('UTLA' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) 

rm(ASC_long_term_rate_raw, ASC_long_term_rate_Eng)

# Care_home_beds
Care_home_beds_LTLA <-  fingertips_data(IndicatorID = 92489,
                                                   AreaTypeID = 'All',
                                                   categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Care_home_beds') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Care_home_beds') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Care_home_beds_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Care_home_beds <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Care_home_beds_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Care_home_beds_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Care_home_beds_LTLA)

# Care_home_admissions
Care_home_admissions_raw <-  fingertips_data(IndicatorID = 1194,
                                                  AreaTypeID = 'All',
                                                  categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Care_home_admissions') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Care_home_admissions') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Care_home_admissions_raw$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Care_home_admissions <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Care_home_admissions_raw, by = c('UTLA' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) 

rm(Care_home_admissions_raw)

# Care_home_self_funders

if(file.exists(paste0(local_data_directory, '/ONS_Care_home_self_funders.xlsx')) == FALSE){
download.file('https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/socialcare/datasets/carehomesandestimatingtheselffundingpopulationengland/march2022tofebruary2023/sfresidentialtables20222023.xlsx',
              paste0(local_data_directory, '/ONS_Care_home_self_funders.xlsx'),
              mode = 'wb')
}

Care_home_self_funders_Eng <- read_excel("//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Data/ONS_Care_home_self_funders.xlsx", 
                                          sheet = "Table_3", 
                                          skip = 4) %>% 
  select(Area_name = 'Region', Value = 'Self-funded residents (%)', England_Lower_CI = 'LCL \r\n(self-funded)', England_Upper_CI = 'UCL \r\n(self-funded)') %>% 
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  filter(Area_name %in% c(ICT_areas, 'England', 'ENGLAND')) %>% 
  mutate(Time_period = '2022/23') %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  mutate(Indicator_short_name = 'Care_home_self_funders') %>% 
  unique()

Care_home_self_funders_LTLA <- read_excel("//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/ICT profiles/Data/ONS_Care_home_self_funders.xlsx", 
                                          sheet = "Table_4", 
                                          skip = 4) %>% 
  select(Area_name = 'Area Name', Value = 'Self-funded residents (%)', Lower_CI = 'LCL \r\n(self-funded)', Upper_CI = 'UCL \r\n(self-funded)') %>% 
  mutate(Indicator_short_name = 'Care_home_self_funders') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  filter(Area_name %in% c(ICT_areas, 'England', 'ENGLAND')) %>% 
  mutate(Time_period = '2022/23') %>% 
  mutate(Significance_England = 'Not applicable') %>% 
  unique() %>% 
  bind_cols(Care_home_self_funders_Eng[c('England_Lower_CI', 'England_Upper_CI')]) %>% 
  mutate(Significance_England = ifelse(Area_name == 'England', 'not_applicable', ifelse(Lower_CI > England_Upper_CI, 'high', ifelse(Upper_CI < England_Lower_CI, 'low', 'similar'))))  %>% 
  bind_rows(Care_home_self_funders_Eng[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

Care_home_self_funders <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Care_home_self_funders_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Care_home_self_funders_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Care_home_self_funders_LTLA, Care_home_self_funders_Eng)

# Demenita_recorded_diagnosis
Demenita_recorded_diagnosis_LTLA <-  fingertips_data(IndicatorID = 92949,
                                        AreaTypeID = 'All',
                                        categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Demenita_recorded_diagnosis') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Demenita_recorded_diagnosis') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Demenita_recorded_diagnosis_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Demenita_recorded_diagnosis <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Demenita_recorded_diagnosis_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Demenita_recorded_diagnosis_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Demenita_recorded_diagnosis_LTLA)

# Emergency_admissions_falls
Emergency_admissions_falls_LTLA <-  fingertips_data(IndicatorID = 22401,
                                                     AreaTypeID = 'All',
                                                     categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (from Apr 2023)', 'England', 'Districts & UAs (from Apr 2023)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Emergency_admissions_falls') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Emergency_admissions_falls') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Emergency_admissions_falls_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Emergency_admissions_falls <- BH_local_data %>%
  filter(Indicator_short_name == 'Emergency_admissions_falls') %>%
  mutate(Value_label = paste0(round(Value_label, 1))) %>%
  mutate(Time_period = unique(Emergency_admissions_falls_LTLA$Time_period)) %>%
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Emergency_admissions_falls_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Emergency_admissions_falls_LTLA)

# Discharge_still_at_home
Discharge_still_at_home_raw <-  fingertips_data(IndicatorID = 90584,
                                             AreaTypeID = 402,
                                             categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Discharge_still_at_home') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Discharge_still_at_home') %>%
#   mutate(Value_label = paste0(round(Value_label, 1))) %>%
#   mutate(Time_period = unique(Discharge_still_at_home_raw$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Discharge_still_at_home <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Discharge_still_at_home_raw, by = c('UTLA' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) 

rm(Discharge_still_at_home_raw)

# Ageing well hex figure ####
hex_ageing_well_df <- ASC_social_contact %>% 
  bind_rows(ASC_carers_social_contact) %>% 
  # bind_rows(ASC_long_term_positive_qol_score) %>% 
  # bind_rows(ASC_carers_positive_qol_score) %>% 
  bind_rows(ASC_nursing_rate) %>%
  bind_rows(ASC_residential_rate) %>%
  bind_rows(ASC_community_rate) %>%
  bind_rows(ASC_long_term_rate) %>%
  bind_rows(Care_home_beds) %>% 
  bind_rows(Care_home_admissions) %>% 
  bind_rows(Care_home_self_funders) %>% 
  bind_rows(Demenita_recorded_diagnosis) %>% 
  bind_rows(Emergency_admissions_falls) %>% 
  bind_rows(Discharge_still_at_home) %>% 
  left_join(hex_indicators, by = 'Indicator_short_name') %>% 
  mutate(Significance_England = factor(ifelse(Polarity == 'Not applicable', 'Not applicable', ifelse(Significance_England %in% c('high', 'Higher') & Polarity == 'Lower is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Higher is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Lower is better', 'Better', ifelse(Significance_England %in% c('high', 'Higher') & Polarity == 'Higher is better', 'Better', ifelse(Significance_England %in% c('Not compared', 'Not applicable','not_applicable'), 'Not applicable', ifelse(Significance_England %in% c('similar', 'Similar', 'same'), 'Similar', Significance_England))))))), levels = c('Better', 'Similar', 'Worse', 'Not applicable'))) %>% 
  mutate(Indicator_label_2 = ifelse(Indicator_short_name %in% c('ASC_nursing_rate', 'ASC_residential_rate', 'ASC_community_rate','ASC_long_term_rate'), gsub('(.{1,29})(\\s|$)', '\\1\n', Indicator_label), gsub('(.{1,27})(\\s|$)', '\\1\n', Indicator_label))) %>% # add line break every 25 characters (or at the end of the word if 25 falls in the middle)
  mutate(Indicator_label_2 = ifelse(str_count(Indicator_label_2, '\\n') == 1, paste0(Indicator_label_2, '\n\n\n'), ifelse(str_count(Indicator_label_2, '\\n') == 2, paste0(Indicator_label_2, '\n\n'),ifelse(str_count(Indicator_label_2, '\\n') == 3, paste0(Indicator_label_2, '\n'), Indicator_label_2)))) # I think we need to have four lines of text to make each hex icon text line up. otherwise the lines start at different places for shorter indicators.

eng_hex_ageing_well_df <- hex_ageing_well_df %>% 
  filter(Area_name == 'England') %>% 
  select(Indicator_short_name, Eng_value_label = Value_label)

hex_ageing_well_df <- hex_ageing_well_df %>% 
  left_join(eng_hex_ageing_well_df, by = 'Indicator_short_name')

for(i in 1:length(ICT_areas)){
  
  ICT_x <- ICT_areas[i]
  Parent_area_x = ICT_areas_lookup %>% filter(Area == ICT_x) %>% select(UTLA) %>% as.character()
  Parent_icb_area_x = trimws(ICT_areas_lookup_2 %>% filter(Area == ICT_x) %>% select(Sub_ICB) %>% as.character(), 'right')
  Parent_ccg_area_x = trimws(ICT_areas_lookup_3 %>% filter(Area == ICT_x) %>% select(CCG) %>% as.character(), 'right')
  
  hex_df_i <- hex_ageing_well_df %>% 
    filter(Area_name == ICT_x) %>% 
    mutate(Ring_colour = factor(ifelse(At_ICT_level %in% c('Sub_ICB', 'CCG'), 'Registered_patients_geography_only', ifelse(At_ICT_level %in% c('UTLA'), 'Higher_geography_only', ifelse(Area_name %in% c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West') & At_ICT_level == 'LTLA', 'Higher_geography_only', At_ICT_level))),
                                levels = c('Registered_patients_geography_only', 'Higher_geography_only', 'LTLA', 'ICT level')))
plot_x <-  ggplot(hex_df_i,
                   aes(x = x,
                       y = y)) +
    geom_star(starshape = 6,
              size = 70,
              aes(fill = Significance_England,
                  colour = Ring_colour),
              starstroke  = 2) +
    scale_x_continuous(limits = c(min(hex_df_i$x)-.4,max(hex_df_i$x)+.4)) +
    scale_y_continuous(limits = c(min(hex_df_i$y)-.3, max(hex_df_i$y)+.3)) +
    scale_fill_manual(values = c(hex_colours_better, hex_colours_similar, hex_colours_worse, hex_colours_na),
                      drop = FALSE,
                      name = 'Compared to England') +
    scale_colour_manual(values = c('Higher_geography_only' = 'maroon', 'Registered_patients_geography_only' = '#2e2d88', 'ICT level' = '#ffffff', 'LTLA' = '#ffffff'),
                        breaks = c('Higher_geography_only'),
                        label = paste0('Available at a higher geography (', Parent_area_x, ' local authority) only'),
                        # breaks = c('Higher_geography_only', 'Registered_patients_geography_only'),
                        # label = c(paste0('Available at a higher geography (', Parent_area_x, ' local authority) only'), paste0('Indicators are published at primary care or other organisation level (',Parent_ccg_area_x, ' and ', Parent_icb_area_x, ')\nrepresenting registered patients rather than residents')),
                        name = '',
                        drop = FALSE) + 
    geom_text(aes(x - .4,
                  y + .11,
                  label = paste0(Value_label)),
              hjust = 0,
              vjust = 0,
              fontface = 'bold',
              size = 9) +
    labs(title = paste0(unique(hex_df_i$Figure_ID), ' - ', ICT_x, ifelse(str_detect(ICT_x, 'Brighton and Hove'), '', ' ICT')),
         caption = paste0('* Note: rates have been calculated on rounded values and may not match outputs published elsewhere exactly.')) +
    geom_text(aes(x - .4,
                  y - .15,
                  label = Indicator_label_2),
              hjust = 0,
              vjust = 0) +
    geom_text(aes(x - .4,
                  y - .15,
                  label = paste0('England: ', Eng_value_label, ', ', Time_period)),
              fontface = 'bold',
              hjust = 0,
              vjust = 0) +
    theme_hex() +
    guides(fill = guide_legend(override.aes = list(size = 11, colour = '#ffffff')),
           colour = guide_legend(override.aes = list(size = 10, drop.legend = FALSE))) +
    theme(legend.box="vertical",
          legend.margin=margin())
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(local_output_directory, '/Ageing_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 12,
      height = 11.5)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(local_output_directory, '/Ageing_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1050,
      height = 1050,
      res = 90)
  print(plot_x)
  dev.off()
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(synced_sp_directory, '/Hex_figures/Ageing_well/Ageing_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 12,
      height = 11.5)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(synced_sp_directory, '/Hex_figures/Ageing_well/Ageing_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 1050,
      height = 1050,
      res = 90)
  print(plot_x)
  dev.off()
  
}

# Dying well ####

hex_indicators %>% 
  filter(Figure_ID == 'Dying well') %>% 
  View()

# Deaths_at_home
Deaths_at_home_LTLA <-  fingertips_data(IndicatorID = 93476,
                                                    AreaTypeID = 'All',
                                                    categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == 'All ages') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Deaths_at_home') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Deaths_at_home') %>%
#   mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>%
#   mutate(Time_period = unique(Deaths_at_home_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Deaths_at_home <- BH_local_data %>%
  filter(Indicator_short_name == 'Deaths_at_home') %>%
  mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>%
  mutate(Time_period = unique(Deaths_at_home_LTLA$Time_period)) %>%
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Deaths_at_home_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Deaths_at_home_LTLA)

# Deaths_in_hospital
Deaths_in_hospital_LTLA <-  fingertips_data(IndicatorID = 93474,
                                        AreaTypeID = 'All',
                                        categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == 'All ages') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Deaths_in_hospital') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Deaths_in_hospital') %>%
#   mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>%
#   mutate(Time_period = unique(Deaths_in_hospital_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Deaths_in_hospital <- BH_local_data %>%
  filter(Indicator_short_name == 'Deaths_in_hospital') %>%
  mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>%
  mutate(Time_period = unique(Deaths_in_hospital_LTLA$Time_period)) %>%
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Deaths_in_hospital_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Deaths_in_hospital_LTLA)

# Deaths_in_care_home
Deaths_in_care_home_LTLA <-  fingertips_data(IndicatorID = 93475,
                                            AreaTypeID = 'All',
                                            categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == 'All ages') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Deaths_in_care_home') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Deaths_in_care_home') %>%
#   mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>%
#   mutate(Time_period = unique(Deaths_in_care_home_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Deaths_in_care_home <- BH_local_data %>%
  filter(Indicator_short_name == 'Deaths_in_care_home') %>%
  mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>%
  mutate(Time_period = unique(Deaths_in_care_home_LTLA$Time_period)) %>%
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Deaths_in_care_home_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Deaths_in_care_home_LTLA)

# Deaths_in_hospice
Deaths_in_hospice_LTLA <-  fingertips_data(IndicatorID = 93478,
                                             AreaTypeID = 'All',
                                             categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == 'All ages') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Deaths_in_hospice') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Deaths_in_hospice') %>%
#   mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>%
#   mutate(Time_period = unique(Deaths_in_hospice_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Deaths_in_hospice <- BH_local_data %>%
  filter(Indicator_short_name == 'Deaths_in_hospice') %>%
  mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>%
  mutate(Time_period = unique(Deaths_in_hospice_LTLA$Time_period)) %>%
  select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period) %>% 
  bind_rows(Deaths_in_hospice_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Deaths_in_hospice_LTLA)

# QOF_palliative_care_prevalence
QOF_palliative_care_prevalence_Sub_ICB <- fingertips_data(IndicatorID = 294,
                                                AreaTypeID = 'All',
                                                     categorytype = FALSE) %>% 
  filter(AreaType %in% c('ICB sub-locations', 'England')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England', 'Sussex ICB - 09D ', 'Sussex ICB - 70F ', 'Sussex ICB - 97R ')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'QOF_palliative_care_prevalence') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1), '%')) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

QOF_palliative_care_prevalence <- ICT_areas_lookup_2 %>% 
  bind_rows(data.frame(Area = 'England', Sub_ICB = 'England')) %>% 
  left_join(QOF_palliative_care_prevalence_Sub_ICB, by = c('Sub_ICB' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name)

rm(QOF_palliative_care_prevalence_Sub_ICB)

# Winter_mortality_index
Winter_mortality_index_LTLA <-  fingertips_data(IndicatorID = 90360,
                                           AreaTypeID = 401,
                                           categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == 'All ages') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Winter_mortality_index') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Winter_mortality_index') %>%
#   mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>%
#   mutate(Time_period = unique(Winter_mortality_index_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Winter_mortality_index <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Winter_mortality_index_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Winter_mortality_index_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Winter_mortality_index_LTLA)

# Winter_mortality_index_85_plus
Winter_mortality_index_85_plus_LTLA <-  fingertips_data(IndicatorID = 90361,
                                                AreaTypeID = 401,
                                                categorytype = FALSE) %>%
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  filter(Age == '85+ yrs') %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Winter_mortality_index_85_plus') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Value_label, Lower_CI, Upper_CI, Indicator_short_name, Significance_England, Time_period) %>% 
  unique()

# BH_local_data %>%
#   filter(Indicator_short_name == 'Winter_mortality_index_85_plus') %>%
#   mutate(Value_label = paste0(round((Value_label * 100), 1), '%')) %>%
#   mutate(Time_period = unique(Winter_mortality_index_85_plus_LTLA$Time_period)) %>%
#   select(Area_name, Value_label, Significance_England, Indicator_short_name, Time_period)

Winter_mortality_index_85_plus <- data.frame(Area_name = c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West')) %>% 
  bind_cols(subset(Winter_mortality_index_85_plus_LTLA, Area_name == 'Brighton and Hove')[c('Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')]) %>% 
  bind_rows(Winter_mortality_index_85_plus_LTLA[c('Area_name','Value_label', 'Significance_England', 'Indicator_short_name', 'Time_period')])

rm(Winter_mortality_index_85_plus_LTLA)

# Dying well hex figure ####
hex_dying_well_df <- Deaths_at_home %>% 
  bind_rows(Deaths_in_hospital) %>% 
  bind_rows(Deaths_in_care_home) %>% 
  bind_rows(Deaths_in_hospice) %>% 
  bind_rows(QOF_palliative_care_prevalence) %>% 
  bind_rows(Winter_mortality_index) %>% 
  bind_rows(Winter_mortality_index_85_plus) %>% 
  left_join(hex_indicators, by = 'Indicator_short_name') %>% 
  mutate(Significance_England = factor(ifelse(Polarity == 'Not applicable', 'Not applicable', ifelse(Significance_England %in% c('high', 'Higher') & Polarity == 'Lower is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Higher is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Lower is better', 'Better', ifelse(Significance_England %in% c('high', 'Higher') & Polarity == 'Higher is better', 'Better', ifelse(Significance_England %in% c('Not compared', 'Not applicable','not_applicable'), 'Not applicable', ifelse(Significance_England %in% c('similar', 'Similar', 'same'), 'Similar', Significance_England))))))), levels = c('Better', 'Similar', 'Worse', 'Not applicable'))) %>% 
  mutate(Indicator_label_2 = gsub('(.{1,27})(\\s|$)', '\\1\n', Indicator_label)) %>% # add line break every 25 characters (or at the end of the word if 25 falls in the middle)
  mutate(Indicator_label_2 = ifelse(str_count(Indicator_label_2, '\\n') == 1, paste0(Indicator_label_2, '\n\n\n'), ifelse(str_count(Indicator_label_2, '\\n') == 2, paste0(Indicator_label_2, '\n\n'),ifelse(str_count(Indicator_label_2, '\\n') == 3, paste0(Indicator_label_2, '\n'), Indicator_label_2)))) # I think we need to have four lines of text to make each hex icon text line up. otherwise the lines start at different places for shorter indicators.

eng_hex_dying_well_df <- hex_dying_well_df %>% 
  filter(Area_name == 'England') %>% 
  select(Indicator_short_name, Eng_value_label = Value_label)

hex_dying_well_df <- hex_dying_well_df %>% 
  left_join(eng_hex_dying_well_df, by = 'Indicator_short_name')

for(i in 1:length(ICT_areas)){
  
  ICT_x <- ICT_areas[i]
  Parent_area_x = ICT_areas_lookup %>% filter(Area == ICT_x) %>% select(UTLA) %>% as.character()
  Parent_icb_area_x = trimws(ICT_areas_lookup_2 %>% filter(Area == ICT_x) %>% select(Sub_ICB) %>% as.character(), 'right')
  Parent_ccg_area_x = trimws(ICT_areas_lookup_3 %>% filter(Area == ICT_x) %>% select(CCG) %>% as.character(), 'right')
  
  hex_df_i <- hex_dying_well_df %>% 
    filter(Area_name == ICT_x) %>% 
    mutate(Ring_colour = factor(ifelse(At_ICT_level %in% c('Sub_ICB', 'CCG'), 'Registered_patients_geography_only', ifelse(At_ICT_level %in% c('UTLA'), 'Higher_geography_only', ifelse(Area_name %in% c('Brighton and Hove North', 'Brighton and Hove East', 'Brighton and Hove Central', 'Brighton and Hove West') & At_ICT_level == 'LTLA', 'Higher_geography_only', At_ICT_level))),
                                levels = c('Registered_patients_geography_only', 'Higher_geography_only', 'LTLA', 'ICT level')))
  plot_x <- ggplot(hex_df_i,
                   aes(x = x,
                       y = y)) +
    geom_star(starshape = 6,
              size = 70,
              aes(fill = Significance_England,
                  colour = Ring_colour),
              starstroke  = 2) +
    scale_x_continuous(limits = c(min(hex_df_i$x)-.4,max(hex_df_i$x)+.4)) +
    scale_y_continuous(limits = c(min(hex_df_i$y)-.3, max(hex_df_i$y)+.3)) +
    scale_fill_manual(values = c(hex_colours_better, hex_colours_similar, hex_colours_worse, hex_colours_na),
                      drop = FALSE,
                      name = 'Compared to England') +
    scale_colour_manual(values = c('Higher_geography_only' = 'maroon', 'Registered_patients_geography_only' = '#2e2d88', 'ICT level' = '#ffffff', 'LTLA' = '#ffffff'),
                        breaks = c('Higher_geography_only', 'Registered_patients_geography_only'),
                        label = c(paste0('Available at a higher geography (', Parent_area_x, ' local authority) only'), paste0('Indicators are published at primary care or other organisation level (',Parent_ccg_area_x, ' and ', Parent_icb_area_x, ')\nrepresenting registered patients rather than residents')),
                        name = '',
                        drop = FALSE) + 
    geom_text(aes(x - .4,
                  y + .11,
                  label = paste0(Value_label)),
              hjust = 0,
              vjust = 0,
              fontface = 'bold',
              size = 9) +
    labs(title = paste0(unique(hex_df_i$Figure_ID), ' - ', ICT_x, ifelse(str_detect(ICT_x, 'Brighton and Hove'), '', ' ICT'))) +
    geom_text(aes(x - .4,
                  y - .15,
                  label = Indicator_label_2),
              hjust = 0,
              vjust = 0) +
    geom_text(aes(x - .4,
                  y - .15,
                  label = paste0('England: ', Eng_value_label, ',\n', Time_period)),
              fontface = 'bold',
              hjust = 0,
              vjust = 0) +
    theme_hex() +
    guides(fill = guide_legend(override.aes = list(size = 11, colour = '#ffffff')),
           colour = guide_legend(override.aes = list(size = 10, drop.legend = FALSE), nrow = 2)) +
    theme(legend.box="vertical",
          legend.margin=margin())
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(local_output_directory, '/Dying_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 10,
      height = 7)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(local_output_directory, '/Dying_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 900,
      height = 625,
      res = 90)
  print(plot_x)
  dev.off()
  
  # a figure using all blocks will need to be much wider can use 9 x 9.3 unit 
  svg(filename = paste0(synced_sp_directory, '/Hex_figures/Dying_well/Dying_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.svg'),
      width = 10,
      height = 7)
  print(plot_x)
  dev.off()
  
  png(filename = paste0(synced_sp_directory, '/Hex_figures/Dying_well/Dying_well_hex_', gsub(' ', '_', tolower(ICT_x)) ,'.png'),
      width = 900,
      height = 625,
      res = 90)
  print(plot_x)
  dev.off()
  
}

# Extra indicators

# Children_in_care
Children_in_care_raw <- fingertips_data(IndicatorID = 90803,
                AreaTypeID = 402,
                categorytype = FALSE) %>% 
  filter(AreaType %in% c('Counties & UAs (2021/22-2022/23)', 'England', 'Districts & UAs (2021/22-2022/23)')) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  filter(Sex == 'Persons') %>% 
  filter(AreaName %in% c(ICT_areas, 'East Sussex', 'West Sussex', 'England')) %>% 
  select(IndicatorID, Indicator = IndicatorName, Area_code = AreaCode, Area_name = AreaName, Sex, Age, Time_period = Timeperiod, Numerator = Count, Denominator, Value, Lower_CI = LowerCI95.0limit, Upper_CI = UpperCI95.0limit, Significance_England = ComparedtoEnglandvalueorpercentiles, Trend = RecentTrend, TimeperiodSortable) %>%
  unique() %>% # Brighton and Hove will appear twice (once for UTLA and once for LTLA)
  mutate(Indicator_short_name = 'Children_in_care') %>% # we add this ID so we can join the data with the hex indicators
  mutate(Value_label = paste0(round(Value, 1))) %>% 
  select(Area_name, Sex, Age, Time_period, Value_label, Lower_CI, Upper_CI, Significance_England, Indicator_short_name) %>% 
  unique()

Children_in_care <- ICT_areas_lookup %>% 
  bind_rows(data.frame(Area = 'England', UTLA = 'England')) %>% 
  left_join(Children_in_care_raw, by = c('UTLA' = 'Area_name')) %>% 
  rename(Area_name = Area) %>% 
  select(Area_name, Time_period, Value_label, Significance_England, Indicator_short_name) 

rm(Children_in_care_raw)

# Export all data ####

all_indicators_df <- Working_age_out_of_work %>% 
  bind_rows(Fuel_poverty) %>% 
  bind_rows(Housing_affordability_ratio) %>% 
  bind_rows(National_deprivation_quintile_1) %>% 
  bind_rows(Children_living_in_poverty) %>% 
  bind_rows(Older_people_living_in_poverty) %>% 
  bind_rows(police_df) %>%
  bind_rows(Good_level_development_reception) %>% 
  bind_rows(Average_attainment_8_score_16) %>% 
  bind_rows(Average_attainment_8_score_16_in_care) %>% 
  bind_rows(People_no_qualifications) %>% 
  bind_rows(Working_age_in_employment) %>% 
  bind_rows(Population_density) %>% 
  bind_rows(HH_overcrowding) %>% 
  bind_rows(HH_no_central_heating) %>% 
  bind_rows(HH_private_rented) %>% 
  bind_rows(HH_at_least_one_resident) %>% 
  bind_rows(HH_living_alone_under_66) %>% 
  bind_rows(HH_living_alone_over_66) %>% 
  bind_rows(HH_lone_parent) %>% 
  bind_rows(HLE_males_65) %>% 
  bind_rows(HLE_females_65) %>% 
  bind_rows(HLE_males_at_birth) %>% 
  bind_rows(HLE_females_at_birth) %>% 
  bind_rows(LE_males_65) %>% 
  bind_rows(LE_females_65) %>% 
  bind_rows(SII_LE_inequality_males_65) %>% 
  bind_rows(SII_LE_inequality_females_65) %>% 
  bind_rows(LE_males_at_birth) %>% 
  bind_rows(LE_females_at_birth) %>% 
  bind_rows(SII_LE_inequality_males_at_birth) %>% 
  bind_rows(SII_LE_inequality_females_at_birth) %>% 
  bind_rows(NEET) %>% 
  bind_rows(Care_leavers_in_EET) %>% 
  bind_rows(Hospital_admissions_uninentional_deliberate_harm_under_five) %>% 
  bind_rows(Hospital_admissions_asthma_under_19) %>% 
  bind_rows(Hospital_admissions_self_harm_10_24) %>% 
  bind_rows(Breastfeeding_six_weeks) %>% 
  bind_rows(MMR_two_dose_5_years) %>% 
  bind_rows(Expected_communication_at_2) %>% 
  bind_rows(Obvious_tooth_decay_at_5) %>% 
  bind_rows(Healthy_weight_year_6) %>% 
  bind_rows(Infant_mortality_rate) %>% 
  bind_rows(SATOD) %>% 
  bind_rows(Premature_birth_rate) %>% 
  bind_rows(Low_birthweight) %>% 
  bind_rows(Flu_65_plus) %>% 
  bind_rows(Flu_at_risk) %>% 
  bind_rows(PPV_immunisation) %>% 
  bind_rows(COVID_19_spring_booster) %>%
  bind_rows(HPV_coverage_13_14_female_2_doses) %>% 
  bind_rows(HPV_coverage_13_14_male_2_doses) %>% 
  bind_rows(Adult_smoking_prevalence) %>% 
  bind_rows(QOF_hypertension_prevalence) %>% 
  bind_rows(QOF_COPD_prevalence) %>% 
  bind_rows(QOF_depression_prevalence) %>% 
  bind_rows(Low_happiness_score) %>% 
  bind_rows(High_anxiety_score) %>% 
  bind_rows(Obesity_18_plus) %>% 
  bind_rows(Physically_active_adults) %>% 
  bind_rows(Physically_inactive_adults) %>% 
  bind_rows(Walking_for_travel_adults) %>% 
  bind_rows(Cycling_for_travel_adults) %>% 
  bind_rows(Health_index_score) %>% 
  bind_rows(Air_pollution_attributable_mortality) %>% 
  bind_rows(Alcohol_specific_admissions) %>% 
  bind_rows(Alcohol_consumption_over_14_units) %>% 
  bind_rows(Under_75_CVD_mortality) %>% 
  bind_rows(Under_75_respiratory_mortality) %>% 
  bind_rows(Under_75_cancer_mortality) %>% 
  bind_rows(Under_75_excess_deaths_SMI) %>% 
  bind_rows(Drug_related_deaths) %>% 
  bind_rows(Suicide_undetermined_injury_mortality) %>% 
  bind_rows(Cancer_screening_breast) %>% 
  bind_rows(Cancer_screening_bowel) %>% 
  # bind_rows(Lung_check) %>% 
  bind_rows(TWW_new_cancer_diagnoses) %>% 
  bind_rows(Cancers_diagnosed_at_stage_1_and_2) %>% 
  bind_rows(Cancer_screening_cervical_cancer_2549) %>% 
  bind_rows(NHS_Health_Check_uptake) %>% 
  bind_rows(Health_Check_uptake_LD) %>% 
  bind_rows(Health_Check_uptake_SMI) %>% 
  bind_rows(Emergency_admissions_self_harm) %>% 
  bind_rows(Emergency_admissions_COPD) %>% 
  bind_rows(Emergency_admissions_pneumonia) %>% 
  bind_rows(Emergency_admissions_asthma) %>% 
  bind_rows(ASC_social_contact) %>% 
  bind_rows(ASC_carers_social_contact) %>% 
  # bind_rows(ASC_long_term_positive_qol_score) %>% 
  # bind_rows(ASC_carers_positive_qol_score) %>% 
  bind_rows(ASC_nursing_rate) %>%
  bind_rows(ASC_residential_rate) %>%
  bind_rows(ASC_community_rate) %>%
  bind_rows(ASC_long_term_rate) %>%
  bind_rows(Care_home_beds) %>% 
  bind_rows(Care_home_admissions) %>% 
  bind_rows(Care_home_self_funders) %>% 
  bind_rows(Demenita_recorded_diagnosis) %>% 
  bind_rows(Emergency_admissions_falls) %>% 
  bind_rows(Discharge_still_at_home) %>% 
  bind_rows(Deaths_at_home) %>% 
  bind_rows(Deaths_in_hospital) %>% 
  bind_rows(Deaths_in_care_home) %>% 
  bind_rows(Deaths_in_hospice) %>% 
  bind_rows(QOF_palliative_care_prevalence) %>% 
  bind_rows(Winter_mortality_index) %>% 
  bind_rows(Winter_mortality_index_85_plus) %>% 
  bind_rows(Children_in_care) %>% 
  left_join(hex_indicators, by = 'Indicator_short_name') %>% 
  mutate(Significance_England = factor(ifelse(Polarity == 'Not applicable', 'Not applicable', ifelse(Significance_England == 'high' & Polarity == 'Lower is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Higher is better', 'Worse', ifelse(Significance_England == 'low' & Polarity == 'Lower is better', 'Better', ifelse(Significance_England == 'high' & Polarity == 'Higher is better', 'Better', ifelse(Significance_England %in% c('Not compared', 'Not applicable','not_applicable'), 'Not applicable', ifelse(Significance_England %in% c('similar', 'Similar', 'same'), 'Similar', Significance_England))))))), levels = c('Better', 'Similar', 'Worse', 'Not applicable')))

all_indicators_df %>% 
  write.csv(., paste0(synced_sp_directory, '/HEX_indicator_master_dataset.csv'), 
            row.names = FALSE, 
            na = '')

# Load life course indicators ####
all_indicators_df <- read_csv(paste0(synced_sp_directory, '/HEX_indicator_master_dataset.csv'))

LC_indicator_list <- read_csv(paste0(synced_sp_directory, '/Life_course_indicators.csv'))

LC_data <- all_indicators_df %>% 
  filter(Indicator_short_name %in% LC_indicator_list$Indicator_short_name) %>% 
  left_join(LC_indicator_list, by = 'Indicator_short_name') %>% 
  arrange(Area_name, Position)

# for(i in 1:length(ICT_areas)){
#   
# ICT_area_x = ICT_areas[i]
# 
# }
# Birth cohort 2020/21

library(tidyverse)
library(readr)
library(lemon)

##### packages used in this script

packages <- c('easypackages', 'tidyr', 'dplyr', 'readxl', 'readr', 'purrr', 'stringr', 'PHEindicatormethods', 'ggplot2', 'scales', 'showtext', 'viridis', 'lemon', 'zoo', 'spdplyr', 'sp', 'sf', 'ggmap', 'geojsonio', 'ggpol', 'leaflet')

install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

# install.packages('spbabel')
# install.packages("C:/Users/asus/Downloads/spdplyr_0.1.2.tar.gz",
#                  repos = NULL,
#                  type = 'source')

output_directory <- 'urban-system/Outputs'


# https://explore-education-statistics.service.gov.uk/find-statistics/secondary-and-primary-school-applications-and-offers

AppsandOffers_2024_SchoolLevel <- read_csv("urban-system/Data/AppsandOffers_2024_SchoolLevel.csv") %>% 
  filter(school_urn %in% c('125981', '142141', '125997', '125816'))

df <- AppsandOffers_2024_SchoolLevel %>% 
  select(Financial_year = time_period, Name = school_name, total_number_places_offered, number_1st_preference_offers,  number_2nd_preference_offers, number_3rd_preference_offers, times_put_as_any_preferred_school, times_put_as_1st_preference, times_put_as_2nd_preference, times_put_as_3rd_preference, proportion_1stprefs_v_1stprefoffers, proportion_1stprefs_v_totaloffers, establishment_type, denomination, FSM_eligible_percent, school_urn) %>% 
  mutate(Financial_year = factor(paste0(substr(Financial_year, 1,4), '/', substr(Financial_year, 5,6)),
                                 levels = c('2014/15','2015/16','2016/17','2017/18','2018/19','2019/20', '2020/21', '2021/22', '2022/23', '2023/24', '2024/25'))) %>% 
  mutate(Applications_per_place = times_put_as_any_preferred_school / total_number_places_offered)


df %>% 
  select(Financial_year, Name, total_number_places_offered, times_put_as_any_preferred_school) %>% 
  pivot_longer(cols = c('total_number_places_offered', 'times_put_as_any_preferred_school'),
               values_to = 'People') %>% 
  ggplot(aes(x = Financial_year,
             y = People,
             fill = name)) +
  geom_col(  position = position_dodge(width = .9),
           colour = 'black') +
  labs(title = 'Number of applications and places offered',
       subtitle = 'Any place preference.',
       x = '',
       y = '') +
  scale_y_continuous(limits = c(0, 170),
                     breaks = seq(0, 160, 50)) +
  scale_fill_manual(values = c('orange','maroon'),
                    labels = c('Times put as a\npreferred school', 'Number of places\navailable'),
                    name = '') +
  geom_text(aes(label = People,
                y = People + 10),
            size = 5,
            hjust = .5,
            vjust = .5,
            position = position_dodge(width = .9),
            show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5),
        legend.position = 'bottom',
        panel.grid.major.x = element_blank()) +
  facet_rep_wrap(~ Name,
                repeat.tick.labels = TRUE)

ggsave(plot = last_plot(),
       filename =  paste0(output_directory, '/Numbers_applications.png'),
       width = 28,
       height = 26.1,
       unit = 'cm',
       bg = 'white',
       dpi = 220)

df_dummy <- df %>% 
  rename(Name_1 = Name)

df %>% 
  ggplot(aes(x = Financial_year,
             y = Applications_per_place,
             group = Name)) +
  geom_line(data = df_dummy,
            aes(x = Financial_year,
                y  = Applications_per_place,
                group = Name_1),
            colour = '#999999') +
  geom_line(aes(colour = Name),
            colour = '#000000')  +
  geom_point(shape = 21,
             size = 2,
             aes(fill = Name)) +
  labs(title = 'Applications per place offered',
       subtitle = 'Any place preference.',
       y = 'Applications per place',
       x = '') +
  scale_y_continuous(limits = c(0, 5),
                     breaks = seq(0, 5, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = .5),
        legend.position = 'none',
        panel.grid.major.x = element_blank()) +
  facet_rep_wrap(~ Name,
                 repeat.tick.labels = TRUE)

 ggsave(plot = last_plot(),
       filename =  paste0(output_directory, '/Applications_per_place.png'),
       width = 18,
       height = 16.1,
       unit = 'cm',
       bg = 'white',
       dpi = 220)

df %>% 
  select(Financial_year, Name, total_number_places_offered, times_put_as_1st_preference) %>% 
  pivot_longer(cols = c('total_number_places_offered', 'times_put_as_1st_preference'),
               values_to = 'People') %>% 
  ggplot(aes(x = Financial_year,
             y = People,
             fill = name)) +
  geom_col(position = 'dodge') +
  labs(title = 'Number of first choice applications and places offered', 
       subtitle = 'First preference only.',
       x = '',
       y = '') +
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  scale_fill_manual(values = c('lightblue','maroon'),
                    labels = c('Times put as a\npreferred school', 'Number of places\navailable'),
                    name = '') +
  geom_text(aes(label = People,
                y = People + 2),
            size = 5,
            hjust = .5,
            vjust = .5,
            position = position_dodge(width = .9),
            show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5),
        legend.position = 'bottom',
        panel.grid.major.x = element_blank()) +
  facet_rep_wrap(~ Name,
                 repeat.tick.labels = TRUE)

ggsave(plot = last_plot(),
       filename =  paste0(output_directory, '/Numbers_first_choice_applications.png'),
       width = 28,
       height = 26.1,
       unit = 'cm',
       bg = 'white',
       dpi = 220)

df %>% 
  select(Financial_year, Name, times_put_as_1st_preference, times_put_as_2nd_preference, times_put_as_3rd_preference) %>% 
  pivot_longer(cols = c('times_put_as_1st_preference', 'times_put_as_2nd_preference', 'times_put_as_3rd_preference'),
               values_to = 'People',
               names_to = 'Preference_rank') %>% 
  mutate(Preference_rank = factor(Preference_rank,
                                  levels = c('times_put_as_1st_preference', 'times_put_as_2nd_preference', 'times_put_as_3rd_preference'))) %>% 
  ggplot(aes(x = Financial_year,
             y = People,
             fill = Preference_rank)) +
  geom_col() +
  scale_fill_manual(values = c('lightblue', '#189bcc', '#de2b51'),
                    labels = c('First choice', 'Second choice', 'Third choice'),
                    name = 'Preference rank') +
  labs(title = 'Number of applications by preference rank',
       y = '',
       x = '') +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = .5),
        legend.position = 'none',
        panel.grid.major.x = element_blank()) +
  facet_rep_wrap(~ Name,
                 repeat.tick.labels = TRUE)

ggsave(plot = last_plot(),
       filename =  paste0(output_directory, '/Applications_by_preference.png'),
       width = 18,
       height = 16.1,
       unit = 'cm',
       bg = 'white',
       dpi = 220)

df %>% 
  select(Financial_year, Name, number_1st_preference_offers, number_2nd_preference_offers, number_3rd_preference_offers) %>% 
  pivot_longer(cols = c('number_1st_preference_offers', 'number_2nd_preference_offers', 'number_3rd_preference_offers'),
               values_to = 'People',
               names_to = 'Preference_rank') %>% 
  mutate(Preference_rank = factor(Preference_rank,
                                  levels = c('number_1st_preference_offers', 'number_2nd_preference_offers', 'number_3rd_preference_offers'))) %>% 
  ggplot(aes(x = Financial_year,
             y = People,
             fill = Preference_rank)) +
  geom_col() +
  scale_fill_manual(values = c('lightblue', '#189bcc', '#de2b51'),
                    labels = c('First choice', 'Second choice', 'Third choice'),
                    name = 'Preference rank') +
  labs(title = 'Number of all offers by preference rank',
       y = '',
       x = '') +
  scale_y_continuous(limits = c(0, 50)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = .5),
        legend.position = 'top',
        panel.grid.major.x = element_blank()) +
  facet_rep_wrap(~ Name,
                 repeat.tick.labels = TRUE)

ggsave(plot = last_plot(),
       filename =  paste0(output_directory, '/Offers_by_preference.png'),
       width = 18,
       height = 16.1,
       unit = 'cm',
       bg = 'white',
       dpi = 220)

df %>% 
  filter(Financial_year == '2024/25') %>% 
  select(Name, Places = total_number_places_offered, Applications_per_place, times_put_as_1st_preference, times_put_as_2nd_preference, times_put_as_3rd_preference)



if(file.exists(paste0(output_directory, '/West_Sussex_LSOA_boundaries_2011.geojson')) != TRUE){
  # IMD data
  IMD_df <- read_csv(url('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv')) %>%
    select(LSOA11CD = `LSOA code (2011)`, LAD = 'Local Authority District name (2019)',  IMD_Decile = "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)") %>%
    mutate(IMD_Decile = case_when(IMD_Decile == 1 ~ 'Decile 1 (most deprived 10%)',
                                  IMD_Decile == 10 ~ 'Decile 10 (least deprived 10%)',
                                  TRUE ~ paste0('Decile ', IMD_Decile))) %>%
    mutate(IMD_Quintile = case_when(IMD_Decile %in% c('Decile 1 (most deprived 10%)', 'Decile 2') ~ 'Quintile 1 (most deprived 20%)', # if value in IMD_Decile is 1 or 2, assign to "Quintile 1" etc
                                    IMD_Decile %in% c('Decile 3', 'Decile 4') ~ 'Quintile 2', # %in% checks if value is present in a vector/list of values
                                    IMD_Decile %in% c('Decile 5', 'Decile 6') ~ 'Quintile 3', #ifelse might be used here  
                                    IMD_Decile %in% c('Decile 7', 'Decile 8') ~ 'Quintile 4',
                                    IMD_Decile %in% c('Decile 9', 'Decile 10 (least deprived 10%)') ~ 'Quintile 5 (least deprived 20%)')) %>%
    filter(LAD %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'))
  
  # LSOA 2011 boundaries for core 20 overlay
  
  lsoa_2011_clipped_sf <- read_sf('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_Dec_2011_Boundaries_Generalised_Clipped_BGC_EW_V3/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>%
    filter(str_detect(LSOA11NM, 'Chichester')) %>%
    select(!c('FID', 'LSOA11NMW')) %>%
    rename('LONG_' = 'LONG')
  
  lsoa_2011_full_sf <- read_sf('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_Dec_2011_Boundaries_Full_Extent_BFE_EW_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>%
    filter(LSOA11CD %in% IMD_df$LSOA11CD) %>%
    filter(!str_detect(LSOA11NM, 'Chichester')) %>%
    select(!c('OBJECTID', 'Shape_Leng'))
  
lsoa_2011_sf <- lsoa_2011_clipped_sf %>%
    rbind(lsoa_2011_full_sf) %>%
  left_join(IMD_df, by = c('LSOA11CD'))
  
  geojson_write(geojson_json(lsoa_2011_sf), file = paste0(output_directory, '/West_Sussex_LSOA_boundaries_2011.geojson'))
  
# Dissolving individual LSOA boundaries
  
  wsx_unioned <- lsoa_2011_full_sf %>%    
    rbind(lsoa_2011_clipped_sf) %>%
    st_buffer(0.5) %>% # make a buffer of half a meter around all parts (to avoid slivers)
    summarise()
  
  geojson_write(geojson_json(wsx_unioned), file = paste0(output_directory, '/West_Sussex_overall_boundary.geojson'))
  
}

lsoa_2011_sf <- read_sf(paste0(output_directory, '/West_Sussex_LSOA_boundaries_2011.geojson'))

wsx_unioned <- read_sf(paste0(output_directory, '/West_Sussex_overall_boundary.geojson'))


df_points <- data.frame(Location = c('Home','Aldingbourne Primary School', 'Barnham Primary School', 'Eastergate CofE Primary School', 'Walberton and Binstead CofE Primary School'),
                        Postcode = c('PO20 3JT', 'PO20 3QR', 'PO22 0HW', 'PO20 3UT', 'BN18 0PH'))

# Eastergate
# Walberton
# Aldingbourne
# Barnham
# Home postcode PO203JT

postcodes_list <- df_points %>%
  pull(Postcode) %>% unique

library(PostcodesioR)

for(i in 1:length(postcodes_list)){

  if(i == 1){
    lookup_result <- data.frame()
  }
  
  if(postcode_validation(postcodes_list[i]) == TRUE){
    lookup_result_x <- postcode_lookup(postcodes_list[i]) %>%
      select(postcode, longitude, latitude, LSOA11CD = lsoa_code, ltla = admin_district)

    lookup_result <- lookup_result_x %>%
      bind_rows(lookup_result)
    
  }
  
}


df_points <- df_points %>% 
  left_join(lookup_result, by = c('Postcode' = 'postcode'))

# leaflet map ####

lsoa_2011_sf <- lsoa_2011_sf %>%
  mutate(IMD_Quintile = factor(IMD_Quintile,
                               levels = c('Quintile 1 (most deprived 20%)',
                                          'Quintile 2',
                                          'Quintile 3',
                                          'Quintile 4',
                                          'Quintile 5 (least deprived 20%)')))

dep_pal <- viridis(5, direction = -1)
dep_pal_1 <- c('#e46c0a', dep_pal[2:5])

dep_leaflet_pal <- colorFactor(dep_pal_1,
                               domain = levels(lsoa_2011_sf$IMD_Quintile))

# imd_leaf <-
  leaflet() %>%
  addControl(paste0("<font size = '2px'><b>West Sussex neighbourhoods; by deprivation quintile;</b><br>Based on the English Index of Multiple Deprivation 2019 (IMD 2019);</font>"),
             position='topright') %>% # change position
  addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png', attribution = '&copy; <a href=[https://www.openstreetmap.org%20/copyright]https://www.openstreetmap.org /copyright>OpenStreetMap</a> contributors &copy; <a href=https://carto.com/attributions>CARTO</a><br>Boundaries reproduced under Open Government Licence v3.0<br>Contains Ordnance Survey data copyright and database right 2024<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons and click on an area to find out more.') %>%
   
  addCircleMarkers(data = subset(df_points, Location != 'Home'),
                     fillColor = 'purple',
                     fillOpacity = 1,
                     color = '#ffffff',
                     opacity = 1,
                     weight = 1,
                     radius = 8,
                     label = ~Location,
                     popup = paste0('<Strong>', df_points$Location, '</Strong><br>', df_points$Postcode),
                   group = 'Show home') %>% 
   addCircleMarkers(data = subset(df_points, Location == 'Home'),
                     fillColor = 'orange',
                     fillOpacity = 1,
                     color = '#ffffff',
                     opacity = 1,
                     weight = 1,
                     radius = 8,
                     label = ~Location,
                     popup = paste0('<Strong>', df_points$Location, '</Strong><br>', df_points$Postcode),
                     group = 'Show schools') %>% 
  addLegend(colors = dep_pal_1,
            labels = levels(lsoa_2011_sf$IMD_Quintile),
            title = 'Deprivation quintile', # title of label
            opacity = 1, # transparency of label
            position = 'bottomright') %>% # position 
  addLegend(position = 'bottomleft',
            colors = c('orange', 'purple'),
            labels = c('Home', 'Schools'),
            title = 'Locations',
            opacity = 1) %>% 
    addLayersControl(overlayGroups = c('Show travel distance', 'Show home', 'Show schools'),
                     options = layersControlOptions(collapsed = FALSE))


library(htmlwidgets)



# save in working directory

saveWidget(imd_leaf, paste0(output_directory, '/IMD_2019_leaflet_map.html'), selfcontained = TRUE)




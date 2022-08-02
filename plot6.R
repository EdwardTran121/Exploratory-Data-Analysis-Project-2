# take library
library(tidyverse)
library(ggrepel)

# get file link
link_url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
zip_file <- 'exdata_data_FNEI_data.zip'

# check zip file existence and download
if(!file.exists(zip_file)){
  download.file(link_url, zip_file, mode = 'wb')
}

# check file existence and unzip
data_path1 <- 'summarySCC_PM25.rds'
data_path2 <- 'Source_Classification_Code.rds'
if(!file.exists(data_path1) & !file.exists(data_path2)){
  unzip(zip_file)
}

# read data
pm25 <- readRDS(file.path(data_path1))
class_code <- readRDS(file.path(data_path2))

# filter vehicle data in SCC
vehicle_code <- class_code[grepl('vehicle', class_code$SCC.Level.Two, ignore.case = TRUE),]

# merge data frame
v_merge_pm25 <- merge(x = pm25, y = vehicle_code, by = 'SCC')

# prepare data for total emission
city_vehicle_pm25 <- v_merge_pm25 %>%
  filter(fips %in% c("24510", "06037")) %>%
  group_by(year, fips) %>% 
  summarise(TotalE = sum(Emissions))

data_ends <- city_vehicle_pm25 %>% 
  filter(year == 2008)
data_ends$fips[data_ends$fips == '24510'] <- 'Baltimore city'
data_ends$fips[data_ends$fips == '06037'] <- 'Los Angeles County'

# make plot
ggplot(city_vehicle_pm25, aes(year, TotalE, color = fips)) +
  xlim(1999, 2010) +
  geom_line() +
  geom_point() +
  labs(title = 'Total PM2.5 Emission from Motor Vehicle Source in 
Baltimore City vs. Los Angeles County',
       subtitle = 'from 1999 to 2008',
       x = 'Year', y = 'Tons') +
  geom_text_repel(aes(label = year), nudge_y = -1, direction = 'y') +
  geom_text_repel(aes(label = fips), color = 'black', data = data_ends, 
                  nudge_x = 1, nudge_y = 0.5, direction = 'y') +
  theme_bw() +
  theme(legend.position = 'none')

# export the plot
dev.copy(png, file = 'plot6.png', height = 480, width = 480)
dev.off()

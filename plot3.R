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

# prepare data for total emission
## the result number of emission is too long -> change unit to thousand tons
tbc_pm25 <- pm25 %>% 
  filter(fips == 24510) %>% 
  group_by(year, type) %>% 
  summarise(TotalE = sum(Emissions))

data_ends <- tbc_pm25 %>% 
  filter(year == 2008) %>% 
  mutate(type = str_to_title(type))

# make plot
ggplot(tbc_pm25, aes(year, TotalE, col = type)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = type), color = 'black', data = data_ends, nudge_x = 1) +
  xlim(1999,2010) +
  labs(title = 'Total PM2.5 Emission by Source Type in Baltimore City',
       subtitle = 'from 1999 to 2008',
       x = 'Year', y = 'Tons') +
  theme_bw() +
  theme(legend.position = 'none')

# export the plot
dev.copy(png, file = 'plot3.png', height = 480, width = 480)
dev.off()

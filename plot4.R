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

# filter coal data in SCC
coal_code <- class_code[grepl('comb', class_code$SCC.Level.One, ignore.case = TRUE),]
coal_code <- class_code[grepl('coal', class_code$SCC.Level.Four, ignore.case = TRUE),]

# merge data frame
merge_pm25 <- merge(x = pm25, y = coal_code, by = 'SCC')

# prepare data for total emission
tecoal_pm25 <- merge_pm25 %>%
  group_by(year) %>% 
  summarise(TotalE = sum(Emissions))

# make plot
ggplot(tecoal_pm25, aes(year, TotalE)) +
  geom_line() +
  geom_point() +
  labs(title = 'Total PM2.5 Emission from Coal Combustion-related Source 
in the United States',
       subtitle = 'from 1999 to 2008',
       x = 'Year', y = 'Tons') +
  geom_text_repel(aes(label = year), nudge_y = -1, direction = 'y') +
  theme_bw()

# export the plot
dev.copy(png, file = 'plot4.png', height = 480, width = 480)
dev.off()

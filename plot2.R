# take library
library(tidyverse)

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
bc_pm25 <- pm25 %>% 
  mutate(Emissions_ktons = Emissions/1000) %>% 
  filter(fips == 24510) %>% 
  group_by(year) %>% 
  summarise(TotalE = sum(Emissions_ktons))

# make plot
with(bc_pm25, plot(year, TotalE, type = 'l', xlab ='Year', ylim = c(1.7,3.3),
                   ylab = 'Thousand tons', 
                   main = 'Total PM2.5 Emission in the Baltimore City from 1999 to 2008'))
with(bc_pm25, points(year, TotalE, pch = 16))
with(bc_pm25, text(year, TotalE, labels = year, pos = 1))

# export the plot
dev.copy(png, file = 'plot2.png', height = 480, width = 480)
dev.off()

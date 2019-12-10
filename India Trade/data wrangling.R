#install.packages("tidyverse")
#install.packages("skimr")
#install.packages("dplyr")
#install.packages("scales")
#install.packages("IRdisplay")
#install.packages("shiny")
#install.packages("RColorBrewer")
#install.packages("ggplot2")
#install.packages("reshape")
#install.packages("plotly")
#install.packages("treemapify")
#install.packages("wesanderson")
library(wesanderson)
library(tidyverse)
library(ggplot2)
library(readr)
library(skimr)
library(dplyr)
library(reshape)
library(scales)
library(IRdisplay)
library(RColorBrewer)
library(treemapify)

export <- read_csv("./India-Trade/data/2018-2010_export.csv")
import <- read_csv("./India-Trade/data/2018-2010_import.csv")
map_data_export <- read_csv("./India-Trade/data/CHLOROPLETH_DATA_EXPORT.csv")
map_data_import <- read_csv("./India-Trade/data/CHLOROPLETH_DATA_IMPORT.csv")

export %>%
  filter(!is.na(value)) %>%
  skim()

export_new <- export %>%
  filter(!is.na(value))

import %>%
  filter(!is.na(value)) %>%
  skim()

import_new <- import %>%
  filter(!is.na(value))

# Total Number of Commodities per Country
Tot_Comm_per_Country <- export_new %>%
  count(country, Commodity) %>%
  count(country, sort = TRUE)
Tot_Comm_per_Country 

# Total Number of Commodities per Year per Country
Tot_Comm_per_Year <- export_new %>%
  count(year, country, value) %>%
  count(year, sort = TRUE)
Tot_Comm_per_Year 

# Highest Export Value by Commodity
export_highest_commodity <- export_new %>%
  group_by(HSCode) %>%
  summarise(Total = sum(value)) %>%
  arrange(desc(Total))
export_highest_commodity <- export_highest_commodity[1:10,]

#Total Exports
Total_Exports <- export_new %>%
  summarise(Total = sum(value))

#Total Imports
Total_Imports <- import_new %>%
  summarise(Total = sum(value))

#Overall Profit
Overall_Profit <- Total_Exports - Total_Imports

# Highest Export Value by Country 
export_highest_country <- export_new %>%
  group_by(country) %>%
  summarise(Total = sum(value)) %>%
  arrange(desc(Total))
export_highest_country <- export_highest_country[1:10,]


# Highest Import Value by Commodity
import_highest_commodity <- import_new %>%
  group_by(HSCode) %>%
  summarise(Total = sum(value)) %>%
  arrange(desc(Total))
import_highest_commodity <- import_highest_commodity[1:10,]

# Highest Import Value by Country 
import_highest_country <- import_new %>%
  group_by(country) %>%
  summarise(Total = sum(value)) %>%
  arrange(desc(Total))
import_highest_country <- import_highest_country[1:10,]

# Highest Export Value by Year
export_highest_year <- export_new %>%
  group_by(year) %>%
  summarise(Total = sum(value)) %>%
  mutate(Trade = "Exports")%>%
  arrange(desc(Total))
export_highest_year <- export_highest_year[1:9,]

# Highest Import Value by Year 
import_highest_year <- import_new %>%
  group_by(year) %>%
  summarise(Total = sum(value)) %>%
  mutate(Trade = "Imports")%>%
  arrange(desc(Total))
import_highest_year <- import_highest_year[1:9,]

# Highest Export Value by Year by Country 
export_highest_year_country <- export_new %>%
  group_by(year, country) %>%
  summarise(Total = sum(value)) %>%
  mutate(Trade = "Exports")%>%
  arrange(desc(Trade))
export_highest_year_country <- export_highest_year_country[1:9,]

# Highest Import Value by Year by Country 
import_highest_year_country <- import_new %>%
  group_by(year, country) %>%
  summarise(Total = sum(value)) %>%
  mutate(Trade = "Imports")
import_highest_year_country <- import_highest_year_country[1:9,]

# Highest Import Value for 2018 - China
import_highest_2018 <- import_new %>%
  filter(year == 2018)%>%
  group_by(country) %>%
  summarise(Total = sum(value)) %>%
  arrange(desc(Total))

# Highest Export/Import Value by Year
export_import_year <- merge(export_highest_year, import_highest_year, by = "year") 

# Highest Export/Import Value by Year by Country
export_import <- rbind(export_highest_year, import_highest_year) 

#Tree Import
import$value[is.na(import$value)] = 0.0
df_import = import %>%
  group_by(country) %>%
  summarise(sum(value))

df_import %>% arrange(-`sum(value)`)

#Tree Export
export$value[is.na(export$value)] = 0.0
df_export = export %>%
  group_by(country) %>%
  summarise(sum(value))


df_export %>% arrange(-`sum(value)`)

#HS Code Export
# topexport = export %>%
#   filter(HSCode %in% (export %>% count(HSCode) %>% 
#                         arrange(-n) %>% 
#                         pull(HSCode)))
# topexport1 = with(topexport,table(year,HSCode))
# topexport1 = as.data.frame(topexport1)
# 
# 
# #HS Code Import
# topimport = import %>%
#   filter(HSCode %in% (import %>% count(HSCode) %>% 
#                         arrange(-n) %>% 
#                         pull(HSCode)))
# topimport1 = with(topimport,table(year,HSCode))
# topimport1 = as.data.frame(topimport1)

#Bubble Export
import$value[is.na(import$value)] = 0.0
export$value[is.na(export$value)] = 0.0

topcountryexp = export %>%
  group_by(country) %>%
  summarise(sum(value))

topcountryexp %>% arrange(-`sum(value)`)

#Bubble Import
import$value[is.na(import$value)] = 0.0
export$value[is.na(export$value)] = 0.0

topcountryimp = import %>%
  group_by(country) %>%
  summarise(sum(value))
topcountryimp %>% arrange(-`sum(value)`)


# Export Value by Commodity
export_comm <- export_new %>%
  group_by(HSCode) %>%
  summarise(Total = sum(value)) %>%
  mutate(Trade = "Exports")%>%
  arrange(desc(Total))
export_comm <- export_comm[1:9,]

# Import Value by Commodity
import_comm <- import_new %>%
  group_by(HSCode) %>%
  summarise(Total = sum(value)) %>%
  mutate(Trade = "Imports")%>%
  arrange(desc(Total))

import_comm <- import_comm[1:9,]

# Highest Export/Import Value by Year by Commodity
export_import_comm <- rbind(export_comm, import_comm) 

# Export Value by Country
export_coun <- export_new %>%
  group_by(country) %>%
  summarise(Total = sum(value)) %>%
  mutate(Trade = "Exports")%>%
  arrange(desc(Total))
export_coun <- export_coun[1:9,]

# Import Value by Country
import_coun <- import_new %>%
  group_by(country) %>%
  summarise(Total = sum(value)) %>%
  mutate(Trade = "Imports")%>%
  arrange(desc(Total))
import_coun <- import_coun[1:9,]

# Export/Import Value by Country
export_import_coun <- rbind(export_coun, import_coun) 


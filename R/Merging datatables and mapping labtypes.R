# 'Skrypt Tabela 2.R'

# Dependencies ------------------------------------------------------------

library(tidyverse)
library(xlsx)
library(unpivotr)
library(tidyxl)
library(rlang)
library(ggplot2)
library(lubridate)

#---------------------------------------------------------------------------------
#Setup
sheet_index <- 1L
path <- './data/clininet_sample.xlsx'

# gathers all informations about cells in the data table
# and doesn't import blank cells
data_raw_cells <-
  xlsx_cells('C:/Program Files/RStudio/R/Data/clininet_sample.xlsx', sheet_index) %>%
  filter(!is_blank)

# gather info about all cell formating
cell_format <- xlsx_formats('C:/Program Files/RStudio/R/Data/clininet_sample.xlsx', sheet_index)

# specifies what kind of format element 'bold' will represent
bold <- cell_format$local$font$bold

# sets up headers to make a clear table with variables and observations on correct place | directions:
# "NNW", "N", "NNE", "ENE", "E", "ESE", "SSE", "S", "SSW", "WSW", "W", "WNW"
data_tidy <-
  data_raw_cells %>%
  behead('NNW', 'title') %>%
  behead('NNW', 'id') %>%
  behead('NNW', 'doctors_name') %>%
  #changed panel to labtype_raw so merge() can work
  behead('W',   'labtype_raw') %>%
  behead('W',   'empty') %>%
  behead('W',   'date1') %>%
  behead('W',   'date2') %>%
  behead('W',   'order_id') %>%
  behead('W',   'test') %>%
  behead('W',   'value') %>%
  behead('W',   'norm') %>%
  behead_if(bold[local_format_id], direction = 'W', name = 'unit2') %>%
  select(title, id, order_id, doctors_name, date1,
         date2, labtype_raw, test, norm, unit = character, value)

labtests_mapping <- read.xlsx('C:/Program Files/RStudio/R/Data/meta_clininet.xlsx', sheet_index)

# merging datatables
data_tidy_mapped <-
  merge(data_tidy, labtests_mapping, by = 'labtype_raw')

# plots


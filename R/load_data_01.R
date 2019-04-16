# 'Skrypt Tabela 2.R'

# Dependencies ------------------------------------------------------------

library(tidyverse)
library(xlsx)
library(tidyverse)
library(unpivotr)
library(tidyxl)
library(rlang)
library(ggplot2)
library(lubridate)

# Setup -------------------------------------------------------------------

sheet_index <- 1L
path <- './data/clininet_sample.xlsx'

# gathers all informations about cells in the data table
# and doesn't import blank cells
data_raw_cells <-
  xlsx_cells(path, sheet_index) %>%
  filter(!is_blank)

# gather info about all cell formating
cell_format <- xlsx_formats(path, sheet_index)

# specifies what kind of format element 'bold' will represent
bold <- cell_format$local$font$bold

# sets up headers to make a clear table with variables and observations on correct place | directions:
# "NNW", "N", "NNE", "ENE", "E", "ESE", "SSE", "S", "SSW", "WSW", "W", "WNW"
data_tidy <-
  data_raw_cells %>%
  behead('NNW', 'title') %>%
  behead('NNW', 'id') %>%
  behead('NNW', 'doctors_name') %>%
  behead('W',   'panel') %>%
  behead('W',   'empty') %>%
  behead('W',   'date1') %>%
  behead('W',   'date2') %>%
  behead('W',   'order_id') %>%
  behead('W',   'test') %>%
  behead('W',   'value') %>%
  behead('W',   'norm') %>%
  behead_if(bold[local_format_id], direction = 'W', name = 'unit2') %>%
  select(title, id, order_id, doctors_name, date1,
         date2, panel, test, norm, unit = character, value)

data_tidy <- data_tidy %>%
  mutate(value_raw = value,
         value = as.numeric(value))

# wartości nie-numeryczne:
#   najczęściej opisowe (np badanie moczu)
#   rzadziej cenzorowane (rozdzielczość aparatu, np CRP < 0.3)
# w drugim przypadku można pomyśleć o interpretacji jako 0
data_tidy %>%
  filter(are_na(value), !are_na(value_raw)) %>%
  distinct(value_raw, .keep_all = TRUE) %>%
  select(panel, value_raw)

data_tidy <- data_tidy %>%
  mutate(value = if_else(value_raw == '<0.30' & panel == 'Białko C-reaktywne (CRP)',
                         0, value))

# Plots -------------------------------------------------------------------

dd <- data_tidy %>%
  mutate(day = as_datetime(date1) %>% date()) %>%
  group_by(day, test) %>%
  summarise(value = mean(value, na.rm = TRUE))

dd %>%
  group_by(test) %>%
  count(sort = TRUE) %>%
  View()

ddw <- dd %>%
  spread(test, value)

ggplot(dd %>%
         filter(test %in% c('ALT', 'Bilirubina całkowita')), 
       aes(x = day, y = value, lty = test)) +
  geom_line() + 
  facet_grid(test ~ ., scales = 'free') +
  theme_bw()

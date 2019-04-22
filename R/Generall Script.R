# Generall Script

# Dependencies ------------------------------------------------------------

library(xlsx)
library(tidyverse)
library(unpivotr)
library(tidyxl)
library(rlang)
library(ggplot2)
library(lubridate)

# Setup -------------------------------------------------------------------

data_path <- './data/clininet_sample.xlsx'
data_sheet_index <- 1L
meta_path <- './data/meta_clininet.xlsx'
meta_sheet_index <- 2L



# Load --------------------------------------------------------------------

# Gathers all informations about cells in the data table
# And doesn't import blank cells
data_raw_cells <-
  xlsx_cells(data_path, data_sheet_index) %>%
  filter(!is_blank)

# Gather info about all cell formating
cell_format <- xlsx_formats(data_path, data_sheet_index)

# specifies what kind of format element 'bold' will be representing
bold <- cell_format$local$font$bold

# Sets up headers to make a clear table with variables and observations on 
# correct place | directions:
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
         date2, panel, test, norm, unit = character, value) %>%
  print(n = 10)

# Creates a new column called value_raw which has values taken from value col
# in addition value col changes its nonnumeric values into NA
data_tidy <- data_tidy %>%
  mutate(value_raw = value, value = as.numeric(value)) %>%
  print(n = 10)

# Ponizsze dotyczy wszystkiego do momentu zaczecia dzialu z example plots
# Wartoœci nie-numeryczne:
#   najczêœciej opisowe (np badanie moczu)
#   rzadziej cenzorowane (rozdzielczoœæ aparatu, np CRP < 0.3)
# W drugim przypadku mo¿na pomyœleæ o interpretacji jako 0

# Filters data for rows that include non NA values in value col and NA valuees in value_raw col
# If thats done destinct removes duplicated rows based on value_raw col which were before filtered
# (unsure??) After that you can use select to display cases/rows which are true with filter() statement 
data_tidy %>%
  filter(are_na(value), !are_na(value_raw)) %>%
  distinct(value_raw, .keep_all = TRUE) %>%
  select(panel, value_raw) %>%
  print(n = 33)

# Changes results of a labtests for CRP which values are below 0.3 to 0 in value column
# For rows where case from value_raw col is less than 0.3
data_tidy <- data_tidy %>%
  mutate(value = if_else(value_raw == '<0.30' & panel == 'Bia³ko C-reaktywne (CRP)', 0, value))

# Example Plots given by the @mjktfw --------------------------------------

dd <- data_tidy %>%
  # Creates a day column which cases are the dates of running a specified labtest
  # as_datetime formats date1 cases to date format 
  # and %>% date() drops time stamp so only yy/mm/dd is being kept
  mutate(day = as_datetime(date1) %>% 
           date()) %>%
  # sorts by date for day column and sorts by alphabetical order test column
  group_by(day, test) %>%
  # creates another column called value which represents 
  # mean of the specified labtest values in the whole day in addition it skips NA values to prevent
  # Errors as you cant calculate mean using NA values
  summarise(value = mean(value, na.rm = TRUE))

# This doesnt save just prints output
# Sorts a col test by alphabetical order
# Counts how many times in generall a specified labtest has been made
dd %>%
  group_by(test) %>%
  count(sort = TRUE) %>%
  View()

# Creates a sheet named ddw from dd data
# generally speaking - spreads values (value) accross the sheat, col names are labtests;
# first col is the date and both are from the dd data
# its more clear on ?spread()
ddw <- dd %>%
  spread(test, value)

# Creates a plot using dd data for ALT and Bilirubina ca³kowita and their mean labtests values
# X axis is for date of a labtest and Y axis is for its whole day mean; lty is a caption
# geom_line is needed to visualize how values are changing
# facet_grid splits a plot into two specified for ALT and Bilb[...]
# theme_bw specifies about parameters of the plot; look for ggtheme on google 
ggplot(dd %>%
         filter(test %in% c('ALT', 'Bilirubina ca³kowita')),
       aes(x = day, y = value, lty = test)) +
  geom_line() +
  facet_grid(test ~ ., scales = 'free') +
  theme_bw()

# Join meta ---------------------------------------------------------------

labtests_mapping <- readxl::read_xlsx(meta_path, meta_sheet_index)

# @mkjtfw

data_tidy_mapped <- data_tidy %>%
  #adds 2 new col with labelled labtype names
  dplyr::left_join(labtests_mapping %>% 
                     # select() selects col names from labtests_mapping that you want to use to labell
                     # the names of labtpyes
                     select(labtype_raw, labtest_raw, 
                            # this part gives names to freshly born column names in the data_tidy dataset
                            labtype, labtest),
                   #specifies by which col names is the left_join supposed to work
                   by = c('panel' = 'labtype_raw',
                          'test' = 'labtest_raw'))

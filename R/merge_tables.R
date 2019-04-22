#  Merging datatables and mapping labtypes
# 'Skrypt Tabela 2.R'

# Dependencies ------------------------------------------------------------

library(tidyverse)
library(xlsx)
library(unpivotr)
library(tidyxl)
library(rlang)
library(ggplot2)
library(lubridate)
library(readxl)

# Setup -------------------------------------------------------------------

sheet_index <- 1L
data_path <- './data/clininet_sample.xlsx'
meta_path <- './data/meta_clininet.xlsx'

# Load --------------------------------------------------------------------

# gathers all informations about cells in the data table
# and doesn't import blank cells
data_raw_cells <-
  xlsx_cells(data_path, sheet_index) %>%
  filter(!is_blank)

# gather info about all cell formating
cell_format <- xlsx_formats(data_path, sheet_index)

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

data_tidy <- data_tidy %>%
  mutate(value_raw = value,
         value = as.numeric(value))

# wartości nie-numeryczne:
#   najczęściej opisowe (np badanie moczu)
#   rzadziej cenzorowane (rozdzielczość aparatu, np CRP < 0.3)
#   w drugim przypadku można pomyśleć o interpretacji jako 0
data_tidy %>%
  filter(are_na(value), !are_na(value_raw)) %>%
  distinct(value_raw, .keep_all = TRUE) %>%
  select(labtype_raw, value_raw)

data_tidy <- data_tidy %>%
  mutate(value = if_else(value_raw == '<0.30' & labtype_raw == 'Białko C-reaktywne (CRP)',
                         0, value))



# Join meta ---------------------------------------------------------------

# do wczytywania wolę korzystać z readxl 
# TODO: projekty trzeba trzymać w katalogach niezależnie od IDE, czy tam
# narzędzi, z których korzystasz
# czasami trzyma się razem z bibliotekami, z którymi korzystasz, ale to już przy
# większych projektach/wersjonowaniu itd.
# ewentualnie dane można trzymać poza katalogiem z projektami - ułatwia to
# współdzielenie danych między projektami, zabezpiecza przed przypadkowym
# upublicznieniem wrażliwych informacji
# ja co prawda trzymam w katalogu projektu, ale wtedy trzeba pamiętać, żeby np.
# w .gitignore dać odpowiednie reguły, żeby się nie synchronizowały na serwer
# u mnie struktura przykładowe projektu wygląda +- tak:
# /projects/ - katalog z projektami
# /projects/project_01 - katalog główny projektu ('working dir', getwd())
# ./R, ./src, ./data, ./bin, ... - podkatalogi z kodem R, innym kodem, danymi,
# binarkami, co tam ci potrzebne do projektu
# to wszystko po to, żeby możliwie często korzystać w projekcie ze ścieżek
# względnych, a nie absolutnych
labtests_mapping <- readxl::read_xlsx(meta_path, 2)

# merging datatables
# TODO: zobacz helpa ?left_join - to są funkcje z dplyr, do różnych typów
# joinów. Merge jest spoko, ale stary, jak się decydujesz na tidyverse, to
# lepiej korzystać z nowszych funkcji z tych pakietów. Co do samych danych -
# lepiej joinować jednocześnie po kolumnach z labtype i test - bo może być tak,
# że dana nazwa testu jest z jakiegoś innego panelu (np surowica/mocz)
data_tidy_mapped <- data_tidy %>%
  dplyr::left_join(labtests_mapping %>% 
                     select(labtype_raw, labtest_raw,
                            labtype = labtype2, labtest),
                   by = c('labtype_raw',
                          'test' = 'labtest_raw'))

# loding multiple tables from one sheet

# „If that is too much work, 
# then @joran's suggestion will probably work. You could use stringr::str_detect() to identify when 
# a column or row is empty or 
# not empty and then plug that information into the startRow, startCol, endRow, endCol parameters.”

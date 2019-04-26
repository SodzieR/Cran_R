# RR_ggplot

###TO DO
# ~Tidy the data~


# Dependencies ------------------------------------------------------------

library(dplyr)
library(tibble)

# Setup -------------------------------------------------------------------

data_path <- './data/01_2017_12_08_16_28_00.txt'

# Load --------------------------------------------------------------------

data <- as_tibble(read.csv(data_path, header = FALSE, sep = '\t'))

patients_ID <- '01'
lab_start_date <- '2017/08/16'
lab_start_time <- '16:28:00'

data <- data %>%
  rename(time_stamp = V1, flag_type = V2)

info <- data[1,1]
info <- pull(info)

data <- data[-1,]

data <- data %>%
  add_column(patients_ID, info, lab_start_date, lab_start_time, .before = 1) %>%
  print(n = 10)

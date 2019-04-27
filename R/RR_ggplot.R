# RR_ggplot

# TO DO:
# Clean the data                                    - DONE
# policzyć ile trwały poszczególne odstępy RR 
# (odległości między sąsiednimi zespołami R)        - DONE
# na tej podstawie chwilową czynność serca,         - DONE - element TCA
#                                                 (temporary cardiac activity)
# potem średnią czynność serca dla: 
#   minut,                                          - DONE - TCA_M
#   godzin,                                         - DONE - TCA_h
#   pory dnia (dzień/noc - przyjmij 22:00 - 07:00)  - DONE - TCA_day
#   i całego zapisu                                 - DONE - TCA_allTime
# wszystko przedstawić na wykresie od czasu         - NOT DONE YET
#  podobny wykres dla odsetka pobudzeń komorowych   _ TO-DO
# przedstawić na wykresie zmienność odstępów RR 
# w zależności od typu pobudzenia.                  - TO-DO

# Dependencies ------------------------------------------------------------

library(dplyr)
library(tibble)
library(ggplot2)

# Setup -------------------------------------------------------------------

data_path <- './data/01_2017_12_08_16_28_00.txt'

# Load --------------------------------------------------------------------

# Tidying the data

data <- as_tibble(read.csv(data_path, header = FALSE, sep = '\t'))

patients_ID <- '01'
lab_start_date <- '2017/08/16'
lab_start_time <- '16:28:00'

# Just to clean the data
data <- data %>%
  rename(time_stamp = V1, flag_type = V2)

info <- data[1,1]
# Pulls the value from the tibble
info <- pull(info)

data <- data[-1,]

data <- data %>%
  add_column(patients_ID, info, lab_start_date, lab_start_time, .before = 1) %>%
  print(n = 10)

# Format lab_start_date to date format
data$lab_start_date <- as.Date(data$lab_start_date)

# Generates correct integer units
data$time_stamp <- as.numeric(sub(",", ".", data$time_stamp, fixed = TRUE))

# ---------------------------------------------------------------------------

# RR space between each flag type

# Calculates differences between each row value, expect the '0' value which
# added later
RR_spaces_missing <- diff(data$time_stamp)

# Gets diff() value for the first time_stamp value
first_ts <- data$time_stamp[1]

# Adds first diff() for first row as the RR_spaces is one value too short
RR_spaces_correct <- append(first_ts, RR_spaces_missing)

# Creates new column which will mean spaces between each RR type
data$RR_space_time <- RR_spaces_correct

# ---------------------------------------------------------------------------

# Time elements

# Calculates for how many minutes was the lab taking place
time_seconds <- max(data$time_stamp)

time_minutes <- time_seconds / 60L

time_hours <- time_minutes / 60L

# Knowing the fact the the lab started at 16:28:00 and that it was taking place
# for ~50 hours we can state that the lab was taking place during 
# two full night time periods - assuming that the night is time between
# 22:00 - 07:00 hours we can make a statemant that the lab was taking place
# for 9 hours x2 during the night so 18 hours during the night in generall

time_night <- 18L

# Day time in hours
time_day <- time_hours - time_night

# Day time in seconds
time_day_s <- time_day * 3600

# Cardiac Activity

time_normal <- sum(data$RR_space_time[data$flag_type == 'N'])

time_normal_ammount <- length(time_normal)

# Temporary cardiac activity value 

TCA <- 1/data$RR_space_time
data$TCA <- TCA

# Calculating average TCA for hours/day time/whole lab time (seconds)
# Adding them to the tibble too

TCA_allTime <-  sum(TCA) / time_normal_ammount 
TCA_day <- TCA_allTime /  time_day
TCA_h <- TCA_allTime / time_hours
TCA_m <- TCA_allTime / time_minutes

data$TCA_allTime <- TCA_allTime
data$TCA_day <- TCA_day
data$TCA_h <- TCA_h
data$TCA_m <- TCA_m

# ---------------------------------------------------------------------------

# Plots

# Not quite done yet

time_stamp_plot <- data$time_stamp

ggplot(data = data, aes(x = time_stamp, y = TCA)) +
  geom_line() +
  theme_bw()

plot(data$time_stamp, data$TCA)

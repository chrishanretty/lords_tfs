library(tidyverse)

all_acts <- read.csv("/mnt/003b6650-3320-4dbb-9c9e-ff84cfef882f/home/chris/Dropbox/tfs/wp5_lords/data/all_bills_passed_2000_to_2023.csv") |>
    distinct(bill) |>
    pull(bill)

### Check against 2nd readings
second <- read.csv("second_readings_checked.csv")

missings <- setdiff(all_acts, second$leg_title)

cat(sort(missings), sep = "\n")

third <- read.csv("third_readings_checked.csv")
missings <- setdiff(all_acts, third$leg_title)

# Data Manipulation


# Load Packages
library(tidyverse)
library(readxl)

# Change to match that of file name
FedEx_file <- "Copy of FedEx_ShipmentDetail_754311843_754373326.xlsx"
AFF_file <- "dakine...xlsx"
warehouse_file <- "warehouse_orders_complete.csv"





# Read in the datasets
AFF <- read_xlsx(str_c("ungrouped_data/", AFF_file)) %>% 
  janitor::clean_names()

FedEx <- read_xslx(str_c("ungrouped_data/", FedEx_file)) %>% 
  janitor::clean_names() %>% 
  filter(recipient_state_province == "HI")

warehouse <- read_csv(str_c("ungrouped_data/", warehouse_file)) %>% 
  janitor::clean_names()





warehouse_final <- warehouse %>%
  filter(invoicedate > "2021-1-1", invoicedate < "2022-01-01") %>% 
  mutate(volume = master_volume_cbm * quantity,
         weight = master_weight_kgs * quantity) %>% 
  rename(date = invoicedate) %>% 
  select(date, volume, weight)


AFF_final <- AFF %>% 
  select(date_in_out, cube, weight) %>% 
  rename(date = date_in_out, volume = cube) %>% 
  mutate(volume = volume / 35.315) %>% 
  filter(date > "2021-1-1", date < "2022-01-01")


FedEx_final <- FedEx %>% 
  select(shipment_date_mm_dd_yyyy, dimmed_height_in, dimmed_width_in, dimmed_length_in, original_weight_pounds) %>% 
  mutate(volume = dimmed_height_in * dimmed_length_in * dimmed_width_in / 61024) %>% 
  mutate(date = as.Date(shipment_date_mm_dd_yyyy, format = "%m/%d/%Y")) %>% 
  filter(date > "2021-1-1", date < "2022-01-01") %>% 
  mutate(weight = original_weight_pounds / 2.205) %>% 
  select(date, volume, weight) %>% 
  filter(volume != 0)



data_compiled <- warehouse_final %>% 
  rbind(AFF_final) %>% 
  rbind(FedEx_final) %>% 
  mutate(day_of_year = yday(date)) %>% 
  arrange(day_of_year) %>% 
  select(-c(date)) %>% 
  mutate(volume = volume * 35.315)

write_csv(data_compiled, "grouped_data/modelling_data.csv")

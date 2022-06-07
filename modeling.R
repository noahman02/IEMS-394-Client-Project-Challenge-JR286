##Install Packages (only if running for first time, otherwise remove)
# install.packages("tidyverse")
# install.packages("tidymodels")
# install.packages("readxl")


# Load Packages
library(tidyverse)
library(tidymodels)
library(readxl)


set.seed(1234)



### Please note that the two models can take up to 5 hours to run. The output file 
### is already included in the /ungrouped_data subdirectory for reference

# Change to match names of files
specification_name <- "DAK_Item_Specification_WF_02162022155825.xlsx"
order_name <- "20220418_2021_Hawaii_Shipments.xlsx"




# Read in spreadsheets
items_master <- read_xlsx(str_c("modelling_data/", specification_name)) %>% 
  janitor::clean_names()

orders <- read_xlsx(str_c("modelling_data/", order_name)) %>% 
  janitor::clean_names()


# Aggregate occurrences by SKU
sku_groups <- orders %>% 
  group_by(item_number) %>% 
  summarize(quantity = sum(quantity_received),
            unit_price = unit_price)


# Combine SKU information with order information
items <- items_master %>% 
  filter(jr_category != "OTHER") %>% 
  select(jr_category, jr_product_line, brand_category, sku_number, 
         size_code, master_volume_cbm, master_weight_kgs) %>% 
  left_join(sku_groups, by = c("sku_number" = "item_number")) %>% 
  filter(!is.na(quantity)) %>% 
  mutate(code = str_c(jr_category, jr_product_line, brand_category))


# Split data
items_test <- items %>% filter(is.na(master_volume_cbm))
items_train <- items %>% filter(!is.na(master_volume_cbm))

items_folds <- vfold_cv(items_train, v = 2, repeats = 2)

items_recipe <- recipe(master_volume_cbm ~ code + quantity + unit_price + jr_category +
                         jr_product_line + brand_category,
                      data = items_train) %>%
  step_other(all_nominal_predictors(), threshold = 0.008) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_interact(terms = ~ starts_with("code"):quantity +
                  starts_with("code"):unit_price +
                  unit_price:quantity + 
                  starts_with("jr_category"):unit_price) %>%
  step_normalize(all_numeric_predictors()) 

# Bake data to see # of columns
items_bake <- items_recipe %>% 
  prep() %>% 
  bake(new_data = items_train)


# Create Models
rf_model <- rand_forest(mode = "regression",
                        min_n = tune(),
                        mtry = tune(),
                        trees = tune()) %>% 
  set_engine("ranger")


# Set Parameters
rf_params <- parameters(rf_model) %>% 
  update(mtry = mtry(c(1, 44)))

rf_grid <- grid_regular(rf_params, levels = 5)



# Create Workflows
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(items_recipe)

# Tuning (skip for time)
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

rf_tuned <- rf_workflow %>% 
  tune_grid(items_folds, grid = rf_grid)


# Find results
rf_workflow_tuned <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tuned, metric = "rmse"))

rf_results <- fit(rf_workflow_tuned, items_train)




# Make Prediction
items_metric <- metric_set(rmse)

predicted_values <- predict(rf_results, new_data = items_test) %>% 
  bind_cols(items_test %>% select(sku_number)) 

items_bound <- items %>% left_join(predicted_values) %>% 
  mutate(master_volume_cbm = ifelse(is.na(master_volume_cbm), .pred, master_volume_cbm)) %>% 
  select(-c(.pred))







## Modelling Weights

# Split data
weight_test <- items %>% filter(is.na(master_weight_kgs))
weight_train <- items %>% filter(!is.na(master_weight_kgs))

weight_folds <- vfold_cv(weight_train, v = 2, repeats = 2)

weight_recipe <- recipe(master_weight_kgs ~ code + quantity + unit_price + jr_category +
                         jr_product_line + brand_category,
                       data = items_train) %>%
  step_other(all_nominal_predictors(), threshold = 0.008) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_interact(terms = ~ starts_with("code"):quantity +
                  starts_with("code"):unit_price +
                  unit_price:quantity + 
                  starts_with("jr_category"):unit_price) %>%
  step_normalize(all_numeric_predictors()) 

# Bake data to see # of columns
weight_bake <- weight_recipe %>% 
  prep() %>% 
  bake(new_data = weight_train)


# Create Models
rfw_model <- rand_forest(mode = "regression",
                        min_n = tune(),
                        mtry = tune(),
                        trees = tune()) %>% 
  set_engine("ranger")


# Set Parameters
rfw_params <- parameters(rfw_model) %>% 
  update(mtry = mtry(c(1, 44)))

rfw_grid <- grid_regular(rfw_params, levels = 5)



# Create Workflows
rfw_workflow <- workflow() %>% 
  add_model(rfw_model) %>% 
  add_recipe(weight_recipe)

# Tuning (skip for time)
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

rfw_tuned <- rfw_workflow %>% 
  tune_grid(weight_folds, grid = rfw_grid)

write_rds(rfw_tuned, "Data/rfw_tuned.rds")

rfw_tuned <- read_rds("Data/rfw_tuned.rds")


# Collect metrics
show_best(rfw_tuned, metric = "rmse")

# Find results
rfw_workflow_tuned <- rfw_workflow %>% 
  finalize_workflow(select_best(rfw_tuned, metric = "rmse"))

rfw_results <- fit(rfw_workflow_tuned, weight_train)




# Make Prediction
weight_metric <- metric_set(rmse)

predicted_values_w <- predict(rfw_results, new_data = weight_test) %>% 
  bind_cols(weight_test %>% select(sku_number)) 

final_skus <- items_bound %>% left_join(predicted_values_w) %>% 
  mutate(master_weight_kgs = ifelse(is.na(master_weight_kgs), .pred, master_weight_kgs)) %>% 
  select(sku_number, master_weight_kgs, master_volume_cbm, quantity)

final_skus %>% group_by(sku_number) %>% summarize(times_ordered = n(),
                                                  amount_ordered = sum(quantity)) %>% view()

final_output <- orders %>% select(-c(master_volume_cbm)) %>% 
  left_join(final_skus, by = c("item_number" = "sku_number")) %>% 
  filter(!is.na(master_volume_cbm)) %>% 
  filter(!is.na(master_weight_kgs)) %>% unique()


write_csv(final_output, file = "ungrouped_data/warehouse_orders_complete.csv")



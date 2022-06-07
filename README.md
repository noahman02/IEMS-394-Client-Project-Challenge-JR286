# IEMS-394-Client-Project-Challenge-JR286


This directory contains two R scripts: 'modelling.R', and 'grouping_data.R'. Instructions are as follows

modelling.R: this script runs the Random Forest model used to impute the missing volumes and weights of the items shipped from the Hawaiian Distribution Center. It pulls information from the '/modelling_data' subdirectory and outputs a cleaned file called 'warehouse_orders_cleaned.csv' into the '/ungrouped_data' subdirectory. To run the model, simply put the dataset of the orders from the Hawaiian Distribution Center into the '/modelling_data', along with the SKU specification dataset. Make sure the strings in the early section of the R script match the names of the datasets as provided. Please note that running this script can take upwards of 5 hours. For your convenience, the output has already been created.


The second script, 'grouping_data.R', is used to compile a cleaned data source for our Simio model from the various datasets provided from JR286. This takes in datasets from the '/ungrouped_data' subdirectory and outputs a cleaned dataset called 'modelling_data.csv' that can be used in the Simio model. Make sure the strings in the early section of the R script match the names of the datasets as provided.

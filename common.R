# set the folder where the files of our project are
path_of_files <- "~/edX Data Science/9. Capstone/HarvardX - Data Science - PH125.9x - MovieLens project/"
saveRDS(path_of_files, file = paste0(path_of_files, "objects/path_of_files.rds"))

file <- paste0(path_of_files, "data sets/edx.rds")
if(file.exists(file)) {
  edx <- readRDS(file) 
}

file <- paste0(path_of_files, "data sets/final_holdout_test.rds")
if(file.exists(file)) {
  final_holdout_test <- readRDS(file)
}

file <- paste0(path_of_files, "objects/minimum_RMSE.rds")
if(file.exists(file)) {
  minimum_RMSE <- readRDS(file)
} else {
  minimum_RMSE <- 0.8649
}  
  
show_kable_table <- function(table_to_print) {
  knitr::kable(table_to_print, "latex", booktabs = T) |>
    kable_styling(full_width = TRUE) #position = "center")
}

file <- paste0(path_of_files, "objects/RMSE_function.rds")
if(file.exists(file)) {
  RMSE <- readRDS(file)
} else {
  # RMSE will be used to evaluate how close your predictions are to the true values in the final_holdout_test data set
  RMSE <- function(true_ratings, predicted_ratings) {
    sqrt(mean((true_ratings - predicted_ratings)^2))
  }
  saveRDS(RMSE, file = paste0(path_of_files, "objects/RMSE_function.rds"))
}  

file <- paste0(path_of_files, "objects/rating_range.rds")
if(file.exists(file)) {
  rating_range <- readRDS(file)
}  

file <- paste0(path_of_files, "objects/RMSE_of_recommender_system.rds")
if(file.exists(file)) {
  RMSE_of_recommender_system <- readRDS(file)
}

file <- paste0(path_of_files, "objects/RMSE_of_gbm")
if(file.exists(file)) {
  RMSE_of_gbm <- readRDS(file)
}

file <- paste0(path_of_files, "results/results_overview_table.rds")
if(file.exists(file)) {
  results_overview_table <- readRDS(file)
}

file <- paste0(path_of_files, "objects/rating_range.rds")
if(file.exists(file)) {
  rating_range <- readRDS(file)
}

#rm(file) # erase this variable since we do not need it anymore
#gc() # garbage collection (free up memory that is not used anymore)
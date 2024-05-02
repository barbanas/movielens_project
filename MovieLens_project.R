## ----load_common_objects11, echo=FALSE---------------------------------------------------------------

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

## ----setting_minimum_RMSE,echo=FALSE-----------------------------------------------------------------

minimum_RMSE <- 0.8649 # as set by the project's guidelines
  


## ----saving_minimum_RMSE, echo=FALSE-----------------------------------------------------------------

saveRDS(minimum_RMSE, file = paste0(path_of_files, "objects/minimum_RMSE.rds"))



## ----showing_minimum_RMSE,echo=TRUE,eval=FALSE-------------------------------------------------------
## 
## minimum_RMSE <- 0.8649 # as set by the project's guidelines
## 


## ----variables_data_types,echo=FALSE-----------------------------------------------------------------

options(digits = 6)

variables_data_types_table <- data.frame(
  Variable = c("`userId`", 
           "`movieId`", 
           "`rating`", 
           "`timestamp`", 
           "`title`", 
           "`genres`"),
  
  Data_type = c("Numerical", 
             "Numerical",
             "Numerical", 
             "Numerical", 
             "Character string", 
             "Character string")
)



## ----variables_table1,echo=FALSE, results='Variables and their data type'----------------------------

 print(variables_data_types_table)
  


## ----variables,echo=FALSE----------------------------------------------------------------------------

suppressPackageStartupMessages(library(kableExtra)) # used for styling the kable tables

options(digits = 6)

variables_description_table <- data.frame(
  Variable = c("`userId`", 
           "`movieId`", 
           "`rating`", 
           "`timestamp`", 
           "`title`", 
           "`genres`"),
  
  Description = c("Each user has a unique ID (e.g. 1) that distinguises him/her from all the other users", 
                  "Each movie has a unique ID (e.g. 122) that distinguises it from all the other movies",
                  "Ratings on this data set are ranging from 0, 0.5, 1, 2, 3, 4, 4.5 and 5.0", 
                  "Date and time stamp as (Unix epoch) when the rating was submitted (e.g. 838985046)", 
                  "String for Movie title and, in parenthesis (), the year the movie first released",
                  "String of characters that contains the genre(s) to which the movie belongs to (e.g. Drama, etc)"
  )
)

## ----variables_table2,echo=FALSE, results='Variables description'------------------------------------

 #print(variables_description_table)
  t <- knitr::kable(variables_description_table, format = "markdown", col.width = 50) |>
    kable_styling()
  t |> column_spec(1, width = "100px")

  


## ----some_lines_from_the_train_set, echo=TRUE--------------------------------------------------------

  head(edx)



## ----RMSE_function,echo=TRUE-------------------------------------------------------------------------

print(RMSE)



## ----show_kable_table_function,echo=TRUE-------------------------------------------------------------

print(show_kable_table)



## ----why_I_need_to_reload_this_although_is_in_common_R_I_do_not_know1,echo=FALSE---------------------
min_rating <- 0.5
max_rating <- 5



## ----count_users_movies, echo=FALSE------------------------------------------------------------------
  
unique_user_count <- length(unique(edx$userId))

unique_movies_count <- length(unique(edx$movieId))



## ----matrix1,fig.cap="Number of ratings given to most rated Movies by most active users",warning=FALSE----

suppressPackageStartupMessages(library(dplyr)) # data manipulation functions
suppressPackageStartupMessages(library(ggplot2)) # used for creating plots and charts
suppressPackageStartupMessages(library(tidyr)) # data manipulation (reshaping and 
                                               # transforming data) to tidy format
library(RColorBrewer) # color palette for charts

top_number <- 100 # how many users and movies to take into account
# Calculate the total number of ratings given by each user
user_ratings_count <- edx |>
  group_by(userId) |>
  summarise(total_ratings = n()) |>
  arrange(desc(total_ratings))

# Calculate the total number of ratings given to each movie
movie_ratings_count <- edx |>
  group_by(movieId) |>
  summarise(total_ratings = n()) |>
  arrange(desc(total_ratings))

# Select the first "top_number" users with the most ratings
top_users <- user_ratings_count |>
  slice(1:top_number) |>
  pull(userId)

# get the movieIds of the first "top_number" movies 
# (movies that were rated the most)
most_rated_movies <-  movie_ratings_count |>
  slice(1:top_number) |>
  pull(movieId)

# Filter the ratings data set to include only ratings from the selected users
ratings_subset <- edx |>
  filter(userId %in% top_users & movieId %in% most_rated_movies)

# Pivot the data to wide format
ratings_matrix <- ratings_subset |>
  select(userId, title, rating) |>
  pivot_wider(names_from = title, values_from = rating, values_fill = NA)

# Convert data to long format
long_data <- pivot_longer(ratings_matrix, 
                          cols = -userId, 
                          names_to = "Item", 
                          values_to = "Rating")

# Plotting
ggplot(long_data, aes(x = Item, y = factor(userId), fill = Rating)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = RColorBrewer::brewer.pal(3, "Blues")) + 
  labs(
    title = "User Ratings for most rated Movies by most active Users",
    x = paste0("Users (",top_number," most active users)"),
    y = paste0("Movie (",top_number," most rated movies)"),
    fill = "Rating"
  ) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 5)) + # make the font of the y-axis 
                                                # values smaller
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.x = element_blank()) + # Hide x-axis labels
  coord_fixed()



## ----matrix2,warning=FALSE---------------------------------------------------------------------------

#Count the total number of rows in the tibble
total_rows <- nrow(user_ratings_count)

# Select the X users with the least ratings
least_active_users <- user_ratings_count |>
  slice((total_rows - top_number):total_rows) |>
  pull(userId)

# Filter the ratings dataset to include only ratings from the selected users
ratings_subset <- edx |>
  filter(userId %in% least_active_users & movieId %in% most_rated_movies)

# Pivot the data to wide format
ratings_matrix <- ratings_subset |>
  select(userId, title, rating) |>
  pivot_wider(names_from = title, values_from = rating, values_fill = NA)

# Convert data to long format
long_data <- pivot_longer(ratings_matrix, 
                          cols = -userId, 
                          names_to = "Item", 
                          values_to = "Rating")



## ----fig.cap="User Ratings given to the most rated Movies by least active Users",warning=FALSE-------

# Plotting
ggplot(long_data, aes(x = Item, y = factor(userId), fill = Rating)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = RColorBrewer::brewer.pal(3, "Blues")) + 
  labs(
    title = "Ratings given to most rated Movies by least active Users",
    x = paste0("Users (",top_number," least active users)"),
    y = paste0("Movie (",top_number," most rated movies)"),
    fill = "Rating"
  ) +
  theme_classic() +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(size = 5)) +
  theme(axis.text.x = element_blank()) + # Hide x-axis labels
  coord_fixed()



## ----mean_rating_per_user_calculation,warning=FALSE--------------------------------------------------

# aggregate ratings by user
user_ratings <- edx |>
  group_by(userId) |>
  summarise(mean_rating = mean(rating))


## ----mean_rating_per_user_plot,fig.cap="Distribution of ratings of Users",warning=FALSE--------------
# plot the results
ggplot(user_ratings, aes(x = mean_rating)) +
  geom_histogram(binwidth = 0.5, 
                 fill = RColorBrewer::brewer.pal(10, "Set3")[5],  
                 color = "black") +
  labs(title = "Distribution of User Ratings",
       x = "Mean Rating",
       y = "Frequency") +
  scale_x_continuous(breaks = seq(min_rating, max_rating, by = 0.5)) + 
  #scale_y_log10() + # Add log scale on y-axis
  theme_classic()



## ----ratings_table,warning=FALSE,results='Counting of ratings'---------------------------------------

library(kableExtra) # used for styling the kable tables

total_number_of_ratings <- edx |> 
  filter(!is.na(rating)) |>
  nrow()

ratings_table <- edx |> group_by(rating) |>
  summarize(count = n()) 

ratings_table <- ratings_table |> 
  mutate(percentage = count / total_number_of_ratings)
  
colnames(ratings_table) <- c("Rating", "Number of ratings", "Percentage")

show_kable_table(ratings_table)



## ----distribution3_calculation,warning=FALSE---------------------------------------------------------

# Aggregating ratings by movieId and calculating count of ratings for each movie
movie_ratings <- edx |>
  group_by(movieId) |>
  summarise(avg_rating = mean(rating),
            rating_count = n())

# Sorting movies by rating count (optional)
movie_ratings <- movie_ratings |>
  arrange(desc(rating_count))


## ----distribution3_plot,fig.cap="Distribution of Movie ratings",warning=FALSE------------------------

# Plotting the distribution of ratings for movies
ggplot(movie_ratings, aes(x = avg_rating)) +
  geom_histogram(binwidth = 0.1, 
                 fill = RColorBrewer::brewer.pal(10, "Set3")[5], 
                 color = "black") +
  labs(title = "Distribution of Movie ratings by movieId",
       x = "Average Rating",
       y = "Number of Movies") +
  theme_classic()



## ----examine_range_of_ratings, echo=FALSE, warning=FALSE---------------------------------------------

ratings <- edx |> group_by(rating) |> summarize(count = n()) |> pull(rating)

min_rating <- ratings[1]
max_rating <- ratings[length(ratings)]
rating_range <- seq(min_rating, max_rating, 0.5)

saveRDS(rating_range, file = paste0(path_of_files, "objects/rating_range.rds"))



## ----plots6,fig.cap="Ratings of the train (edx) data set",warning=FALSE------------------------------
library(ggplot2) # used for creating plots and charts

# Create histogram 
binwidth <- 0.5
edx |> ggplot(aes(x = rating)) +
  geom_histogram(binwidth = binwidth, 
                 fill = RColorBrewer::brewer.pal(10, "Set3")[5], 
                 color = "black", 
                 alpha = 0.8, 
                 center = TRUE) +
  labs(title = "Ratings of the train (edx) data set", x = "Ratings", y = "Count") +
  scale_x_continuous(breaks = seq(min_rating, max_rating, by = 0.5)) + 
  theme_classic()
  


## ----ratings_per_movie_data1,warning=FALSE,results="Ratings per movie"-------------------------------

ratings_per_movie <- edx |>
  group_by(movieId) |>
  summarise(total_ratings = n()) |>
  arrange(desc(total_ratings)) |> # Sort by total ratings in ascending order
  top_n(1000)



## ----ratingspermovieplot1,fig.cap='Total number of Ratings per Movie',warning=FALSE------------------

# Plotting
binwidth <- 200
ggplot(ratings_per_movie, aes(x = total_ratings)) +
  geom_histogram(binwidth = binwidth, 
                 fill = RColorBrewer::brewer.pal(10, "Set3")[5], 
                 color = "black") +
  labs(
    title = "Total Number of Ratings per Movie",
    x = "Total Ratings",
    y = "Number of Movies (log 10 scale)"
  ) +
  #xlim(NA, 30000) +
  #scale_x_log10() + # Add log scale on x-axis
  scale_x_continuous(breaks = seq(0, 
                                  max(as.numeric(ratings_per_movie$total_ratings)), 
                                  by = binwidth * 20)) + 
  scale_y_log10() + # Add log scale on y-axis
  theme_classic()


## ----examine_range_of_ratings_4,echo=FALSE,warning=FALSE---------------------------------------------
options(tinytex.verbose = TRUE)

suppressPackageStartupMessages(library(dplyr)) # data manipulation functions

ratings <- edx |> 
  group_by(rating) |> 
  summarize(count = n()) |> 
  pull(rating)

min_rating <- ratings[1]
max_rating <- ratings[length(ratings)]
rating_range <- seq(min_rating, max_rating, 0.5)



## ----algos1,warning=FALSE,echo=TRUE------------------------------------------------------------------

suppressPackageStartupMessages(library(kableExtra)) # used for styling the kable tables

set.seed(2024)

# make random guessing of the rating a user may give to a movie
B <- nrow(final_holdout_test) 
random_predictions_based_on_absolute_guessing <- data.frame(random_rating = 
                                                            replicate(B, 
                                                            sample(rating_range, 
                                                                   size = 1, 
                                                                   replace = TRUE)))

# Construct a table that will hold the results of the various models
results_overview_table <- 
    tibble(Model = c("Objective of the Project is RMSE < 0.8649", 
                     "Random guessing"),
                     RMSE = c(minimum_RMSE, 
                              RMSE(final_holdout_test$rating,
                              random_predictions_based_on_absolute_guessing$random_rating)
                             )
           )

# inspect results so far
show_kable_table(results_overview_table)



## ----random_guesses,warning=FALSE,echo=TRUE----------------------------------------------------------

library(kableExtra) # used for styling the kable tables

how_many_movies_do_not_have_rating <- edx |> 
  filter(!edx$rating %in% ratings) |> 
  count()

if (how_many_movies_do_not_have_rating != 0) {
  print("There are movies that do not have a rating!")
  
} else {
  #print("All movies have ratings")
  total_ratings <- nrow(edx) # how many ratings we have?
  
  # calculate the prevelance of each rating (0.5, 1, 2, ..., 5)
  prevelance <- edx |>
    group_by(rating) |> 
    summarize(count = n(), prevelance = n() / total_ratings) |> 
    pull(prevelance)
  
  # guess in random again but this time, use as the probability to select 
  # a rating, according to the prevalence of this rating in the edx data set
  random_predictions_based_on_biased_guessing <- 
    data.frame(random_biased_rating = replicate(B, sample(rating_range, 
                                        size = 1, 
                                        replace = TRUE, 
                                        prob = prevelance)))

  # add the result to our comparison table
  results_overview_table <- bind_rows(results_overview_table, 
                                      tibble(Model = "Random guessing (biased)",
                                             RMSE = RMSE(final_holdout_test$rating,                                                   random_predictions_based_on_biased_guessing$random_biased_rating)))

  }

# inspect results so far
show_kable_table(results_overview_table)



## ----mean_rating_everywhere,warning=FALSE,echo=TRUE--------------------------------------------------

# Start with the mean rating of all movies from all users
mu <- mean(edx$rating) # mean rating independently from movie and user bias

# create as many predictions as the rows of the final_holdout_test
y_hat_mean <- rep(mu, nrow(final_holdout_test))

# calculate the RMSE according to this method
results_overview_table <- bind_rows(results_overview_table, 
                                    tibble(Model = "Give mean rating to all movies",
                                           RMSE = 
                                             RMSE(final_holdout_test$rating, 
                                                  y_hat_mean)))

# inspect results so far
show_kable_table(results_overview_table)



## ----mea_rating_everywhere,warning=FALSE,echo=TRUE---------------------------------------------------

# estimate the movie bias which is the mean rating per movie 
# minus the mean rating of all movies
b_i <- edx |>
  group_by(movieId) |> 
  summarize(b_i = mean(rating - mu)) 
# get ratings of each movie and subtract the mean rating

b_i # Movie bias



## ----movei_bias,warning=FALSE,echo=TRUE--------------------------------------------------------------

# calculate the rating based on the mean rating and the movie effect 
y_hat_b_i <- final_holdout_test |>
  left_join(b_i, by = "movieId") |>
  mutate(b_i = b_i + mu) |> pull(b_i)

# inspect the results
head(y_hat_b_i)

# calculate the RMSE of the movie bieas and add it to our comparison table
results_overview_table <- bind_rows(results_overview_table, 
                                    tibble(Model = "Mean rating + movie bias",
                                           RMSE = RMSE(final_holdout_test$rating, 
                                                       y_hat_b_i)))

# inspect results so far
show_kable_table(results_overview_table)



## ----adding_the_user_effect, warning=FALSE,echo=TRUE-------------------------------------------------

# calculate the bias a user has when it rates a movie
b_u <- edx |>
  left_join(b_i, by = 'movieId') |>
  group_by(userId) |> 
  summarize(b_u = mean(rating - mu - b_i))  # note here that we subtract not only 
                                            # the mu but also the b_i
b_u # User bias

# calculate the y_hat for the user effect + the movie effect
y_hat_b_u <- final_holdout_test |>
  left_join(b_i, by='movieId') |>
  left_join(b_u, by='userId') |>
  mutate(y_hat = mu + b_i + b_u) |>
  pull(y_hat)

# show the results
head(y_hat_b_u)

# add the RMSE of this model to our comparison table
results_overview_table <- bind_rows(results_overview_table, 
                                    tibble(Model="Mean rating + movie bias + user bias",
                                           RMSE = RMSE(final_holdout_test$rating, 
                                                       y_hat_b_u)))

# inspect results so far
show_kable_table(results_overview_table)

# save the results table for later use
saveRDS(results_overview_table, file = paste0(path_of_files, 
                                              "results/results_overview_table.rds"))


## ----echo=FALSE,warning=FALSE------------------------------------------------------------------------

RMSE_of_gbm <- 0 # initialize this variable (we will use it later)

# how much data we will feed to our ML algos?
percentage_of_data_to_use <- 0.01  # any value above 0.10 crashes the R



## ----show_libraries,echo=TRUE,warning=FALSE,eval=FALSE-----------------------------------------------
## 
## library(caret) # train and predict with ML algos
## library(dplyr) # data manipulation
## 
## # libraries used for parallel execution by utilizing multiple cores of the CPU
## library(doParallel)
## library(parallel)
## 
## library(kableExtra) # used for styling the kable tables
## 

## ----load_libraries,echo=FALSE,warning=FALSE---------------------------------------------------------

suppressPackageStartupMessages(library(caret)) # train and predict with ML algos
suppressPackageStartupMessages(library(dplyr)) # data manipulation

# libraries used for parallel execution by utilizing multiple cores of the CPU
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(parallel))

suppressPackageStartupMessages(library(kableExtra)) # used for styling the kable tables


## ----echo=TRUE,warning=FALSE-------------------------------------------------------------------------

set.seed(20240423)

if (percentage_of_data_to_use == 1) {
  # use the entire data sets (100% of the data)
  train_set <- edx
  test_set <- final_holdout_test
  
} else {
# use a percentage_of_data_to_use of the data 
  train_set <- edx |> 
    slice(1:round(n() * percentage_of_data_to_use))
  
  test_set <- final_holdout_test |> 
    slice(1:round(n() * percentage_of_data_to_use))
  
}



## ----prediction_formula_5,echo=TRUE------------------------------------------------------------------

# set what we want to predict and our predictors for the job
prediction_formula <- as.formula("rating ~ userId + movieId") 


## ----prediction_formula,echo=FALSE-------------------------------------------------------------------
total_caret_models <- length(unique(modelLookup()$model))


## ----training,echo=TRUE,warning=FALSE----------------------------------------------------------------

# Set how many cores will be used in the parallel execution for 
# train() and predict() below.  
# I leave 2 cores free for the basic operation of the OS itself
number_of_cores <- detectCores() - 2 

# # set this to TRUE if you want to use Tune Grids for the algos. 
# Note that TuneGrids increase the time a model needs for training
EnableTuneGrids <- TRUE 

cl <- makeCluster(number_of_cores) # create a parallel environment
registerDoParallel(cl)

# send to the parallel environment the data for training
clusterExport(cl, c("train_set"))  

  #
  # 1st ML algo
  #
  file <- paste0(path_of_files, 
                 "fits/fit_svmRadial_with_",
                 percentage_of_data_to_use,
                 "_of_the_data.rds")
  if(file.exists(file) == TRUE) {
  # if we have trained this model before, just load it from its file, 
  # otherwise train() it then save it to its file
      fit_svmRadial <- readRDS(file)    
    
  } else {
  # we have not trained this model before: do this now
      if (EnableTuneGrids == FALSE) {
        tg <- NULL
      } else {
        tg <- NULL
        #tg <- data.frame(# Number of neighbors: choose a range based on 
        #   data size and complexity
        #   k = seq(3, 50, 2)
        # )
      }
      
      # do the training
      fit_svmRadial <- train(prediction_formula,
                     data = train_set,
                     method = "svmRadial",
                     trControl = trainControl(allowParallel = TRUE, 
                                              # this runs the training in parallel
                                              verboseIter = TRUE, 
                                              method = "cv"), # use cross validation
                     tuneGrid = tg
      )
      
      # save the fit 
      saveRDS(fit_svmRadial, file = file)
      #rm(fit_svmRadial) # we do this in order to save memory; 
      # we can reload the object after ALL calculations have finished
      #gc() # clear free up memory (garbage collection)

  }

  #
  # 2nd ML algo
  #
  file <- paste0(path_of_files, 
                 "fits/fit_rf_with_",
                 percentage_of_data_to_use,
                 "_of_the_data.rds")
  if(file.exists(file) == TRUE) {
  # if we have trained this model before, just load it from its file, otherwise train() 
  # it then save it to its file
      fit_rf <- readRDS(file)    
      
  } else {    
  # we have not trained this model before: do this now
      if (EnableTuneGrids == FALSE) {
        tg <- NULL
        ns <- NULL
      } else {
        tg <- expand.grid(
          # Number of variables sampled at each split: try different ratios
          mtry = seq(2, sqrt(ncol(edx)))  # ncol is the number of predictor variables
          #splitrule = c("gini", "extratrees", "variance"),
          #ntree = c(100, 200, 500), # Number of trees: explore a range suitable 
          #  for data size and complexity
          #min.node.size = seq(1, 51, 10) # Minimum size of terminal nodes: prevent 
          # overfitting with large trees
        )
        ns <- seq(1, 51, 10)
      }
      
      # train
      fit_rf <- train(prediction_formula,
                      data = train_set,
                      method = "rf",
                      trControl = trainControl(allowParallel = TRUE, 
                                               verboseIter = TRUE, 
                                               method = "cv"),
                      tuneGrid = tg,
                      nodesize = ns
      )
      
      # save the results
      saveRDS(fit_rf, file = file)
      #rm(fit_rf) # we do this in order to save memory; we can
      # reload the object after ALL calculations have finished
      #gc() # clear free up memory (garbage collection)
      
  }

  #
  # 3rd ML algo
  #
  file <- paste0(path_of_files, "fits/fit_gbm_with_",
                 percentage_of_data_to_use,
                 "_of_the_data.rds")
  if(file.exists(file) == TRUE) {
      fit_gbm <- readRDS(file)  
      
  } else {    
    # train the model from scratch
      if (EnableTuneGrids == FALSE) {
        tg <- NULL
      } else {
        tg <- NULL #expand.grid(parameter = seq(0.01, 1, by = 0.01)) #), 
                   # family = "binomial"), # binomial for classification)
      }
    
      # train
      fit_gbm <- train(prediction_formula,
                     data = train_set,
                     method = "gbm",  #tried: xgboost, lightgbm but they are not part 
                                      # of the caret package
                     trControl = trainControl(allowParallel = TRUE, 
                                              verboseIter = TRUE, 
                                              method = "cv"),
                     tuneGrid = tg
      )
      
      # save the results
      saveRDS(fit_gbm, file = file)
      #rm(fit_gbm) # we do this in order to save memory; we can
      # reload the object after ALL calculations have finished
      #gc() # clear free up memory (garbage collection)
      
  }

# Stop the parallel backend
stopCluster(cl)




## ----predict_results,echo=TRUE-----------------------------------------------------------------------

# Make predictions
# Predict based on the Support Vector Machine (SVM) Radial model
file <- paste0(path_of_files, 
               "predictions/predictions_svmRadial_with_",
               percentage_of_data_to_use,
               "_of_the_data.rds") 
if(file.exists(file) == TRUE) {
# predictions have already been made. No need to do these again: just 
# load them from the file.
# We do (this to save time the 2nd, 3rd, etc time we run this script)
  predictions_svmRadial <- readRDS(file)
    
} else {
    # do the predictions with this model
    predictions_svmRadial <- predict(fit_svmRadial, 
                                     newdata = test_set) 
    
    # save the results
    saveRDS(predictions_svmRadial, file = file)
    
}

# Predict based on the Random Forest model
file <- paste0(path_of_files, 
               "predictions/predictions_rf_with_",
               percentage_of_data_to_use,
               "_of_the_data.rds") 
if(file.exists(file) == TRUE) {
# predictions have already been made. No need to do these again: just 
# load them from the file (this is to save time the 2nd, 3rd, etc 
# time you run this script).
    predictions_rf <- readRDS(file)
    
} else {
    # predict 
    predictions_rf <-predict(fit_rf, 
                             newdata = test_set) 
    
    # save the results
    saveRDS(predictions_rf, file = file)
    
}  

# Predict based on the Gradient Boosting Machine model
file <- paste0(path_of_files, 
               "predictions/predictions_gbm_with_",
               percentage_of_data_to_use,"
               _of_the_data.rds") 
if(file.exists(file) == TRUE) {
# predictions have already been made. No need to do these again: just load them 
# from the file (we do this to save time the 2nd, 3rd, etc time you run this script)
    predictions_gbm <- readRDS(file)
    
} else {
    # predict 
    predictions_gbm <- predict(fit_gbm, 
                               newdata = test_set) 
    
    # save the results
    saveRDS(predictions_gbm, file = file)
    
}



## ----summarize_results,echo=TRUE---------------------------------------------------------------------

# read our results table
results_overview_table <- readRDS(paste0(path_of_files, 
                                         "results/results_overview_table.rds")) 

# add the RMSE of the prediction results into the comparison table
# add the results of the SVM model
results_overview_table <- bind_rows(results_overview_table, 
                                    tibble(Model = 
                                             paste0("Support Vector Machine (SVM) ",
                                                    "Radial (with ", 
                                                    percentage_of_data_to_use, 
                                                    " of the data)"),
                                           RMSE = 
                                             RMSE(test_set$rating, 
                                                  predictions_svmRadial)))

# add the results of the RF model
results_overview_table <- bind_rows(results_overview_table, 
                                    tibble(Model = paste0("Random Forest (with ",
                                                          percentage_of_data_to_use,
                                                          " of the data)"),
                                           RMSE = RMSE(test_set$rating, 
                                                       predictions_rf)))

# add the results of GBM model
results_overview_table <- bind_rows(results_overview_table, 
                                    tibble(Model =paste0("Gradient Boosting Machine ",
                                                           "(with ",
                                                           percentage_of_data_to_use,
                                                           " of the data)"),
                                           RMSE = RMSE(test_set$rating, 
                                                       predictions_gbm)))

# the GBM model seems that it estimated with greater accuracy. Save its 
# RMSE for future use
RMSE_of_gbm <- RMSE(test_set$rating, predictions_gbm)  
saveRDS(RMSE_of_gbm, file = paste0(path_of_files, "objects/RMSE_of_gbm.rds"))



## ----show_results,echo=TRUE,results='Comparison Table I'---------------------------------------------

# print the results so far
show_kable_table(results_overview_table)


## ----recosystem_train_and_predict,echo=TRUE,warning=FALSE--------------------------------------------

library(parallel) # used for parallel processing 
library(recosystem) # building, evaluating recommendation systems

library(kableExtra) # used for styling the kable tables

set.seed(20240424)

percentage_of_data_to_use <- 1 # how much data to take from the train set (1 = 100%)

file <- paste0(path_of_files, 
               "predictions/y_hat_rec_MF_with_",
               percentage_of_data_to_use,
               "_of_the_data.rds")
if(file.exists(file) == TRUE) {
# if the algo is already trained and its predictions already made, 
# load its predictions from the corresponding file 
    y_hat_rec_MF <- readRDS(file)   
    
} else {
  # set how many cores will be used in the parallel execution for training
  number_of_cores <- detectCores() - 2 
  
  # specify the indices that point to the user, movie and rating in the edx (train) 
  # data set. 
  train_set_reco <- with(edx, data_memory(user_index = userId, 
                                          item_index = movieId, 
                                          rating = rating))
  
  # do the same for the final_holdout_test (test) data set. 
  test_set_reco <- with(final_holdout_test, data_memory(user_index = userId, 
                                                        item_index = movieId, 
                                                        rating = rating))
  
  #
  # Note that we use the ENTIRE edx and final_holdout_test 
  # (not just a percentage of them)
  #
  
  # initialize a new object of a recommendation system
  recommendation_system <- Reco()
  
  # set the tuning parameters
  tuning_MF <- recommendation_system$tune(train_set_reco, 
                                          opts = list(dim = c(10, 20, 30, 40),
                                                      lrate = c(0.1, 0.2, 0.3, 0.4),
                                                      nthread  = number_of_cores,
                                                      niter = 20))
  
  # do the training with the train set
  recommendation_system$train(train_set_reco, 
                              opts = c(tuning_MF$min, 
                                       nthread = number_of_cores, 
                                       niter = 20)
                              )
  
  # do the predictions on the test set
  y_hat_rec_MF <- recommendation_system$predict(test_set_reco, out_memory())

  # save the results of the predictions
  saveRDS(y_hat_rec_MF, file = file)
  
}



## ----recosystem_evaluation,echo=TRUE,warning=FALSE---------------------------------------------------

library(dplyr)

# calculate the RMSE of the predictions of our model
RMSE_of_recommender_system <- RMSE(final_holdout_test$rating, y_hat_rec_MF)

# save the results to this file for future use
saveRDS(RMSE_of_recommender_system, 
        file = paste0(path_of_files, 
                      "objects/RMSE_of_recommender_system.rds"))

# add the results to the comparison table
results_overview_table <- bind_rows(results_overview_table, 
                                    tibble(Model = 
                                             paste0("Matrix factorization",
                                               " (with 100% of the data)"),
                                           RMSE = RMSE_of_recommender_system))



## ----display_results,echo=TRUE,eval=TRUE-------------------------------------------------------------

# print the comparison table that contains all the models 
show_kable_table(results_overview_table)



## ----previs_of_progress_bar,echo=TRUE,warning=FALSE,eval=FALSE---------------------------------------
## 
##   0%   10   20   30   40   50   60   70   80   90   100%
##   [----|----|----|----|----|----|----|----|----|----|
##   **
## 


## ----echo=FALSE--------------------------------------------------------------------------------------

library(parallel) 

system_info <- Sys.info()
number_of_cores <- detectCores()

print(paste0("OS: ", system_info["sysname"], " version ", system_info["version"], " (release: ", system_info["release"], ")"))
print(paste0("Virtual Machine: ", system_info["machine"]))
print(paste0("CPU: 2 x Intel Xeon E5-2697 v2 @ 2.70 GHz (from which we used 1 processor with ", number_of_cores, " cores)"))
print(paste0("Total R.A.M memory: 40 GB"))



## ----echo=FALSE--------------------------------------------------------------------------------------

sessionInfo()



## ----------------------------------------------------------------------------------------------------

# create a bib db of R packages
knitr::write_bib(c(.packages(), "bookdown", "knitr", "rmarkdown"), "packages.bib")



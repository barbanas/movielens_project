##########################################################
# Create edx and final_holdout_test sets 
##########################################################
suppressWarnings(library(caret))

# define the two data sets: edx (= training set) and final_holdout_test (= test set)
data_files <- c(paste0(path_of_files, "data sets/edx.rds"), 
                paste0(path_of_files, "data sets/final_holdout_test.rds"))

# if the data files are already downloaded and prepared, just load them from the disk in order to save time
if(all(file.exists(data_files)) == FALSE) {
  # data set files are not downloaded: do this now
  # Note: this process could take a couple of minutes
  if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
  
  suppressWarnings(library(tidyverse))
  suppressWarnings(library(caret))
  
  # MovieLens 10M data set:
  # https://grouplens.org/data sets/movielens/10m/
  # http://files.grouplens.org/data sets/movielens/ml-10m.zip
  
  options(timeout = 120)
  
  dl <- "ml-10M100K.zip"
  if(!file.exists(dl))
    download.file("https://files.grouplens.org/data sets/movielens/ml-10m.zip", dl)
  
  ratings_file <- "ml-10M100K/ratings.dat"
  if(!file.exists(ratings_file))
    unzip(dl, ratings_file)
  
  movies_file <- "ml-10M100K/movies.dat"
  if(!file.exists(movies_file))
    unzip(dl, movies_file)
  
  ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                           stringsAsFactors = FALSE)
  colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
  ratings <- ratings |>
    mutate(userId = as.integer(userId),
           movieId = as.integer(movieId),
           rating = as.numeric(rating),
           timestamp = as.integer(timestamp))
  
  movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                          stringsAsFactors = FALSE)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- movies |>
    mutate(movieId = as.integer(movieId))
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Final hold-out test set will be 10% of MovieLens data
  set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
  # set.seed(1) # if using R 3.5 or earlier
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  # Make sure userId and movieId in final hold-out test set are also in edx set
  final_holdout_test <- temp |> 
    semi_join(edx, by = "movieId") |>
    semi_join(edx, by = "userId")
  
  # Add rows removed from final hold-out test set back into edx set
  removed <- anti_join(temp, final_holdout_test)
  edx <- rbind(edx, removed)
  
  rm(dl, ratings, movies, test_index, temp, movielens, removed)
  
  saveRDS(edx, file = paste0(path_of_files, "data sets/edx.rds"))
  saveRDS(final_holdout_test, file = paste0(path_of_files, "data sets/final_holdout_test.rds"))
  
}

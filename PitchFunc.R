# functions for PitchCalc

#
# Data Wrangling
#
load_data <- function(file_name) {
  data <- read.csv(file_name, stringsAsFactors = FALSE)
  data <- keep_only_fastballs(data)
  data <- clean_variable_types(data)
  data <- reduce_data_size(data)
  data <- complete_cases(data)
  data <- create_swinging_strike_variable(data)
  data <- remove_variables_that_should_be_avoided(data)
  data <- convert_character_variables_to_factors(data)
  data <- train_and_validation_data_split(data)
  return(data)
}

keep_only_fastballs <- function(data) {
  return(data[data$PitchType == "Fastball", ])
}

clean_variable_types <- function(data) {
  #
  # Some values in the data CSV contain "NULL" strings, making variables
  # containing such values to be read as `character` types, and when
  # converting them to `numeric` they produce an "NAs introduced by coercion"
  # warning. That's fine, and we can take care of those if we want to remove
  # them by using our `complete_cases()` function afterwards. You can see the
  # warnings by uncommenting below the three lines that hide them.
  #
  original_warning_level <- getOption("warn")
  options(warn = -1)
  data$SpinAxis <- as.numeric(data$SpinAxis)
  data$SpinRate <- as.numeric(data$SpinRate)
  data$ReleaseSpeed <- as.numeric(data$ReleaseSpeed)
  data$VertApproachAngle <- as.numeric(data$VertApproachAngle)
  data$HorzApproachAngle <- as.numeric(data$HorzApproachAngle)
  options(warn = original_warning_level)
  return(data)
}

reduce_data_size <- function(data) {
  set.seed(12345)
  if (is.na(DATA_SIZE)) { return(data) }
  selection <- sample(1:nrow(data), size = DATA_SIZE)
  return(data[selection, ])
}

complete_cases <- function(data) {
  return(data[complete.cases(data), ])
}

remove_variables_that_should_be_avoided <- function(data) {
  c <- colnames(data)
  return(data[, c[!(c %in% VARIABLES_TO_AVOID)]])
}

create_swinging_strike_variable <- function(data) {
  data$SwingingStrike <- as.factor(data$PitchResult %in% SWINGING_STRIKE_CASES)
  return(data)
}

convert_character_variables_to_factors <- function(data) {
  convert <- colnames(data)[lapply(data, class) == "character"]
  for (c in convert) { data[, c] <- as.factor(data[, c]) }
  return(data)
}

train_and_validation_data_split <- function(data) {
  set.seed(12345)
  sample_size <- floor(DATA_VALIDATE_PROPORTION * nrow(data))
  selection <- sample(1:nrow(data), size = sample_size)
  return(list(validate = data[selection, ], train = data[-selection, ]))
}


#
# Probit Model
#




#
# Random Forest Model
#
optimal_random_forest <- function(data, n_tree, n_cv_splits, n_var_each_split) {
  #
  # `n_var_each_split`: number of variables to randomly select at each split
  # `n_tree`: number of trees to grow for each random forest
  # `n_cs_splits`: number of cross-validation splits
  #
  set.seed(12345)
  tuneGrid <- expand.grid(.mtry = n_var_each_split)
  trControl <- trainControl(
    number = n_cv_splits,
    verboseIter = TRUE,
    search = "grid",
    method = "cv"
  )
  optimal_model <- train(
    SwingingStrike ~ .,
    trControl = trControl,
    tuneGrid = tuneGrid,
    metric = "Accuracy",
    method = "rf",
    importance = TRUE,
    ntree = n_tree,
    data = data
  )
  return(optimal_model)
}

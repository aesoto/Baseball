# functions for ~/R/Twins/main.R

#
# Data Wrangling
#
load_data <- function(file_name) {
  data <- read.csv(file_name, stringsAsFactors = FALSE)
  data <- keep_only_fastballs(data) 
  data <- clean_variable_types(data) # cleans data
  data <- reduce_data_size(data)
  data <- complete_cases(data) # cleans data
  data <- create_swinging_strike_variable(data)
  data <- remove_variables_that_should_be_avoided(data)
  data <- convert_character_variables_to_factors(data)
  data <- train_and_validation_data_split(data) # creates training and validation data sets
  return(data)
}

keep_only_fastballs <- function(data) {
  return(data[data$PitchType == "Fastball", ])
}

clean_variable_types <- function(data) {
  #
  # NOTE: Some values in the data file contain "NULL" strings, making such variables
  #       to be read as `character` types. When converted to 'numeric' they produce
  #       "NAs introduced by coercion" warnings.
  #       These NAs are removed later using the `complete_cases()'. 
  #       I suppress the warnings to keep the console clean.
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

complete_cases <- function(data) { # cleans data
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

train_and_validation_data_split <- function(data) {    # returns training and validation data sets
  set.seed(12345)
  sample_size <- floor(DATA_VALIDATE_PROPORTION * nrow(data)) 
  selection <- sample(1:nrow(data), size = sample_size)   
  return(list(validate = data[selection, ], train = data[-selection, ]))
}


#
# Logistical Regression Model
#
optimal_logit <- function(data, n_cv_splits) {
  #
  # NOTE: Here I leverage off the 'caret' package syntax which effectively allows me to deploy multiple models.
  #       The `train()`function call is not necessary since Logit models do not have parameters to optimize
  #       (they simply maximize the Maximum Likelihood), so the result will be the same
  #       regardless of the cross-validation procedure.
  #
  set.seed(12345)
  optimal_model <- train(
    SwingingStrike ~ .,
    trControl = train_control(n_cv_splits),
    family = binomial(),
    metric = "Accuracy",
    method = "glm",
    data = data
  )
  return(optimal_model)
}


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


#
# Model Validation
#
train_control <- function(n_cv_splits) {
  return(trainControl(
    number = n_cv_splits, verboseIter = TRUE, search = "grid", method = "cv"))
}

print_confusion_matrix <- function(model, data_validate) {
  predictions <- predict(model, data_validate)
  print(confusionMatrix(predictions, data_validate$SwingingStrike))
}
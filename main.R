#
# COMMENTS:
#     Also part of this package are:
#       - PitchFunc.R: functions to run main
#       - README.TXT: I write an executive summary and paste some output from the script
#       - Image of variable importance plot (2 versions)
#     All of the above reside on my GitHub page: https://bit.ly/2zwSDZB
#

library(randomForest)
library(caret)

source("~/R/Twins/PitchFunc.R")

# 
# NOTE: SwingStrikes occur relatively rarely - approximately 6.83% of the time, resulting
#       in an imbalanced data set. There are techniques to accommodate rare events and would require
#       adjustments to resampling, evaluating, and cross sampling.  Something necessarily
#       done in a second phase of analysis.
#

#
# Parameters
# 
SWINGING_STRIKE_CASES <- c("SwingingStrike", "SwingPitchout", "SwingStrikeBlk")
VARIABLES_TO_AVOID <- c("GameId", "PitchResult", "PitchType")
DATA_VALIDATE_PROPORTION <- 0.3

#
# NOTE: Full data analysis takes too much computation time. This parameter
#       controls how many observations are loaded.
#       There are 385,318 fastballs (before cleaning).
#       If this parameter is specified as `NA`, no data reduction will
#       take place.
#
DATA_SIZE <- NA # or 20000


#
# Analysis
#
data <- load_data("~/R/Twins/PitchData_v2.csv")  # n.b. all data wrangled and cleaned in function file
data_validate <- data[['validate']]
data_train    <- data[['train']]


##################################################################
# Logit Model
#
# NOTE: Categorical response variables are binomial (e.g. coin toss).  An OLS
#       regression won't work as you do not satisfy the condition of normal residuals.
#       Logistic regression, among others, is a suitable choice.
#
model <- optimal_logit(data = data_train, n_cv_splits = 3)
print(model)
print_confusion_matrix(model, data_validate)

# For formal regression equation:
summary(model$finalModel)

#
# NOTE: Not all factors under consideration are statistically significant (p values > .05). 
#       Also, it is considered good form to further develop your model to be as parsimonious
#       as possible.  This process of factor selection consists of a variety of techniques and
#       my approach would begin with a more considered selection of predictive factors.
#


#################################################################
# Random Forest
#
# NOTE: A random forest provides some advantages in this case:
#       - Can be applied to binomial dependent variables
#       - However, this technique tends to overfit and
#         care should be taken to mitigate this issue.
#

# First Random Forest
model <- optimal_random_forest(
  n_tree = 100,
  n_cv_splits = 3,
  n_var_each_split = 20,
  data = data_train
)
print(model)
print_confusion_matrix(model, data_validate)

# Variable importance plot
varImpPlot(model$finalModel)

# Second Random Forest
model <- optimal_random_forest(
  n_tree = 200,
  n_cv_splits = 3,
  n_var_each_split = 10,
  data = data_train
)
print(model)
print_confusion_matrix(model, data_validate)

# Variable importance plot
varImpPlot(model$finalModel)


#
# Probability of a pitcher throwing a SwingStrike
#

# Based on actual data:
throws_by_pitcher <- aggregate(
  list(Pitches = data_validate$SwingingStrike),
  by = list(PitcherId = data_validate$PitcherId),
  FUN = function(x) { length(x) }
)

strike_probabilities_by_pitcher_with_records <- aggregate(
  list(StrikeProbability = data_validate$SwingingStrike),
  by = list(PitcherId = data_validate$PitcherId),
  FUN = function(x) { sum(as.logical(x)) / length(x) }
)

# Based on predicted data:
# NOTE: You can choose among the different models
data_validate$prediction <- predict(model, data_validate)

strike_probabilities_by_pitcher_with_predictions <- aggregate(
  list(StrikeProbability = data_validate$prediction),
  by = list(PitcherId = data_validate$PitcherId),
  FUN = function(x) { sum(as.logical(x)) / length(x) }
)

# Difference among predictions:
# (assumes same PitcherId order)
diff <- (
  strike_probabilities_by_pitcher_with_records -
    strike_probabilities_by_pitcher_with_predictions
)


# Most differences are within 7 percentage points
summary(diff$StrikeProbability)
hist(diff$StrikeProbability)

# look at the top 25 pitchers
strike_probabilities_by_pitcher_with_records <- merge(x = strike_probabilities_by_pitcher_with_records, y = throws_by_pitcher, by = "PitcherId", all = TRUE)
top_pitchers <- head(strike_probabilities_by_pitcher_with_records[order(-strike_probabilities_by_pitcher_with_records$StrikeProbability),],25)


library(randomForest)
library(caret)

source("~/R/Twins/PitchFunc.R")

# TODO: Imbalanced data set


#
# Parameters
#
SWINGING_STRIKE_CASES <- c("SwingingStrike", "SwingPitchout", "SwingStrikeBlk")
VARIABLES_TO_AVOID <- c("GameId", "PitchResult", "PitchType")
DATA_VALIDATE_PROPORTION <- 0.3



#
# NOTE: The models take too much time when using the full data. This parameter
#       can be used to control how many observations to keep when loading data.
#       There are 385,318 observations in the data marked as fastballs (before
#       cleaning). If the parameter is specified as `NA`, no data reduction will
#       take place.
#
DATA_SIZE <- NA



#
# Analysis
#
data <- load_data("~/R/Twins/PitchData_v2.csv")
data_validate <- data[['validate']]
data_train    <- data[['train']]

################################################################
# Probit Model
probit_factors <- data_validate[,c("SwingingStrike", "ReleaseSpeed", "PitchTimeToPlate", "SpinAxis", "SpinRate", "HorzBreakPFX",
                     "VertBreakPFX", "ReleaseHeight", "ReleaseSide", "Extension", "VertApproachAngle", "HorzApproachAngle")] 
probit_factors$SwingingStrike <- as.integer(as.logical(probit_factors$SwingingStrike))
corMatrix <- cor(probit_factors)
# Apply forward selection of factors
pitchCalc <- glm(SwingingStrike ~ VertApproachAngle + ReleaseHeight + ReleaseSpeed + VertBreakPFX + HorzBreakPFX + SpinRate, family = binomial(link = "probit"), 
                 data = probit_factors)
summary(pitchCalc)

# Test with validation data

# Prediction

################################################################
# Random Forest
# First specification
model <- optimal_random_forest(
    n_tree = 100,
    n_cv_splits = 3,
    n_var_each_split = 20,
    data = data_train
)
print(model)

# Random Forest
# Second specification
model <- optimal_random_forest(
    n_tree = 200,
    n_cv_splits = 3,
    n_var_each_split = 10,
    data = data_train
)
print(model)

# Test with validation data
predictions <- predict(model, data_validate)
confusionMatrix(predictions, data_validate$SwingingStrike)

# Variable importance plot
varImpPlot(model$finalModel)

# Prediction
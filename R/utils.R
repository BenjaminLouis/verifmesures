

#' Function to fit a model
#'
#' @param xdata a dataframe with the predictors
#' @param yvec a vector with response variable values
#'
#' @return the results of the fitted model with function caret::train
#'
#' @importFrom caret train trainControl twoClassSummary
#'
#' @export
#'
#' @examples
#' #find an example
train_mod <- function(xdata, yvec) {
  train(x = xdata, y = yvec,  method ="glm", metric = "ROC",
        trControl = trainControl(summaryFunction = twoClassSummary,
                                 classProbs = TRUE,
                                 savePredictions = TRUE,
                                 method = "repeatedcv",
                                 repeats = 10,
                                 number = 5))
}


#' Function to get confusion matrix from a model
#'
#' @param model a model from the train_mod function
#'
#' @return a list. see ?caret::confusionMatrix
#'
#' @importFrom caret confusionMatrix
#'
#' @export
#'
#' @examples
#' #find an example
get_confMat <- function(model) {
  confusionMatrix(data = model$pred$pred, reference = model$pred$obs)
}

#' Function to get the ROV statistics of a model
#'
#' @param model a model from the train_mod function
#' @param success character value defining how the success is coded
#'
#' @return a list of class "roc". See ?pROC::roc
#'
#' @importFrom pROC roc
#'
#' @export
#'
#' @examples
#' #find an example
get_roc <- function(model, success) {
  resroc <- roc(response = model$pred$obs,
                predictor = model$pred[, success],
                levels = levels(model$pred$obs))
}

#' Function to get predictions according to a fitted model
#'
#' @param model a model from the train_mod function
#' @param newdata the dtibble/ataframe with predictors
#' @param type "raw" or "prob". see ?predict.train
#'
#' @return a tibble with the data and the corresponding predictions
#'
#' @importFrom caret predict.train
#' @importFrom dplyr bind_cols
#'
#' @export
#'
#' @examples
#' #find an example
get_prediction <- function(model, newdata, type) {
  bind_cols(predict.train(model, newdata = newdata, type = type),
            newdata)
}

#' Function to get sensitivities ad specificities from a ROC statistics
#'
#' @param roc roc statistics obtain from et_roc function
#'
#' @return a tibble with two columns (sensitivities and sensibilities)
#'
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' #find an example
get_senspe <- function(roc) {
  tibble(sensi = roc$sensitivities,
         speci = roc$specificities)
}

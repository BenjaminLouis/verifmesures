
#' Title
#'
#' @param df a tibble or a data frame
#' @param ... the columns unquoted names with the methods to compare
#' @param response the response variable
#' @param success a character value defining how the success event of the response variable is coded
#' @param level a named vector defining how to code the events of response variable. See example below
#' @param pal a vector of colors of length the quantity of methods to compare
#' @param text_class a character value. The title response varibale class legend
#' @param graph boolean. Should the graphics be plotted ?
#'
#' @return a list with four elements
#' \item{models}{the different models fitted with some statistics}
#' \item{roc}{statistics about ROC}
#' \item{graph_pred}{the  graphic of predictions}
#' \item{graph_roc}{the graphic of ROC}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr enquo quos quo_name mutate group_by select arrange
#' @importFrom tibble as_tibble
#' @importFrom purrr map map_dbl
#' @importFrom stats setNames
#' @importFrom tidyr gather nest unnest
#' @importFrom rlang !! !!! is_true
#' @importFrom graphics plot
#' @importFrom readr parse_factor
#' @importFrom RColorBrewer brewer.pal
#' @importFrom forcats fct_reorder
#' @importFrom pROC ci
#' @importFrom tidyselect everything
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' #Find an example
compare_prediction <- function(df, ..., response, success, level = NULL, pal, text_class = NULL, graph = TRUE) {

  # get the quosure of arguments
  tocompare <- quos(...)
  response <- enquo(response)

  # Some parameters
  if (is.null(level)) {
    vec <- name <- select(df, !! response) %>%
      unlist() %>%
      unique()
    level <- setNames(vec, name)
  }
  methods_name <- select(df, !!! tocompare) %>% colnames()
  if (is.null(text_class)) text_class = "Class"
  my_y <- sym(names(level)[level %in% success])

  # Data manipulation
  df <- as_tibble(df) %>%
    mutate(fact_response = ifelse(!! response == success, names(level)[level %in% success], names(level)[!level %in% success])) %>%
    mutate(fact_response = parse_factor(fact_response, levels = c(names(level)[level %in% success], names(level)[!level %in% success]))) %>%
    mutate(prob_response = ifelse(!! response == success, 1, 0)) %>%
    gather(key = "methods", value = "value", !!! tocompare)
  min_pred <- min(select(df, value), na.rm = TRUE)
  max_pred <- max(select(df, value), na.rm = TRUE)
  new_data <- tibble(value = seq(min_pred, max_pred, length.out = 1000))

  # Models and statistics
  dfcomp <- df %>%
    group_by(methods) %>%
    nest() %>%
    mutate(model = map(data, ~train_mod(xdata = as.data.frame(.[, "value"]), yvec = .$fact_response))) %>%
    mutate(confusion = map(model, get_confMat)) %>%
    mutate(roc = map(model, get_roc, success = names(level)[level %in% success])) %>%
    mutate(auc = map(roc, ci)) %>%
    mutate(AUC = map_dbl(auc, ~.[2])) %>%
    mutate(prediction = map(model, get_prediction, newdata = new_data, type = "prob")) %>%
    mutate(senspe = map(roc, get_senspe))

  # graph of prediction
  ggp1 <- ggplot() +
    geom_jitter(data = unnest(dfcomp, data), aes(x = value, y = prob_response, color = fact_response), height = 0.05, width = 0) +
    geom_line(data = unnest(dfcomp, prediction), aes(x = value, y = !! my_y)) +
    facet_wrap(~methods, nrow = ceiling(nrow(dfcomp)/3),
               labeller = as_labeller(setNames(map(tocompare, quo_name), map(tocompare, quo_name)))) +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(face = "bold", size = 11),
          axis.title = element_text(face = "bold", size = 13),
          strip.text = element_text(face = "bold", size = 13),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 13)) +
    scale_color_manual(values = brewer.pal(3, "Set1")[1:2], name = text_class) +
    labs(y = "Probabilities", x = "Predictor values")

  # Graph of ROC
  ggp2 <- dfcomp %>%
    unnest(senspe) %>%
    mutate(methods = parse_factor(methods, levels = methods_name)) %>%
    group_by(methods) %>%
    arrange(sensi) %>%
    ggplot(aes(x = 1 - speci, y = sensi, color = fct_reorder(methods, AUC, .desc = TRUE))) +
    geom_line() +
    theme_classic() +
    theme(axis.text = element_text(face = "bold", size = 11),
          axis.title = element_text(face = "bold", size = 13),
          legend.title = element_text(face = "bold", size = 13)) +
    labs(x = "1 - specificities", y = "sensibilities") +
    scale_color_manual(values = pal, name = "Methods")

  if (is_true(graph)) {
    plot(ggp1)
    plot(ggp2)
  }

  # Stats about ROC
  dfrocstat <- dfcomp %>%
    mutate(confMat = map(confusion, ~as_tibble(t(.$byClass)))) %>%
    select(methods, auc, confMat) %>%
    mutate(lowerAUC = map_dbl(auc, ~.[1])) %>%
    mutate(AUC = map_dbl(auc, ~.[2])) %>%
    mutate(upperAUC = map_dbl(auc, ~.[3])) %>%
    select(-auc) %>%
    unnest(confMat) %>%
    select(-Precision, -Recall, -F1, -'Balanced Accuracy', -'Detection Rate') %>%
    rename(Methods = methods, PPV = 'Pos Pred Value', NPV = 'Neg Pred Value') %>%
    select(Methods, AUC, lowerAUC, upperAUC, everything())


  #Graph of AUC
  ggp3 <- ggplot(dfrocstat) +
    geom_segment(aes(x = lowerAUC, xend = upperAUC,
                     y = fct_reorder(Methods, AUC), yend = fct_reorder(Methods, AUC))) +
    geom_point(aes(x = AUC, y = fct_reorder(Methods, AUC)), shape = 21, size = 3, fill = "black") +
    geom_point(aes(x = lowerAUC, y = fct_reorder(Methods, AUC)), shape = 124, size = 3) +
    geom_point(aes(x = upperAUC, y = fct_reorder(Methods, AUC)), shape = 124, size = 3) +
    theme_classic() +
    theme(axis.text = element_text(face = "bold", size = 11),
          axis.title = element_text(face = "bold", size = 13),
          strip.text = element_text(face = "bold", size = 13)) +
    labs(x = "AUC", y = "Methods") +
    xlim(0.5, 1)

  return(list(models = dfcomp, roc = dfrocstat, graph_pred = ggp1, graph_roc = ggp2, graph_auc = ggp3))


}

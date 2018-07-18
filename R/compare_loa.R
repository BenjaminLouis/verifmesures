#' Title
#'
#' @param df a tibble or a dataframe
#' @param ... unquoted names of variable to compare
#' @param reference unquoted name of reference variable to compare with
#' @param IC_level a number. Value of the confidence intervals level
#' @param x_text graphic x axis label. If NULL, nothing is displayed
#' @param y_text graphic y axis label. If NULL, name of reference variable is displayed
#' @param strip_text graphic strip text. IF NULL, names of variables in ... arg are displayed
#' @param graph boolean. Should the graphic be plotted ?
#'
#' @return a list with three elements
#' \item{data}{the data with call data and graphic quantities}
#' \item{loa}{statistics about limits of agreament}
#' \item{graphic}{the ggplot graphic}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr enquo quos quo_name mutate group_by select summarise
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @importFrom stats qt setNames sd
#' @importFrom tidyr gather
#' @importFrom rlang !! !!! is_true
#' @importFrom graphics plot
#' @importFrom readr parse_factor
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' #find an example
compare_loa <- function(df, ..., reference, IC_level = 0.95, x_text = NULL, y_text = NULL, strip_text = NULL, graph = TRUE) {

  # get the quosure of arguments
  reference <- enquo(reference)
  tocompare <- quos(...)

  # transform data as tibble
  df <- as_tibble(df)

  # default graphics text
  if (is.null(strip_text)) strip_text <- map(tocompare, quo_name)

  # data for plotting limit of agreament graphics
  dfstat <- df %>%
    gather(key = "methods", value = "value", !!! tocompare) %>%
    mutate(methods = parse_factor(methods, levels = unlist(map(tocompare, quo_name)))) %>%
    mutate(difference = value - !! reference) %>%
    mutate(average = (!! reference + value)/2)

  # statistics about limits of agreament
  dfloa <- dfstat %>%
    group_by(methods) %>%
    summarise(bias = mean(difference, na.rm = TRUE),
              biasStdDev = sd(difference, na.rm = TRUE),
              biasStdErr = sd(difference, na.rm = TRUE)/sqrt(length(difference)),
              CI_bias_multiplier = qt((1 + IC_level)/2, length(difference) - 1),
              upperbias = bias + CI_bias_multiplier*biasStdErr,
              lowerbias = bias - CI_bias_multiplier*biasStdErr,
              CI_LoA_multiplier = qt((1 + IC_level)/2, length(difference) - 1)*sqrt((length(difference) + 1)/length(difference)),
              upperLoA = bias + CI_LoA_multiplier*biasStdDev,
              lowerLoA = bias - CI_LoA_multiplier*biasStdDev) %>%
    mutate(methods = parse_factor(methods, levels = unlist(map(tocompare, quo_name)))) %>%
    mutate(rangeLoA = upperLoA - lowerLoA) %>%
    select(-CI_bias_multiplier, -CI_LoA_multiplier)

  # build the graphic
  ggp <- ggplot() +
    geom_hline(data = dfloa, aes(yintercept = bias), size = 1) +
    geom_hline(data = dfloa, aes(yintercept = lowerLoA), linetype = "solid") +
    geom_hline(data = dfloa, aes(yintercept = upperLoA), linetype = "solid") +
    geom_hline(data = dfloa, aes(yintercept = 0), linetype = "dashed") +
    geom_point(data = dfstat, aes(x = average, y = difference), color = "gray60") +
    facet_wrap(~methods, nrow = ceiling(nrow(dfloa)/3),
               labeller = as_labeller(setNames(strip_text, map(tocompare, quo_name)))) +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(face = "bold", size = 11),
          axis.title = element_text(face = "bold", size = 13),
          strip.text = element_text(face = "bold", size = 13)) +
    labs(x = x_text, y = y_text)
  # plot graphic if true
  if (is_true(graph)) plot(ggp)

  return(data = dfstat, loa = dfloa, graph = ggp)

}


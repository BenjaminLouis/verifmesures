#' Function to compare reproducility and repeatability of different measurement methods through the calcul of Intraclass Correlation Coefficient (ICC)
#'
#' @param df a tibble
#' @param ... unquoted names of column to compare
#' @param subjects unquoted name of subject column
#' @param observers unquoted name of observers/raters column
#' @param repetitions unquoted name of repetitions column
#' @param which_repetition a character string. The code of repetitions to use for reproducibility.
#' @param which_observer a character string. The code of observers/raters to use for repeatability.
#' @param graph boolean. Should the graphics be plotted ?
#'
#' @return a list with two element
#' \item{Reproducibility}{a list with two element : the tibble and the ggplot graphic of results}
#' \item{Repeatability}{a list with two element : the tibble and the ggplot graphic of results}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr enquo quos mutate group_by select filter
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @importFrom tidyr gather spread nest unnest
#' @importFrom rlang !! !!! is_true
#' @importFrom graphics plot
#' @importFrom forcats fct_reorder
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' #find example
compare_rr <- function(df, ..., subjects, observers, repetitions, which_repetition, which_observer, graph = TRUE) {

  df <- as_tibble(df)

  # get the quosure of arguments
  tocompare <- quos(...)
  subjects <- enquo(subjects)
  observers <- enquo(observers)
  repetitions <- enquo(repetitions)

  # Reproducibility
  df_repro <- df %>%
    filter(!! repetitions == which_repetition) %>%
    select(-!! repetitions) %>%
    gather(key = "methods", value = "value", !!! tocompare) %>%
    group_by(methods) %>%
    spread(key = !! observers, value = value) %>%
    nest(-methods, -!! subjects) %>%
    mutate(icc = map(data, ~get_icc(., model = "twoway", type = "agreement", unit = "single"))) %>%
    unnest(icc) %>%
    select(-data)

  ggp_repro <- ggplot(df_repro) +
    geom_segment(aes(x = lowerICC, xend = upperICC,
                     y = fct_reorder(methods, ICC), yend = fct_reorder(methods, ICC))) +
    geom_point(aes(x = ICC, y = fct_reorder(methods, ICC)), shape = 21, size = 3, fill = "black") +
    geom_point(aes(x = lowerICC, y = fct_reorder(methods, ICC)), shape = 124, size = 3) +
    geom_point(aes(x = upperICC, y = fct_reorder(methods, ICC)), shape = 124, size = 3) +
    theme_classic() +
    theme(axis.text = element_text(face = "bold", size = 11),
          axis.title = element_text(face = "bold", size = 13),
          strip.text = element_text(face = "bold", size = 13)) +
    labs(x = "ICC (Reproducibility)", y = "Methods") +
    xlim(0, 1)

  # Repeatability
  df_repeat <- df %>%
    filter(!! observers == which_observer) %>%
    select(-!! observers) %>%
    gather(key = "methods", value = "value", !!! tocompare) %>%
    group_by(methods) %>%
    spread(key = !! repetitions, value = value) %>%
    nest(-methods, -!! subjects) %>%
    mutate(icc = map(data, ~get_icc(., model = "oneway", type = "agreement", unit = "single"))) %>%
    unnest(icc) %>%
    select(-data)


  ggp_repeat <- ggplot(df_repeat) +
    geom_segment(aes(x = lowerICC, xend = upperICC,
                     y = fct_reorder(methods, ICC), yend = fct_reorder(methods, ICC))) +
    geom_point(aes(x = ICC, y = fct_reorder(methods, ICC)), shape = 21, size = 3, fill = "black") +
    geom_point(aes(x = lowerICC, y = fct_reorder(methods, ICC)), shape = 124, size = 3) +
    geom_point(aes(x = upperICC, y = fct_reorder(methods, ICC)), shape = 124, size = 3) +
    theme_classic() +
    theme(axis.text = element_text(face = "bold", size = 11),
          axis.title = element_text(face = "bold", size = 13),
          strip.text = element_text(face = "bold", size = 13)) +
    labs(x = "ICC (Repeatability)", y = "Methods") +
    xlim(0, 1)

  if (is_true(graph)) {
    plot(ggp_repro)
    plot(ggp_repeat)
  }

  return(list(Reproducibility = list(result = df_repro, graphic = ggp_repro),
              Repeatability = list(result = df_repeat, graphic = ggp_repeat)))

}

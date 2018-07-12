
#' Title
#'
#' @param df a tibble or a dataframe
#' @param ... unquoted names of variable to compare
#' @param reference unquoted name of reference variable to compare with
#' @param x_text graphic x axis label. If NULL, nothing is displayed
#' @param y_text graphic y axis label. If NULL, name of reference variable is displayed
#' @param strip_text graphic strip text. IF NULL, names of variables in ... arg are displayed
#' @param graph boolean. Does the graph should be plotted ?
#'
#' @return a list with two element
#' \item{models}{a tibble with models estimatoin statistics}
#' \item{graphic}{the ggplot graphic}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr enquo quos quo_name bind_cols rename mutate group_by
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @importFrom stats as.formula confint lm setNames
#' @importFrom stringr str_c
#' @importFrom broom tidy glance
#' @importFrom tidyr gather unite spread nest unnest
#' @importFrom rlang !! !!! is_true
#' @importFrom graphics plot
#' @importFrom utils data
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' compare_values(df = mtcars, hp, drat, wt, reference = disp)

compare_values <- function(df, ..., reference, x_text = NULL, y_text = NULL, strip_text = NULL, graph = TRUE) {

  # get the quosure of arguments
  reference <- enquo(reference)
  tocompare <- quos(...)

  # transform data as tibble
  df <- as_tibble(df)

  # default graphics text
  if (is.null(x_text)) x_text = ""
  if (is.null(y_text)) y_text = quo_name(reference)
  if (is.null(strip_text)) strip_text = map(tocompare, quo_name)

  # formula of linear model
  forms <- as.formula(str_c(quo_name(reference), " ~ value"))
  # function to get statistics about model estimation
  f <- function(x) {
    bind_cols(
      as_tibble(tidy(x)) %>%
        rename(p.value.coef = p.value,
               statistic.coef = statistic) %>%
        mutate(term = ifelse(term == "value", "slope", "intercept")),
      as_tibble(confint(x))) %>%
      rename(lower = '2.5 %', upper = '97.5 %') %>%
      gather(key, value, -term) %>%
      unite(stat, term, key) %>%
      spread(key = stat, value = value)
  }
  # get the linear models to compare
  dfmod <- df %>%
    gather(key = "key", value = "value", !!! tocompare) %>%
    group_by(key) %>%
    nest() %>%
    mutate(model = map(data, ~lm(forms, data = .x))) %>%
    mutate(summod = map(model, glance)) %>%
    unnest(summod) %>%
    mutate(coefmod = map(model, f)) %>%
    unnest(coefmod, .drop = FALSE)

  # build the graphics
  ggp <- df %>%
    gather(key = "key", value = "value", !!! tocompare) %>%
    ggplot(aes(x = value, y = !! reference)) +
    geom_point(color = "gray") +
    geom_smooth(method = "lm", color = "black") +
    facet_wrap(~key, scales = "free_x", nrow = ceiling(nrow(dfmod)/3),
               labeller = as_labeller(setNames(strip_text, map(tocompare, quo_name)))) +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(face = "bold", size = 11),
          axis.title = element_text(face = "bold", size = 13),
          strip.text = element_text(face = "bold", size = 13)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = x_text, y = y_text)
  # plot graph if true
  if (is_true(graph)) plot(ggp)

  return(list(models = dfmod, graphic = ggp))

}


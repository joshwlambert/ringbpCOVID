#' Plot the percentage of outbreaks controlled again the percentage of
#' contacts traced for various outbreak parameters
#'
#' @inheritParams ggplot2::ggplot
#' @param var The variable that is being plotted, i.e. which variable is being
#' split by colour.
#'
#' @importFrom rlang .data
#'
#' @return A `ggplot` object.
#' @autoglobal
#' @export
plot_outbreak_control <- function(data, var) {
  ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = prop_ascertain,
      y = pext,
      colour = as.factor(.data[[var]])
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(linetype = version),
      size = 0.75
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = as.factor(.data[[var]]), shape = version),
      colour = "black",
      size = 3,
      stroke = 1
    ) +
    ggplot2::scale_x_continuous(
      name = "Contacts traced (%)",
      breaks = seq(0, 1, 0.2),
      labels = seq(0, 100, 20)
    ) +
    ggplot2::scale_y_continuous(
      name = "Simulated outbreaks controlled (%)",
      breaks = seq(0, 1, 0.2),
      labels = seq(0, 100, 20)
    ) +
    ggplot2::scale_shape_manual(values = c(21, 23)) +
    ggplot2::guides(shape = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}

#' @importFrom tidyr gather
#' @import ggplot2
#' @export
autoplot.psvar <- function(x) {
  y <- as.data.frame(x$irs[,,1])
  y$irhor <- 1:nrow(y)
  
  tidyr::gather(y, name, value, -irhor, factor_key = TRUE) %>%
    ggplot(aes(irhor, value)) +
    geom_line() +
    facet_wrap(~name, scales = "free_y") +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      axis.title = element_blank()
    )
}
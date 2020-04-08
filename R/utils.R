#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
format_long  <- function(x) {
  fct_lvl <- colnames(x)
  as.data.frame(x) %>%
    mutate(horizon = 1:nrow(.)) %>%
    pivot_longer(-horizon, names_ptypes = list(name = factor(levels = fct_lvl)))
}

#' @importFrom purrr reduce map
#' @importFrom glue glue
array_to_list <- function(x, margin = 3) {
  out <- apply(x, margin, list) %>%
    reduce(c)
  # set_names(paste0("h", 1:length(.)))
  map(out, ~ `class<-`(.x, glue::glue("wb({dim(x)[3]})")))
}
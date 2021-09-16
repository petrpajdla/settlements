# model-based clustering

#' Model-based clustering
#'
#' @param x A nested PCA result.
#' @param prob A threshold of cumulative variability explained.
#'     Only PCs that cumulatively explain at least the given value are kept.
#'     Defaults to 0.75.
#'
#' @return A nested \code{tibble} with \code{mclust} objects and classification
#'     for individual points.
#' @export
#'
#' @examples
mbc_cluster <- function(x, prob = 0.75) {
  mb_clustering_neo2 <- x %>%
    mutate(
      # select only PCs that cumulatively explain given variability
      overProb = purrr::map(sdev, \(x) x[(cumsum(x[, "var_prop"]) >= prob), ]$pc[1]),
      overProb = purrr::map(overProb, \(x) 1:x),
      # select PCs
      x2 = purrr::map2(x, overProb, \(x, y) x[, c(y)]),
      # model-based clustering
      bic = purrr::map(x2, \(x) mclust::mclustBIC(x)),
      bic_sum = purrr::map(bic, \(x) mclust::summary.mclustBIC(x)),
      # select best model based on BIC
      bic_best = purrr::map(bic_sum, \(x) names(x[1])),
      bic_best = purrr::map(bic_best, \(x) unlist(strsplit(x, ","))),
      mclust = purrr::pmap(list(x2, bic_best, bic),
                           \(data, model, bic)
                           mclust::Mclust(data = data,
                                          G = as.numeric(model[[2]]),
                                          modelNames = model[[1]],
                                          x = bic)),
      classif = purrr::map(mclust, \(x) x$classification),
      classif = purrr::map2(id, classif, \(x, y) bind_cols(id = x, g = y))
    )
}

#' Classification based on model-based clustering
#'
#' @param x Nested object, result of \code{mbc_cluster}.
#'
#' @return
#' @export
#'
#' @examples
mbc_classify <- function(x, neo) {
  x %>%
    select(classif) %>%
    filter(neo == neo) %>%
    unnest() %>%
    ungroup(c(reg, neo)) %>%
    mutate(g = str_c(reg, g))
}


#' Plot a map of groups resulting from model based clustering
#'
#' @param x Classification resulting from \code{mbc_classify}.
#' @param sf
#' @param neo
#'
#' @return
#' @export
#'
#' @examples
mbc_map <- function(x, sf) {
  x %>%
    left_join(sf , by = "id") %>%
    st_as_sf() %>%
    ggplot() +
    geom_sf(data = admin1, fill = "white", color = "gray") +
    geom_sf(data = rivers0, color = "lightblue") +
    geom_sf(aes(color = g), show.legend = FALSE) +
    facet_wrap(vars(g)) +
    theme_void() +
    labs(title = neo)
}


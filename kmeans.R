#' Naive implementation of kmeans.
#' @param data0 matrix / dataframe.
#' @param k integer; number of clusters.
#' @param maxiter integer; maximum number of iterations.
naive_kmeans <- function(data0, k, centers, maxiter = 100, tol = 1e-5) {
  data_dim <- nrow(data0)
  if (missing(centers)) centers <- data0[sample(data_dim, k), ]
  labels <- assign_labels(data0, centers)
  
  count <- 0
  update <- Inf
  while ((count <= maxiter) & (update >= tol)) {
    new_centers <- compute_centers(data0, labels)
    labels <- assign_labels(data0, centers)
    
    count <- count + 1
    update <- max(abs(centers - new_centers))
    centers <- new_centers
  }
  
  cat("The algorithm converges in", count, "iterations.\n")
  list(data = data0, labels = labels, centers = centers)
}


compute_centers <- function(data0, labels) {
  unique_labels <- unique(labels)
  data1 <- data0 %>% data.frame(labels)
  purrr::map(
    .x = unique_labels,
    .f = function(.x) {
      data1 %>% 
        dplyr::filter(labels == .x) %>% 
        apply(2, mean)
    }
  ) %>%
    do.call(rbind, .) %>% data.frame() %>%
    dplyr::select(-labels)
}
assign_labels <- function(data0, centers, dist = l2_distance) {
  row_seq <- seq(nrow(data0))
  row_seq %>% 
    purrr::map_dbl(~data0[.x, ] %>% closest_centers(centers, dist))
}
closest_centers <- function(coord, centers, dist = l2_distance) {
  dist_vec <- fun_vec_and_mat(coord, centers, dist)
  min(which(dist_vec == min(dist_vec)))
}
l2_distance <- function(v1, v2) {
  mean((v1 - v2)^2)
}


fun_vec_and_mat <- function(v1, mat, FUN) {
  row_seq <- seq(nrow(mat))
  row_seq %>% purrr::map_dbl(~FUN(mat[.x, ], v1))
}

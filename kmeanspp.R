#' Initialise centers for kmeans cluster.
#' @param data0 matrix / dataframe; the data.
#' @param k; integer; number of clusters.
#' @return matrix; the centers of clusters.
kmeanspp <- function(data0, k) {
  if (k <= 1) stop("The number of clusters should at least be two.")
  
  index_set <- seq(nrow(data0))
  sampled_index <- index_set %>% sample(1)
  centers <- data0[sampled_index, ]
  
  for (i in 2:k) {
    index_set %<>% setdiff(sampled_index)
    dist_vec <- data0[index_set, ] %>% compute_dist(centers) %>% `^`(2)
    prob_vec <- dist_vec / sum(dist_vec)
    sampled_index <- index_set %>% sample(1, prob = prob_vec)
    centers %<>% rbind(data0[sampled_index, ])
  }
  
  centers
}


compute_dist <- function(mat0, mat1, dist = l2_distance) {
  row_seq <- seq(nrow(mat0))
  purrr::map_dbl(
    .x = row_seq, 
    .f = ~mat0[.x, ] %>% fun_vec_and_mat(mat1, dist) %>% min()
  )
}

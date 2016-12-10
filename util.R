l2_distance <- function(v1, v2) {
  mean((v1 - v2)^2)
}

# FUN maps Rn x Rn to R
fun_vec_and_mat <- function(v1, mat, FUN) {
  if (is.null(nrow(mat))) 
    return(FUN(v1, mat))
  
  row_seq <- seq(nrow(mat))
  row_seq %>% purrr::map_dbl(~FUN(mat[.x, ], v1))
}

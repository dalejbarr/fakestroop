#' Create a vector with unique random seeds.
#'
#' @param n_seeds Number of seeds to create.
#' @returns A vector of randomly-generated integers from 1 to `.Machine$integer.max`.
#' @export
random_seeds <- function(n_seeds) {
  sample(1:.Machine$integer.max, n_seeds)
}

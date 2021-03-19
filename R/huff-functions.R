
library(dplyr)

# With a given OD, compute absolute attractiveness (Aj^a)*(Dij^(-b))

# 1. A OD storing distance between points
# 2. information about the size (attractiveness measure of destinations)


compute_attractiveness = function(size, distance, exponent_coef = 2) {
  return( size / (distance) ^ exponent_coef)
}

# size/attractiveness of the destination: a df with "origin id", "destination id", "distance"
# size: a df with columns of "name" and "size"

#' Title
#'
#' @param od_matrix A dataframe of origin-destination (OD) matrix in long format with column names of "origin", "destination" and "distance"
#' @param size A dataframe storing the size (or other measures for "attractiveness")
#' @param exponent_coef Impedance coefficient (beta)
#'
#' @return the original `od_matrix` with selection probability of each row computed
#' @export
#'
#' @examples
compute_select_prob = function(od_matrix, size, exponent_coef) {

  od_size = left_join(od_matrix, size, by = "destionation_id")

  # TODO: use `compute_attractiveness`
  od_size = od_size %>%
    mutate(attr = size / (distance)^exponent_coef)

  # Get total attractiveness of each residential building
  attr_total = od_size %>%
    group_by(origin) %>%
    summarise(attr_sum = sum(attr))

  # Probability of using that open space
  od_prob =
    merge(df_area, attr_total, by = "origin", all.x = TRUE) %>%
    mutate(prob = attr / attr_sum)

  return(od_prob)
}



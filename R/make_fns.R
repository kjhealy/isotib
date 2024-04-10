## Gridpoints for a 2D wall



#' Make the grid points for an inso 'wall'
#'
#' @param width n columns in the wall
#' @param height n rows in the wall
#' @param z z (3rd dimension) position of the wall
#' @param fill Color of the wall
#'
#' @return A dataframe of coordinates and color fill
#' @export
#'
#' @examples
make_wall <- function(width, height, z = 0, fill = "gray90") {
  coords <- expand.grid(x=1:width, y=1:height, z=z)
  coords$fill <- fill
  coords
}


#' Make a vector of text labels
#'
#' @param labels Character vector of labels
#' @param width Width of the wall
#' @param height Height of the wall
#' @param z Z-position
#' @param color Color
#'
#' @return A vector of labels
#' @export
#'
#' @examples
make_textvec <- function(labels, width, height, z = 0, color = "gray50") {
  coords <- expand.grid(x=width, y=height, z=z)
  coords$color <- color
  coords$label <- labels
  coords
}


#' ISO coords
#'
#' @param width width
#' @param height height
#' @param depth block depth
#' @param fill Fill color
#'
#' @return Vector of brick coordinates
#' @export
#'
#' @examples
make_brick <- function(width, height, depth, fill = "gray90") {
  coords <- expand.grid(x=1:width, y=1:height, z=1:depth)
  coords$fill <- fill
  coords
}

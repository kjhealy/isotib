#' Paint x vec
#'
#' @param object x
#' @param range x
#' @param col x
#'
#' @return x
#' @export
#'
#' @examples
paint_x <- function(object, range, col) {
  ind <- object$x %in% range
  object$fill[ind] <- col
  object
}

#' Paint y
#'
#' @param object x
#' @param range x
#' @param col x
#'
#' @return x
#' @export
#'
#' @examples
paint_y <- function(object, range, col) {
  ind <- object$y %in% range
  object$fill[ind] <- col
  object
}

#' Paint xy
#'
#' @param object x
#' @param xrange x
#' @param yrange x
#' @param col x
#'
#' @return x
#' @export
#'
#' @examples
paint_xy <- function(object, xrange, yrange, col) {
  indx <- object$x %in% xrange
  indy <- object$y %in% yrange
  object$fill[indx & indy] <- col
  object
}

#' Delete on x
#'
#' @param object x
#' @param remove x
#'
#' @return x
#' @export
#'
#' @examples
delete_x <- function(object, remove) {
  object[!object$x %in% remove,]
}

#' Delete on y
#'
#' @param object x
#' @param remove x
#'
#' @return x
#' @export
#'
#' @examples
delete_y <- function(object, remove) {
  object[!object$y %in% remove,]
}


#' Draw a column
#'
#' @param xpos x
#' @param ncubes x
#' @param fill x
#' @param darkenby x
#' @param z x
#' @param delete_x x
#' @param delete_y x
#' @param light x
#' @param ysize x
#'
#' @return x
#' @export
#'
#' @examples
draw_column <- function(xpos, ncubes = 10,
                        fill = "gray80", darkenby = 0.05, z = 0,
                        delete_x = NULL,
                        delete_y = NULL,
                        light = "left-top",
                        ysize = 1/ncubes) {
  coords <- expand.grid(x=xpos, y=1:ncubes, z=z)
  coords$fill <- fill

  if(is.null(delete_x) & is.null(delete_y)) {
    coords |>
      isocubesGrob(darkenby = darkenby, light = light) |>
      grid::grid.draw()
  } else {
    coords |>
      delete_x(delete_x) |>
      delete_y(delete_y) |>
      isocubesGrob(darkenby = darkenby, light = light, ysize = ysize) |>
      grid::grid.draw()
  }
}

#' Draw a row
#'
#' @param ypos x
#' @param ncubes x
#' @param fill x
#' @param darkenby x
#' @param z x
#' @param delete x
#' @param light x
#' @param ysize x
#'
#' @return x
#' @export
#'
#' @examples
draw_row <- function(ypos, ncubes = 10, fill = "gray80",
                     darkenby = 0.05, z = 0,
                     delete = NULL,
                     light = "left-top",
                     ysize = 1/ncubes) {
  coords <- expand.grid(x=1:ncubes, y=ypos, z=z)
  coords$fill <- fill
  if(is.null(delete)) {
    coords |>
      isocubesGrob(darkenby = darkenby, light = light) |>
      grid::grid.draw()
  } else {
    coords |>
      delete_y(delete) |>
      isocubesGrob(darkenby = darkenby, light = light,
                             ysize = ysize) |>
      grid::grid.draw()
  }
}


#' Draw on z
#'
#' @param zpos x
#' @param ncubes x
#' @param fill x
#' @param darkenby x
#'
#' @return x
#' @export
#'
#' @examples
draw_z <- function(zpos, ncubes = 10, fill = "gray80", darkenby = 0.05) {
  coords <- expand.grid(x=1, y=1:ncubes, z=zpos)
  coords$fill <- fill
  coords |>
    isocubesGrob(darkenby = darkenby) |>
    grid::grid.draw()
}



#' Get vector of labels
#'
#' @param labels x
#' @param coords x
#'
#' @return x
#' @export
#'
#' @examples
get_labels <- function(labels, coords) {
  if (!is.null(labels))
    labels
  else if (hasName(coords, 'label'))
    coords$label
  else if (hasName(coords, 'labels'))
    coords$labels
  else
    "No label"
}

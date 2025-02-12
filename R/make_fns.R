## Gridpoints for a 2D wall



#' Make the grid points for an iso 'wall'
#'
#' @param width n columns in the wall
#' @param height n rows in the wall
#' @param z z (3rd dimension) position of the wall
#' @param fill Color of the wall
#'
#' @return A dataframe of coordinates and color fill
#' @export
#'
#' @examples \dontrun{
#' }
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
#' @examples \dontrun{
#' }
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
#' @examples \dontrun{
#' }
make_brick <- function(width, height, depth, fill = "gray90") {
  coords <- expand.grid(x=1:width, y=1:height, z=1:depth)
  coords$fill <- fill
  coords
}

#' Trim all border whitespace in a figure already saved to a file
#'
#' @param x Filename
#' @param quiet Shut up
#'
#' @returns Converts the figure in place
#' @export
#'
#' @examples \dontrun{
#' }
my_plot_crop <- function (x, quiet = TRUE)
{
  is_pdf = grepl("[.]pdf$", x, ignore.case = TRUE)
  x2 = x
  x = path.expand(x)
  if (is_pdf && !knitr:::has_utility("pdfcrop"))
    return(x2)
  if (!quiet)
    message("cropping ", x)
  if (is_pdf) {
    system2("pdfcrop", shQuote(c("--margins", "1", x, x)), stdout = if (quiet)
      FALSE
      else "")
  }
  else if (xfun::loadable("magick")) {
    img <-  magick::image_read(x)
    img <- magick::image_trim(img)
    magick::image_write(magick::image_border(img, "white", "10x10"), x)
  }
  else message("The magick package is required to crop \"",
               x2, "\" but not available.")
  x2
}

#' Draw a batch of prepared isocubes
#'
#' Take an object produced by e.g. `make_wall()` and draw it
#'
#' @param obj A df with all the coordinate and fill information in it
#' @param ... Arguments passed on to `isocubesGrob()`
#'
#' @returns An isotib graphic
#' @export
#'
#' @examples \dontrun{
#' }
drawcubes <- function(obj,...) {

  df <- obj[,c("x", "y", "z")]
  fills <- obj[,"fill"]
  out <- isocubesGrob(..., coords = df,
               fill = fills,
               fill2 = NULL,
               fill3 = NULL,
               light = 'top-left',
               verbose = FALSE)

  out_width <- grid::convertWidth(grid::grobWidth(out), "npc", valueOnly = TRUE)
  out_height <- grid::convertHeight(grid::grobHeight(out), "npc", valueOnly = TRUE)

  # Add padding
  padding <- 0.05
  vp_width <- out_width + padding
  vp_height <- out_height + padding

  vp <- viewport(width = vp_width, height = vp_height, just = "center")
  out$vp <- vp
  out <- gridExtra::arrangeGrob(out)
  out
}


#' Draw text for labels
#'
#' @param coords Dataframe of x,y,z coordinates
#' @param label Label vector
#' @param fill Fill color
#' @param fill2 Fill color
#' @param fill3 Fill color
#' @param light Light source char
#' @param darkenby Darken
#' @param ysize x
#' @param xo x
#' @param yo x
#' @param orient x
#' @param verbose x
#' @param fontsize x
#' @param just x
#' @param ... x
#'
#' @returns textgrob
#' @export
#'
#' @examples \dontrun{
#' }
draw_text <- function(coords, label = NULL, fill = NULL, fill2 = NULL,
                    fill3 = NULL, light = 'top-left', darkenby = 0.1,
                    ysize = 1/25, xo = 0.5, yo = ysize, orient = "col",
                    verbose = FALSE, fontsize = 9, just = "center", ...) {

  if (nrow(coords) == 0) {
    return(grid::nullGrob())
  }

  fill <- get_fill(fill, coords)
  labels <- get_labels(label, coords)

  sf <- 1/ysize # Scale-factor

  # depth sort the cubes
  coords$x <- as.integer(round(coords$x))
  coords$y <- as.integer(round(coords$y))
  coords$z <- as.integer(round(coords$z))

  sort_order <- with(coords, order(-x, -z, y))
  coords     <- coords[sort_order,]

  # which cubes are actually visible
  Norig <- nrow(coords)
  visible <- visible_cubes(coords)
  if (verbose) message("Visible cubes: ", sum(visible), " / ", nrow(coords))
  coords  <- coords[visible,]

  #
  # Prepare the fill colours
  #
  if (length(fill) == 1) {
    fill <- rep(fill, Norig)
  } else if (length(fill) != Norig) {
    stop("'fill' must be length = 1 or N")
  }

  # Rearrange colours to match depth-sorted cubes
  fill <- fill[sort_order]
  fill <- fill[visible]
  N    <- nrow(coords)

  # rearrange the colour vector to match the polygons being drawn,
  # i.e. (fill, fill_L, fill_R, fill, fill_L, fill_R, ...)
  # Polygons for faces are always drawn TOP, LEFT, then RIGHT
  # colors <- as.vector(rbind(fill, fill2, fill3))
  colors <- switch (
    light,         #               TOP ,  LEFT, RIGHT
    'top-left'   = as.vector(rbind(fill , fill2, fill3)),
    'top-right'  = as.vector(rbind(fill , fill3, fill2)),
    'left-top'   = as.vector(rbind(fill2, fill , fill3)),
    'left-right' = as.vector(rbind(fill3, fill , fill2)),
    'right-top'  = as.vector(rbind(fill2, fill3, fill )),
    'right-left' = as.vector(rbind(fill3, fill2, fill )),
    stop("'light' argument is not valid: ", light)
  )

  #
  # Template for the cube at (0, 0, 0)
  #
  theta <- seq(90, 390, 60) * pi/180
  x     <- cos(theta)
  y     <- sin(theta)
  # xall  <- c(x[1], x[2], 0, x[6],  x[2], x[3], x[4], 0,  x[4], x[5], x[6], 0)/sf + xo
  # yall  <- c(y[1], y[2], 0, y[6],  y[2], y[3], y[4], 0,  y[4], y[5], y[6], 0)/sf + yo
  # reduce to just one point for label

  if(orient == "col") {

    xall  <- (c(x[2])/sf + xo) - (ysize/2)
    yall  <- (c(y[2])/sf + yo) - (ysize/3)

  } else if(orient == "row") {

    xall  <- (c(x[1])/sf + xo) - (ysize/2)
    yall  <- (c(y[1])/sf + yo) + (ysize/3)
  } else {
    print("Should not be seeing this.")
  }

  #
  # Calculate the offset coordinates for each cube
  #
  ix <-1/sf * ((coords$x - coords$z) * cos(pi/6))
  iy <-1/sf * ((coords$x + coords$z) * sin(pi/6) + coords$y)


  gp <-  grid::gpar(color = fill, fontsize = fontsize)

  # textcubes <- pmap(lout, my_textgrob, default.units = 'snpc', just = just,
  #                  xall = xall, yall = yall, gp = gp)
  #
  # textcubes

  textcube <- textGrob(
    label = labels,
    x = xall + ix,
    y = yall + iy,
    default.units = 'snpc',
    just = just,
    gp = gp
  )

  textcube
}

my_textgrob <- function(xall = xall,
                        yall = yall,
                        ix = ix,
                        iy = iy,
                        default.units, label, gp = gp, just, ...) {
  textGrob(
    label = label,
    x = xall + ix,
    y = yall + iy,
    default.units = 'snpc',
    just = just,
    gp = gp
  )
}




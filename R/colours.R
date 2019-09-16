#' Make shades
#'
#'Given a colour make n lighter or darker shades.
#'Very interesting package.
#'
#' @param colour The colour to make shades of.
#' @param n The number of shades to make
#' @param lighter Whether to make it lighter (\code{TRUE}) or darker (\code{FALSE})
#'
#' @return A vector of \code{n} colour hex codes
#' @export
#'
#'@details this is the details of the function
#'
#' @examples
#' # Five lighter shades
#' make_shades("goldenrod", 5)
#' # Five darker shades
#' # note: \dontrun or \dontshow could be used
#' make_shades("goldenrod", 5, lighter=FALSE)
make_shades <- function(colour, n, lighter = TRUE) {

  # check the value of n
  #stopifnot(n<1, "n must be at least 1")
  if (n<1) {
    stop("n must be at least 1")
  }
  if (!is.logical(lighter)) {
    stop("lighter must be logical TRUE or FALSE")
  }
  # Convert the colour to RGB
  colour_rgb <- grDevices::col2rgb(colour)[, 1]

  # Decide if we are heading towards white or black
  if (lighter) {
    end <- 255
  } else {
    end <- 0
  }

  # Calculate the red, green and blue for the shades
  # we calculate one extra point to avoid pure white/black
  red <- seq(colour_rgb[1], end, length.out = n + 1)[1:n]
  green <- seq(colour_rgb[2], end, length.out = n + 1)[1:n]
  blue <- seq(colour_rgb[3], end, length.out = n + 1)[1:n]

  # Convert the RGB values to hex codes
  shades <- grDevices::rgb(red, green, blue, maxColorValue = 255)

  return(shades)
}

#' Plot colours
#'
#' Plot a vector of colours to see what they look like
#'
#' @param colours Vector of colour to plot
#'
#' @return A ggplot2 object
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' shades <- make_shades("goldenrod", 5)
#' plot_colours(shades)
plot_colours <- function(colours) {
  plot_data <- data.frame(Colour = colours)

  ggplot2::ggplot(plot_data,
         ggplot2::aes(x = .data$Colour, y = 1, fill = .data$Colour,
             label = .data$Colour)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(angle = "90") +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void() #+
    #ggplot2::geom_text("test")
}

#' make colours and plot them, both lighter and darker
#'
#' @param colour base colour for plot (string)
#' @param numshades number of shades
#'
#' @return
#' @export
#'
#' @examples
plot_colours2<-function(colour, numshades) {

  plot_colours(c(make_shades(colour, numshades, lighter = TRUE),
                     make_shades(colour, numshades, lighter = FALSE)))

}

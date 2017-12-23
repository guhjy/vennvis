#' Distance calculation function
#'
#' @keywords internal
calculateDistance <- function(rx, ry, cov, precision = 1000) {
  # Dalculate distance between centers such that area == cov
  # Function adapted from
  # Weisstein, Eric W. "Circle-Circle Intersection."
  # From MathWorld--A Wolfram Web Resource.
  # http://mathworld.wolfram.com/Circle-CircleIntersection.html
  ds <- seq(0, rx+ry, length.out = precision)
  dvals <- abs(suppressWarnings(sapply(ds, function(d) {
    part1 <- rx^2*acos((d^2 + rx^2 - ry^2)/(2*d*rx))
    part2 <- ry^2*acos((d^2 + ry^2 - rx^2)/(2*d*ry))
    part3 <- sqrt((rx+ry-d) * (d+ry-rx) * (d-ry+rx) * (d+rx+ry)) / 2
    part1 + part2 - part3 - cov
  })))
  ds[which.min(dvals)]
}

#' Visualise the relation between two variables as circles
#'
#' Using a venn diagram where the overlap indicates the covariance between two
#' variables and the area of the circles indicates the variance of each.
#'
#' @param x a base variable (vector)
#' @param y a comparison variable (vector)
#' @param scale whether to scale the variables
#' @param prec the precision of the drawing and the numerical optimisation
#' @param plot whether to plot, default TRUE
#' @param names display the names of the variables, default TRUE
#' @param points plot informative points, default FALSE
#'
#' @export
#'
vennvis <- function(x, y, scale = FALSE, prec = 400, plot = TRUE,
                    names = TRUE, points = FALSE) {
  if (scale) {
    xs <- scale(x)
    ys <- scale(y)
    sx <- sd(xs)
    sy <- sd(ys)
    xy <- abs(cor(x, y))
  } else {
    sx <- sd(x)
    sy <- sd(y)
    xy <- abs(cov(x, y))
  }

  d <- calculateDistance(sx, sy, pi*xy, 2*prec)

  theta <- seq(0, 2*pi, length.out = prec)


  nx <- deparse(substitute(x))
  ny <- deparse(substitute(y))
  if (plot) {
    opt <- par(mar = c(0,0,0,0))
    plot(0, 0, type = "n",
         xlim = c(-1.5*sx, 0.5*sx+sy+d),
         ylim = c(-1.5*sx, 1.5*sx), asp = 1, bty = "L")
    lines(x = sx*cos(theta), y = sx*sin(theta), lwd = 2)
    lines(x = sy*cos(theta)+d, y = sy*sin(theta), lwd = 2)
    if (names) {
      text(x = -sx, y = sx, labels = nx, cex = 1.5)
      text(x = sy+d, y = sy, labels = ny, cex = 1.5)
    }
    if (points)
      points(c(0,d,sx-d, sx+2*sy-d), c(0,0,0,0), pch = 21, bg = "black")
    par(opt)
  }

  out <- list(
    rx = sx,
    ry = sy,
    xy = xy,
    d = d,
    ax = pi*sx^2,
    ay = pi*sy^2,
    axy = pi*xy,
    nx = nx,
    ny = ny,
    scaled = scale,
    prec = prec
  )

  class(out) <- "vennvis"
  invisible(out)
}


#' Visualise the relation between two variables as circles
#'
#' Using a venn diagram where the overlap indicates the covariance between two
#' variables and the area of the circles indicates the variance of each.
#'
#' @param x a base variable (vector)
#' @param y a comparison variable (vector)
#' @param scale whether to scale the variables
#' @param plot whether to plot, default TRUE
#' @param precision the precision of the drawing
#'
#'
#' @export
#'
vennvis <- function(x, y, z, scale = FALSE,
                    plot = TRUE, precision = 400, axes = FALSE,
                    triangle = FALSE) {
  # check for errors in the arguments
  .args <- as.list(match.call()[-1]) # use inner 1
  check <- do.call(checkArgs, .args)
  if (!is.na(check)) {
    stop(check)
  }

  vv <- calcVennVis(x, y, z, scale, match.call())
  vv$plotpars <- list(
    precision = precision,
    axes = axes,
    triangle = triangle
  )

  if (plot) {
    plot(vv)
    return(invisible(vv))
  } else {
    return(vv)
  }
}

#' Distance calculation function
#'
#' @keywords internal
#'
calcDist <- function(rx, ry, cov, precision = 1000) {
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


#' Vennvis object creation
#'
#' @keywords internal
#'
calcVennVis <- function(x, y, z, scale = FALSE, call = NULL) {

  if (is.null(call)) {
    call <- match.call()
  }

  if (missing(z)) {

    if (scale) {
      x <- as.vector(scale(x))
      y <- as.vector(scale(y))
    }

    # calculate overlap and radii
    cm <- cov(x, y)
    r <- c(sd(x), sd(y))
    names(r) <- c("x", "y")
    dxy <- calcDist(r["x"], r["y"], cm*pi)
    cx <- c(0, 0)
    cy <- c(cx[1] + dxy, cx[2])

    out <- list(
      nvars = 2L,
      distances = list(xy = dxy),
      centers = list(x = cx, y = cy),
      radii = list(x = r["x"], y = r["y"]),
      covar = cm
    )

  } else {

    if (scale) {
      x <- as.vector(scale(x))
      y <- as.vector(scale(y))
      z <- as.vector(scale(z))
    }

    # calculate overlaps and radii
    cm <- cov(cbind(x = x, y = y, z = z))
    r <- sqrt(diag(cm))

    # calculate distances
    dxy <- calcDist(r["x"], r["y"], cm["x", "y"]*pi)
    dxz <- calcDist(r["x"], r["z"], cm["x", "z"]*pi)
    dyz <- calcDist(r["y"], r["z"], cm["y", "z"]*pi)

    # calculate centers
    cx <- c(0, 0)
    cy <- c(cx[1] + dxy, cx[2])

    if (dxz + dyz < dxy) {

      warning("Third center could not be calculated,",
              " 3-circle plot not possible.\n",
              "Possibly, one cov is small and the other two are large.",
              "\nTry cov(cbind(x, y, z)) to inspect.")

      out <- list(
        nvars = 2L,
        distances = list(xy = dxy),
        centers = list(x = cx, y = cy),
        radii = list(x = r["x"], y = r["y"]),
        covar = cov(x, y)
      )

    } else {

      cosax <- (dxz^2+dxy^2-dyz^2)/(2*dxz*dxy)
      sinax <- sin(acos(cosax))
      cz <- c(cx[1] + cosax * dxz, cx[2] - sinax * dxz)

      out <- list(
        nvars = 3L,
        distances = list(xy = dxy, xz = dxz, yz = dyz),
        centers = list(x = cx, y = cy, z = cz),
        radii = list(x = r["x"], y = r["y"], z = r["z"]),
        covar = cm
      )

    }
  }

  # return a vennvis object with the call included
  return(structure(out, call = call, class = "vennvis"))
}

#' Argument checking
#'
#' @keywords internal
#'
checkArgs <- function(...) {
  # TODO: argument checking
  return(NA)
}



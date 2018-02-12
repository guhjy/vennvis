#' Visualise the relation between two variables as circles
#'
#' Using a venn diagram where the overlap indicates the covariance between two
#' variables and the area of the circles indicates the variance of each.
#'
#' @param x a base variable or a matrix/data frame with 2/3 variables
#' @param y a comparison variable
#' @param z an optional third variable
#' @param scale whether to scale the variables
#' @param plot whether to plot, default TRUE
#' @param plotOpts named list of custom plot options; see details
#'
#' @details
#' There are several custom options that can be added to the plotOpts argument:
#' \itemize{
#'   \item precision (400) - the number of points to use to draw the circles
#'   \item axes (FALSE) - whether to draw the (arbitrary) axes
#'   \item triangle (FALSE) - only available with 3 vars: whether to draw a
#'   triangle between the centers of the circles
#' }
#'
#' @examples
#' set.seed(147289)
#' x <- rnorm(100)
#' y <- 0.5*x+rnorm(100, 0.25)
#' vennvis(x, y)
#'
#' @examples
#' z <- 0.3*x+0.4*y+rnorm(100, 0.15)
#' vennvis(x, y, z)
#'
#' @export
#'
vennvis <- function(x, y, z, scale = FALSE, plot = TRUE, plotOpts) {
  # if x is a data frame
  if (is.data.frame(x) || is.matrix(x)) {
    if (ncol(x) < 2 || ncol(x) > 3) {
      stop("Data has ", ncol(x), " cols. Only 2/3 cols allowed!")
    }
    labs <- colnames(x)
    y <- x[,2]
    if (ncol(x) == 3) z <- x[,3]
    x <- x[,1]
  }

  # check for errors in the arguments
  msg <- checkArgs()
  if (!is.na(msg)) {
    stop(msg)
  }

  vv <- calcVennVis(x, y, z, scale, match.call())

  if (!missing(plotOpts)) {
    if (exists("labs") && is.null(plotOpts[["labels"]])) {
      plotOpts[["labels"]] <- labs
    }
    vv$plotpars <- parsePlotOpts(plotOpts)
  } else {
    l <- list()
    if (exists("labs")) l[["labels"]] <- labs
    vv$plotpars <- parsePlotOpts(l)
  }

  if (plot) {
    plot(vv)
    return(invisible(vv))
  } else {
    return(vv)
  }
}

#' Distance calculation function
#'
#' @importFrom stats cov sd
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
    } else {
      x <- as.vector(x)
      y <- as.vector(y)
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
    } else {
      x <- as.vector(x)
      y <- as.vector(y)
      z <- as.vector(z)
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
  pf <- parent.frame()
  if (!is.logical(pf[["plot"]])) return("Plot argument needs TRUE or FALSE")
  if (!is.logical(pf[["scale"]])) return("Scale argument needs TRUE or FALSE")
  if (!is.numeric(pf[["x"]])) return("Enter numeric var for x")
  if (!is.numeric(pf[["y"]])) return("Enter numeric var for y")
  if (!is.name(pf[["z"]]) && !is.numeric(pf[["z"]])) {
    return("Enter numeric var for z")
  }
  return(NA)
}


#' Plot option parsing
#'
#' @keywords internal
#'
parsePlotOpts <- function(opts) {
  defaultOpts <- list(
    precision = 1000,
    axes = FALSE,
    triangle = FALSE,
    labels = c("X", "Y", "Z")
  )
  if (!is.list(opts)) {
    warning("Options not provided as list, falling back to default opts.")
    return(defaultOpts)
  }
  for (n in names(defaultOpts)) {
    if (is.null(opts[[n]])) opts[[n]] <- defaultOpts[[n]]
  }
  return(opts)
}


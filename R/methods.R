#' Print method
#'
#' @param x a vennvis object returned from vennvis() call
#' @param ... any other args
#'
#'
#' @export
print.vennvis <- function(x, ...) {
  cat("VennVis Object\n----------\ncall: ")
  print(attr(x, "call"))
  cat("----------\n")
  str(x, give.attr = FALSE,
      give.head = FALSE,
      no.list = TRUE)
}


#' Replot method
#'
#' @param x a vennvis object returned from vennvis() call
#' @param ... any other args
#'
#' @export
plot.vennvis <- function(x, ...) {
  vv <- x
  if (!vv$plotpars$axes) {
    boxtype <- "n"
    opt <- par(mar = c(0,0,0,0))
  } else {
    boxtype <- "L"
  }
  theta <- seq(0, 2 * pi, length.out = vv$plotpars$precision)

  if (vv$nvars == 2) {

    plot(0, 0, type = "n",
         xlim = c(-1.5 * vv$radii$x,
                  0.5*vv$radii$x + vv$centers$y[1] + vv$radii$y),
         ylim = c(-1.5 * vv$radii$x, 1.5 * vv$radii$x), asp = 1, bty = "L")

    # draw circles
    lines(x = vv$radii$x*cos(theta) + vv$centers$x[1],
          y = vv$radii$x*sin(theta) + vv$centers$x[2], lwd = 2)
    lines(x = vv$radii$y*cos(theta) + vv$centers$y[1],
          y = vv$radii$y*sin(theta) + vv$centers$y[2], lwd = 2)

    # add labels
    # add labels
    text(vv$plotpars$labels[1:2],
         x = c(1.1*vv$radii$x*cos(0.6*pi) + vv$centers$x[1],
               1.1*vv$radii$y*cos(0.4*pi) + vv$centers$y[1]),
         y = c(1.1*vv$radii$x*sin(0.6*pi) + vv$centers$x[2],
               1.1*vv$radii$y*sin(0.4*pi) + vv$centers$y[2]),
         pos = c(2, 4), cex = 2)

  } else {

    plot(0, 0, type = "n",
         xlim = c(-1.5*vv$radii$x,
                  0.5*vv$radii$x + vv$centers$y[1] + vv$radii$y),
         ylim = c(-0.5*vv$radii$x + vv$centers$z[2] - vv$radii$z,
                  max(1.5*vv$radii$x, 0.5*vv$radii$x + vv$radii$y)),
         asp = 1, bty = "L", axes = vv$plotpars$axes, xlab = "", ylab = "")

    # draw circles
    lines(x = vv$radii$x*cos(theta) + vv$centers$x[1],
          y = vv$radii$x*sin(theta) + vv$centers$x[2], lwd = 2)
    lines(x = vv$radii$y*cos(theta) + vv$centers$y[1],
          y = vv$radii$y*sin(theta) + vv$centers$y[2], lwd = 2)
    lines(x = vv$radii$z*cos(theta) + vv$centers$z[1],
          y = vv$radii$z*sin(theta) + vv$centers$z[2], lwd = 2)

    # add labels
    text(vv$plotpars$labels[1:3],
         x = c(1.1*vv$radii$x*cos(0.75*pi) + vv$centers$x[1],
               1.1*vv$radii$y*cos(0.25*pi) + vv$centers$y[1],
               1.1*vv$radii$z*cos(1.75*pi) + vv$centers$z[1]),
         y = c(1.1*vv$radii$x*sin(0.75*pi) + vv$centers$x[2],
               1.1*vv$radii$y*sin(0.25*pi) + vv$centers$y[2],
               1.1*vv$radii$z*sin(1.75*pi) + vv$centers$z[2]),
         pos = c(2, 4, 4), cex = 2)

    # triangle
    if (vv$plotpars$triangle) {
      points(x = c(vv$centers$x[1], vv$centers$y[1], vv$centers$z[1]),
             y = c(vv$centers$x[2], vv$centers$y[2], vv$centers$z[2]),
             pch = 21, bg = "black")
      lines(x = c(vv$centers$x[1], vv$centers$y[1],
                  vv$centers$z[1], vv$centers$x[1]),
            y = c(vv$centers$x[2], vv$centers$y[2],
                  vv$centers$z[2], vv$centers$x[2]))
    }

  }

  if (!vv$plotpars$axes) par(opt)
}


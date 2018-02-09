#' Print method
#'
#' @rdname vennvis-methods
#'
#' @export
print.vennvis <- function(vv) {
  cat("VennVis Object\ncall: ")
  print(attr(vv, "call"))
  str(vv, give.attr = FALSE,
      give.head = FALSE,
      no.list = TRUE)
}


#' Replot method
#'
#' @rdname vennvis-methods
#'
#' @export
plot.vennvis <- function(vv) {
  if (!vv$plotpars$axes) opt <- par(mar = c(0,0,0,0))
  theta <- seq(0,2*pi, length.out = vv$plotpars$precision)

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

  } else {

    plot(0, 0, type = "n",
         xlim = c(-1.5*vv$radii$x,
                  0.5*vv$radii$x + vv$centers$y[1] + vv$radii$y),
         ylim = c(-0.5*vv$radii$x + vv$centers$z[2] - vv$radii$z,
                  1.5*vv$radii$x),
         asp = 1, bty = "L", axes = vv$plotpars$axes, xlab = "", ylab = "")

    # draw circles
    lines(x = vv$radii$x*cos(theta) + vv$centers$x[1],
          y = vv$radii$x*sin(theta) + vv$centers$x[2], lwd = 2)
    lines(x = vv$radii$y*cos(theta) + vv$centers$y[1],
          y = vv$radii$y*sin(theta) + vv$centers$y[2], lwd = 2)
    lines(x = vv$radii$z*cos(theta) + vv$centers$z[1],
          y = vv$radii$z*sin(theta) + vv$centers$z[2], lwd = 2)

    # triangle
    if (vv$plotpars$triangle) {
      points(x = c(cx[1], cy[1], cz[1]),
             y = c(cx[2], cy[2], cz[2]),
             pch = 21, bg = "black")
      lines(x = c(cx[1], cy[1], cz[1], cx[1]),
            y = c(cx[2], cy[2], cz[2], cx[2]))
    }

  }

  if (!vv$plotpars$axes) par(opt)
}


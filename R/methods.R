#' Print method
#'
#' @rdname vennvis-methods
#'
#' @export
print.vennvis <- function(vv) {
  cat(" vennvis info\n")
  str(vv, give.attr = FALSE,
      give.head = FALSE,
      indent.str = " ",
      no.list = TRUE)
}


#' Replot method
#'
#' @rdname vennvis-methods
#'
#' @export
plot.vennvis <- function(vv) {
  sx <- vv$rx
  sy <- vv$ry
  xy <- vv$xy
  prec <- vv$prec
  d <- vv$d
  theta <- seq(0, 2*pi, length.out = prec)
  opt <- par(mar = c(0,0,0,0))
  plot(0, 0, type = "n",
       xlim = c(-1.5*sx, 0.5*sx+sy+d),
       ylim = c(-1.5*sx, 1.5*sx), asp = 1, bty = "L", axes = F)
  lines(x = sx*cos(theta), y = sx*sin(theta), lwd = 2)
  lines(x = sy*cos(theta)+d, y = sy*sin(theta), lwd = 2)
  text(x = -sx, y = sx, labels = vv$nx, cex = 1.5)
  text(x = sy+d, y = sy, labels = vv$ny, cex = 1.5)
  par(opt)
}


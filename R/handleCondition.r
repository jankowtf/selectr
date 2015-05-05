handleCondition <- function(x, xx, n, strict, force, ...) {
  UseMethod("handleCondition")
}
handleCondition.default <- function(x, xx, n, strict, force) {
  if (inherits(xx, "try-error") || force) {
    if (strict %in% c(0, 1, 2)) {
      xx <- x[1:NROW(x)]
    }
    msg <- sprintf("Invalid value for `n`: %s (max. valid: %s)", n, NROW(x))
    if (strict == 1) {
      message(msg)
    } else if (strict == 2) {
      warning(msg, call. = FALSE)
    } else if (strict == 3) {
      stop(msg, call. = FALSE)
    }
  }
  xx
}
handleCondition.data.frame <- function(x, xx, n, strict, force, margin) {
  if (inherits(xx, "try-error") || force) {

    if (margin == 1) {
      xx <- x[1:NROW(x),]
      msg <- sprintf("Invalid value for `n`: %s (max. valid: %s)", n, NROW(x))
    } else {
      xx <- x[,1:NCOL(x)]
      msg <- sprintf("Invalid value for `n`: %s (max. valid: %s)", n, NCOL(x))
    }

    if (strict == 1) {
      message(msg)
    } else if (strict == 2) {
      warning(msg, call. = FALSE)
    } else if (strict == 3) {
      stop(msg, call. = FALSE)
    }
  }
  xx
}

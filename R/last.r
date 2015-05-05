## TODO: implement strictness levels

#' @title
#' Select last element(s)
#'
#' @description
#' Selects the last \code{n} elements of an object.
#'
#' @section Methods:
#' \itemize{
#'  \item{\code{\link[selectr]{last.default}}}
#'  \item{\code{\link[selectr]{last.data.frame}}}
#' }
#'
#' @param x \code{\link{ANY-class}}.
#'  The object from which to select elements.
#' @param ... Further arguments passed to the respective methods that are being
#'  called
#' @return Depends on the actual method being called
#' @example inst/examples/last.r
#' @seealso \code{
#'  \link[selectr]{last.default},
#'  \link[selectr]{last.data.frame}
#' }
#' @template author
#' @template references
#' @export

last <- function(x, ...) {
  UseMethod("last")
}

#' @title
#' Select last element(s)
#'
#' @description
#' Selects the last \code{n} elements of an object. The default method of the
#' convenience wrapper \code{\link[selectr]{last}}.
#'
#' @param x \code{\link{ANY-class}}.
#'  The object from which to select elements.
#' @param n \code{\link{numeric}}.
#'  The number of elements to select from the end
#' @param keep \code{\link{logical}}.
#'  \code{TRUE}: keep elements \code{1} through \code{n - 1} as
#'    attribute \code{keep}.
#'  \code{FALSE}: just return the selected elements.
#' @param strict \code{\link{numeric}}.
#'  \itemize{
#'    \item{\code{0} :} {disregard invalid values for \code{n} without signaling
#'      a condition and return with an object as if the maximum valid \code{n}
#'      had been specified}
#'    \item{\code{1} :} {signal a \code{\link[base]{message}} for invalid
#'      values for \code{n} and
#'      return with an object as if the maximum valid \code{n}
#'      had been specified}
#'    \item{\code{2} :} {signal a \code{\link[base]{warning}} for invalid
#'      values for \code{n} and
#'      return with an object as if the maximum valid \code{n}
#'      had been specified}
#'    \item{\code{3} :} {signal an error (see \code{\link[base]{stop}})
#'      for invalid values for \code{n}}
#'  }
#' @return Subset of \code{x}. Exact value depends on the class of \code{x}.
#' @example inst/examples/last.r
#' @seealso \code{
#'  \link[selectr]{last},
#'  \link[selectr]{last.data.frame}
#' }
#' @template author
#' @template references
#' @export

last.default <- function(
  x,
  n = 1,
  keep = FALSE,
  strict = c(0, 1, 2, 3)
) {
  strict <- as.numeric(match.arg(as.character(strict), c("0", "1", "2", "3")))
  if (is.null(dim(x))) {
    if (n > 0) {
      xx <- try(x[(NROW(x) - n + 1):NROW(x)], silent = TRUE)
      force <- n > NROW(x)
      xx <- handleCondition(x = x, xx = xx, n = n, strict = strict,
        force = force)
      if (keep) xx <- structure(xx, keep = x[1:(NROW(x)+(-n))])
      xx
    } else {
      xx <- try(x[1:(NROW(x)+n)], silent = TRUE)
      force <- n > NROW(x)
      xx <- handleCondition(x = x, xx = xx, n = n, strict = strict,
        force = force)
      if (keep) xx <- structure(xx, keep = x[((NROW(x)-(-n)+1):NROW(x))])
      xx
    }
  } else {
    if (n > 0) {
      xx <- try(x[(NROW(x)-n+1):NROW(x),], silent = TRUE)
      force <- n > NROW(x)
      xx <- handleCondition(x = x, xx = xx, n = n, strict = strict,
        force = force)
      if (keep) xx <- structure(xx, keep = x[1:(NROW(x)+(-n)),])
      xx
    } else {
      xx <- try(x[1:(NROW(x)+n),], silent = TRUE)
      force <- n > NROW(x)
      xx <- handleCondition(x = x, xx = xx, n = n, strict = strict,
        force = force)
      if (keep) xx <- structure(xx, keep = x[((NROW(x)-(-n)+1):NROW(x)),])
      xx
    }
  }
}

#' @title
#' Select last element(s)
#'
#' @description
#' Selects the last \code{n} elements of a \code{data.frame}.
#' Depending on \code{margin} these the last \code{n} \emph{rows} or
#' \emph{columnns}.
#'
#' @param x \code{\link{data.frame}}.
#'  The object from which to select elements.
#' @param n \code{\link{numeric}}.
#'  The number of elements to select from the end See argument \code{margin}.
#' @param keep \code{\link{logical}}.
#'  \code{TRUE}: keep elements \code{1} through \code{n - 1} as
#'    attribute \code{keep}.
#' @param margin \code{\link{numeric}}.
#'  \code{1} stands for \emph{rows}, \code{2} stand for \emph{columns}.
#' @param drop \code{\link{logical}}.
#'  See argument \code{drop} in \code{\link[base]{data.frame}}.
#' @return A \code{data.frame} or a \code{vecor} depending on \code{drop}.
#' @param strict \code{\link{numeric}}.
#'  \itemize{
#'    \item{\code{0} :} {disregard invalid values for \code{n} without signaling
#'      a condition and return with an object as if the maximum valid \code{n}
#'      had been specified}
#'    \item{\code{1} :} {signal a \code{\link[base]{message}} for invalid
#'      values for \code{n} and
#'      return with an object as if the maximum valid \code{n}
#'      had been specified}
#'    \item{\code{2} :} {signal a \code{\link[base]{warning}} for invalid
#'      values for \code{n} and
#'      return with an object as if the maximum valid \code{n}
#'      had been specified}
#'    \item{\code{3} :} {signal an error (see \code{\link[base]{stop}})
#'      for invalid values for \code{n}}
#'  }
#' @example inst/examples/last.r
#' @seealso \code{
#'  \link[selectr]{last},
#'  \link[selectr]{last.default}
#' }
#' @template author
#' @template references
#' @export

last.data.frame <- function(
  x,
  n = 1,
  keep = FALSE,
  margin = c(1, 2),
  drop = FALSE,
  strict = c(0, 1, 2, 3)
) {
  margin <- as.numeric(match.arg(as.character(margin), c("1", "2")))
  strict <- as.numeric(match.arg(as.character(strict), c("0", "1", "2", "3")))
  if (n > 0) {
    if (margin == 1) {
      xx <- try(x[(NROW(x)-n+1):NROW(x),], silent = TRUE)
      force <- n > NROW(x)
      xx <- handleCondition(x = x, xx = xx, n = n, strict = strict,
        force = force, margin = margin)
      if (keep) xx <- structure(xx, keep = x[1:(NROW(x)+(-n)),])
      xx
    } else if (margin == 2) {
      xx <- try(x[,(NCOL(x)-n+1):NCOL(x), drop = drop], silent = TRUE)
      force <- n > NCOL(x)
      xx <- handleCondition(x = x, xx = xx, n = n, strict = strict,
        force = force, margin = margin)
      if (keep) xx <- structure(xx, keep = x[,1:(NCOL(x)+(-n)), drop = drop])
      xx
    }
  } else {
    if (margin == 1) {
      xx <- try(x[1:(NROW(x)+n),], silent = TRUE)
      force <- n > NROW(x)
      xx <- handleCondition(x = x, xx = xx, n = n, strict = strict,
        force = force, margin = margin)
      if (keep) xx <- structure(xx, keep = x[((NROW(x)-(-n)+1):NROW(x)),])
      xx
    } else if (margin == 2) {
      xx <- try(x[,1:(NCOL(x)+n), drop = drop], silent = TRUE)
      force <- n > NCOL(x)
      xx <- handleCondition(x = x, xx = xx, n = n, strict = strict,
        force = force, margin = margin)
      if (keep) xx <- structure(xx, keep = x[,((NCOL(x)-(-n)+1):NCOL(x)), drop = drop])
      xx
    }
  }
}

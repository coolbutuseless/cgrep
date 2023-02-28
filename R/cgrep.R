

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Colour highlighting a regular expression search
#' 
#' @description 
#' Highlight text within an R object which matches a given regex. This 
#' only works in a terminal which supports ANSI colour codes.
#' 
#' There are slightly different versions of the highlighting function depending
#' upon which text version of the object you'd like to match against:
#' 
#' \describe{
#'   \item{cgrep}{ - the given object \code{x} must already be a character vector}
#'   \item{cgrep_character}{ - performs the matching after first calling 
#'           \code{as.character(x)}}
#'   \item{cgrep_print}{ - performs the matching against the default 
#'            \code{print(x)} output}
#'   \item{cgrep_deparse}{ - performs the matching after first calling 
#'           \code{deparse1(x)}}
#'   \item{cgrep_str}{ - performs the matching on the output of calling 
#'           \code{str(x)}}
#' }
#' 
#' @param x character string
#' @param pattern regular expression string. Note: don't get too fancy here
#' @param fg,bg any valid R colour specification e.g. 'hotpink', '#335588'
#' @param ... extra args passed to \code{gsub}
#' 
#' @importFrom utils capture.output str
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cgrep <- function(x, pattern, fg = 'black', bg = 'yellow', ...) {
  
  stopifnot(is.character(x))
  
  pattern <- paste0("(", pattern, ")")
  replacement <- paste0(
    col2fg(fg),
    col2bg(bg),
    "\\1",
    reset_code
  )
  
  
  # x <- gsub(pattern, "\033[43m\033[30m\\1\033[39m\033[49m", x, ...) 
  x <- gsub(pattern, replacement, x, ...) 
  
  cat(x, sep="\n")
  
  invisible(x)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cgrep
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cgrep_character <- function(x, pattern, fg = 'black', bg = 'yellow', ...) {
  x <- capture.output(cat(as.character(x)))
  cgrep(x, pattern, fg, bg, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cgrep
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cgrep_print <- function(x, pattern, fg = 'black', bg = 'yellow', ...) {
  x <- capture.output(x)
  cgrep(x, pattern, fg, bg, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cgrep
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cgrep_deparse <- function(x, pattern, fg = 'black', bg = 'yellow', ...) {
  x <- deparse1(x)
  cgrep(x, pattern, fg, bg, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cgrep
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cgrep_str <- function(x, pattern, fg = 'black', bg = 'yellow', ...) {
  x <- capture.output(str(x, vec.len = 200))
  cgrep(x, pattern, fg, bg, ...)
}
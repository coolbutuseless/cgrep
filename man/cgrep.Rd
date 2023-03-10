% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cgrep.R
\name{cgrep}
\alias{cgrep}
\alias{cgrep_character}
\alias{cgrep_print}
\alias{cgrep_deparse}
\alias{cgrep_str}
\title{Colour highlighting a regular expression search}
\usage{
cgrep(x, pattern, fg = "black", bg = "yellow", ...)

cgrep_character(x, pattern, fg = "black", bg = "yellow", ...)

cgrep_print(x, pattern, fg = "black", bg = "yellow", ...)

cgrep_deparse(x, pattern, fg = "black", bg = "yellow", ...)

cgrep_str(x, pattern, fg = "black", bg = "yellow", ...)
}
\arguments{
\item{x}{character string}

\item{pattern}{regular expression string. Note: don't get too fancy here}

\item{fg, bg}{any valid R colour specification e.g. 'hotpink', '#335588'}

\item{...}{extra args passed to \code{gsub}}
}
\description{
Highlight text within an R object which matches a given regex. This 
only works in a terminal which supports ANSI colour codes.

There are slightly different versions of the highlighting function depending
upon which text version of the object you'd like to match against:

\describe{
  \item{cgrep}{ - the given object \code{x} must already be a character vector}
  \item{cgrep_character}{ - performs the matching after first calling 
          \code{as.character(x)}}
  \item{cgrep_print}{ - performs the matching against the default 
           \code{print(x)} output}
  \item{cgrep_deparse}{ - performs the matching after first calling 
          \code{deparse1(x)}}
  \item{cgrep_str}{ - performs the matching on the output of calling 
          \code{str(x)}}
}
}

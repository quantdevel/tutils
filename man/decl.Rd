% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/declare.R
\name{decl}
\alias{decl}
\title{Type-check a function parameter}
\usage{
decl(x, pred)
}
\arguments{
\item{`x`}{Variable to be type-checked}

\item{`pred`}{A type checking predicate, such as
\code{is.character} or \code{is.data.frame} (function)}
}
\value{
\code{decl} returns its first argument,
but halts on type errors.
}
\description{
Stops if variable type is wrong.
}
\details{
\code{decl} is a simple, fast type checker,
which is nice when speed matters,
such as in low-level functions.
}

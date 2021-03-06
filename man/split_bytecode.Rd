% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lex_parse.R
\name{split_bytecode}
\alias{split_bytecode}
\title{Split bytecode into individual rows}
\usage{
split_bytecode(code)
}
\arguments{
\item{code}{parsed asm containing \code{addr} and \code{bytecode} columns.}
}
\value{
the \code{code} input split into single hex addresses
}
\description{
Split bytecode into individual rows
}

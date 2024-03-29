% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{write_input_file}
\alias{write_input_file}
\title{Write Data Frame to CSV File}
\usage{
write_input_file(x, file, context = deparse(substitute(x)), sep = ";")
}
\arguments{
\item{x}{data frame to be written to CSV file}

\item{file}{full path to target file}

\item{context}{text describing the kind of data that is written. This text
will appear in the debug message}

\item{sep}{column separator, default: ";"}
}
\description{
Write Data Frame to CSV File
}

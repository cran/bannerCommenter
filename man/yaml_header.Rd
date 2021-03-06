% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yaml.R
\name{yaml_header}
\alias{yaml_header}
\alias{chunk_options}
\title{Functions for YAML comments}
\usage{
yaml_header(title, author, ...)

chunk_options(...)
}
\arguments{
\item{title}{A character string; if omitted may be supplied via the console}

\item{author}{A character string; if omitted may be supplied via the console}

\item{...}{Extra arguments, for yaml_header currently ignored; for chunk_options
comma separated chunk options}
}
\value{
An incomplete yaml header, or a line of chunk options, in the clipboard by default
}
\description{
A facility for generating a comment block for inserting at the top of an
R script to insert a yaml header and for setting chunk options within scripts
to use the RStudio facility for automatically rendering scripts into documents.
}
\examples{
yaml_header(title = "My script",
            author = "Bill Venables")
chunk_options(comment="",
              fig.height=7,
              fig.width=8,
              out.width="90\%")
}

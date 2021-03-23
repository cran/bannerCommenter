
###  Special facilities for scripts as .Rmd files

header_txt <- c("#' ---",
                "#' title: \"@title@\"",
                "#' author: \"@author@\"",
                "#' date: \"`r Sys.Date()`\"",
                "#' ---")

#' Functions for YAML comments
#'
#' A facility for generating a comment block for inserting at the top of an
#' R script to insert a yaml header and for setting chunk options within scripts
#' to use the RStudio facility for automatically rendering scripts into documents.
#'
#' @param title A character string; if omitted may be supplied via the console
#' @param author  A character string; if omitted may be supplied via the console
#' @param ... Extra arguments, for yaml_header currently ignored; for chunk_options
#'            comma separated chunk options
#'
#' @return An incomplete yaml header, or a line of chunk options, in the clipboard by default
#' @export
#'
#' @examples
#' yaml_header(title = "My script",
#'             author = "Bill Venables")
#' chunk_options(comment="",
#'               fig.height=7,
#'               fig.width=8,
#'               out.width="90%")
yaml_header <- function(title, author, ...) {
  if(missing(title)) {
    title <- if(interactive()) {
      readline(prompt = "title: ")
    } else ""
  }
  if(missing(author)) {
    author <- if(interactive()) {
      readline(prompt = "author: ")
    } else ""
  }
  txt <- sub("@title@", title, sub("@author@", author, header_txt))
  class(txt) <- "banner"
  txt
}

#' @rdname yaml_header
#' @export
chunk_options <- function(...) {
  dots <- list(...)
  if(length(dots) == 0 || is.null(names(dots))) {
    opt <- ".."
    tdots <- "list("
    while(nchar(opt) > 0) {
      opt <- readline(prompt = "option: ")
      tdots <- paste0(tdots, opt, ",")
    }
    tdots <- sub(",*$", ")", tdots)
    dots <- eval(parse(text = tdots)[[1]])
    dots <- lapply(dots, function(x) if(is.character(x)) paste0("\"", x, "\"") else x)
  }
  out_txt <- paste0("#+ ", paste(names(dots), sapply(dots, as.character), sep = "=", collapse = ","))
  class(out_txt) <- "banner"
  out_txt
}
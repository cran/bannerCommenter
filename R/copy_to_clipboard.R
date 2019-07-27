
#' Transfer text strings to the clipboard ready for paste
#'
#' This is only guaranteed for Windows; in the case of Linux you will
#' need to have the \code{xclip} command installed and visible on the \code{PATH}
#' and for Mac OS you will need to have \code{pbcopy} similarly available.
#' In any case the transfer to the clipboard is only activated while in
#' an interactive session.
#'
#' It behaves like \code{base::cat} but differs in three respects.
#'
#' First, if \code{file} is left missing, in an interactive session,
#' the default file is a clipboard device, if possible.
#'
#' Second, the return value is \code{invisible(x)} rather than
#' \code{invisible(NULL)} as it is for \code{base::cat}.
#'
#' Third, it only has a copying side-effect if used in an interactive session.
#' In a non-interactive session it merely returns the \code{x} argument, invisibly.
#'
#' Note the on \code{Windows} the function \code{utils::writeClipboard} offers
#' a much more extensive range of possibilities for communicating with the
#' clipboard device, but this facility is only available on \code{Windows}.
#'
#' @param x a characeter string vector
#' @param ... additional arguments as for \code{cat}
#' @param file a file or connection (usually left at the default)
#'
#' @return \code{x}, invisibly (as for a print method)
#' @export
copy_to_clipboard <- function(x, ..., file = con) {
  if(missing(file)) {
    oldOpt <- options(warn = -1)
    on.exit(options(oldOpt))
    con <- switch(Sys.info()["sysname"],
                  Linux = {
                    if(system("which xclip", ignore.stdout = TRUE,
                              ignore.stderr = TRUE) == 0) {
                      pipe("xclip -selection clipboard -i", open = "w")
                    } else NULL
                  },
                  Windows = {
                    base::file("clipboard")
                  },
                  {  ### guessing some version of Mac OS!
                    if(system("which pbcopy", ignore.stdout = TRUE,
                              ignore.stderr = TRUE) == 0) {
                      pipe("pbcopy", "w")
                    } else NULL
                  })
    if(!is.null(con))
      on.exit(close(con), add = TRUE)
  }
  if(interactive())
    cat(x, file = file, ...)
  invisible(x)
}

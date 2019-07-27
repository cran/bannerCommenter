
#' Make a decorated comment in an R script
#'
#' Make a decorated multi-line comment from input strings and, if possible,
#' transfer it to the clipboard ready for pasting into an R script (via
#' the \code{print} method).
#'
#' @param x A string, first line of the comment. If
#' \code{""}, the zero-length string, only the top lines of the banner are made.
#' If missing, in an interactive session the user will be prompted for the input
#' strings, one per line, in the console.
#' @param ... Zero or more additional strings as extra lines.  Strings may contain
#' newline characters resulting in further line breaks.
#' @param emph A logical value: Do you want this to be an emphasised comment?
#' @param snug A logical value: Do you want the decoration to hug the strings closely?
#' @param upper A logical value: Do you want the strings converted to upper case?
#' @param centre A logical value: Do you want the text strings centred?
#' (alternative: left justified)
#' @param leftSideHashes A positive integer: How many hashes go on the left side?
#' @param rightSideHashes A non-negative integer: How many hashes go on the right side?
#' @param minHashes A non-negative integer: What is the minimum number of hashes
#'  in the boundary lines?
#' @param numLines A non-negative integer: How many lines of hashes above
#' and below do you want?
#' @param bandChar A single character.  Used instead of # for all characters in
#' the bands around the text, apart from the first character of every line.
#' @param center Alternative spelling of \code{centre}.
#' @param fold Logical: should the text be folded to ensure lines are not too long?
#' @param maxChar Ingeter: maximum length allowed in any line if \code{fold} is \code{TRUE}.
#'
#' @return A character string vector returned invisibly,
#' but automatically displayed in the console
#' @export
#'
#' @examples
#' banner("This should appear clearly and stand out.",
#'        "The lines are left justified by default.")
#' section("This is the first line in a section heading",
#'         "and this is the second\nand this the third.")
#' boxup("This is a less obtrusive comment",
#'       "centred on multiple lines", center = TRUE, bandChar = ".")
#' banner("This is an important side issue.", "Take note!",
#'       snug = TRUE, bandChar = "=")
#' open_box("This is a succinctly presented comment",
#'          "left justified and open at the right",
#'          "on multiple lines")
#'
#' block("This is a chatty comment.  Entering it this way just",
#'       "saves a tiny bit of typing but it can be useful if",
#'       "you need multiple initial hash marks, as you may when",
#'       "using Emacs/ESS, for example.",
#'       "Or if you want the lines centred for some odd reason.",
#'       center = TRUE)
#' ## some authors like to use lines of a uniform length to separate code sections:
#' boxup("")
banner <- function(x, ..., emph = FALSE, snug = FALSE, upper = emph,
                   # centre = !snug && emph,
                   centre = !fold,
                   leftSideHashes = 2 + emph, rightSideHashes = leftSideHashes,
                   minHashes = (!snug) * (65 + 10*emph), numLines = 1+emph,
                   bandChar = "#", center = centre, fold = FALSE, maxChar = 75) {
  if(missing(x)) {
    x <- if(interactive()) {
      paste(scan(what = "", sep = "\n", quiet = TRUE), collapse = "\n")
    } else ""
  }
  if(fold) {
    text <- gsub("\n", " ", paste(as.character(unlist(list(x, ...))), collapse = " "))
    if(nchar(text) > maxChar) {
      txt <- character()
      repeat {
        if(nchar(text) <= maxChar) break
        pos <- gregexpr(" ", text)[[1]]
        if(any(pos < 0) || !any(pos <= maxChar)) break
        pos <- max(pos[pos <= maxChar])
        txt <- c(txt, substring(text, 0, pos))
        text <- substring(text, pos+1, nchar(text))
      }
      text <- paste(sub("^ +", "", sub(" +$", "", c(txt, text))), collapse = "\n")
    }
  } else {
    text <- paste(as.character(unlist(list(x, ...))), collapse = "\n")
  }
  text <- strsplit(text, "\n")[[1]]
  if(length(text) == 0) text <- ""
  nt <- length(text)
  kt <- seq_len(nt)
  n0 <- max(sapply(text, nchar))
  hashes <- max(n0 + leftSideHashes + rightSideHashes + 4, minHashes)
  hashes <- hashes + ((n0 %% 2) != (hashes %% 2))

  bandChar <- substring(paste0(as.character(bandChar), " "), 0, 1)
  line <- paste(c(rep("#", leftSideHashes),
                  rep(bandChar, hashes - leftSideHashes),
                  "\n"), collapse = "")
  leftHash <- paste(rep("#", leftSideHashes), collapse = "")
  rightHash <- paste(c(rep(bandChar, rightSideHashes), "\n"), collapse = "")

  if(nt == 1 && n0 == 0)
    return(structure(c("\n", rep(line, 1 + emph), "\n"),
                     class = "banner"))
  if(missing(centre) & !missing(center))
    centre <- center
  mid <- character(nt)
  for(k in kt) {
    blanks <- (hashes - leftSideHashes - rightSideHashes - nchar(text[[k]]))
    blanks1 <- ifelse(centre, round(blanks/2), 2)
    blanks2 <- blanks - blanks1
    left <- paste(c(leftHash, rep(" ", blanks1)), collapse = "")
    right <- paste(c(rep(" ", blanks2), rightHash), collapse = "")
    mid[k] <- paste(left,
                    ifelse(upper, toupper(text[[k]]), text[[k]]),
                    right, sep = "", collapse = "")
  }
  blanks <- rep(" ", hashes - leftSideHashes - rightSideHashes)
  gap <- paste(c(leftHash, blanks, rightHash), collapse = "")
  comment <- c("\n",
               rep(line, numLines),
               rep(gap, max(0, numLines - 1)),
               mid,
               rep(gap, max(0, numLines - 1)),
               rep(line, numLines),
               "\n")
  structure(comment, class = "banner")
}

#' @describeIn banner Make a prominent banner such as might be useful at the beginning
#' of a major code section
#'
#' @export
section <- function(..., emph = TRUE, centre = TRUE, fold = TRUE) {
  banner(..., emph = emph, centre = centre, fold = fold)
}
#' @describeIn banner Make a minimally boxed banner comment
#'
#' @export
boxup <- function(..., rightSideHashes = 1,
                  bandChar = "-") {
  banner(..., rightSideHashes = rightSideHashes,
         bandChar = bandChar)
}

#' @describeIn banner Make a boxed banner coment open at the right
#'
#' @export
open_box <- function(..., minHashes = 0, rightSideHashes = 0, centre = FALSE,
                     bandChar = "-", center) {
  if(missing(centre) & !missing(center))
    centre <- center
  banner(..., minHashes = minHashes, rightSideHashes = rightSideHashes, centre = centre,
         bandChar = bandChar)
}

#' @describeIn banner Make a simple block of comment lines
#'
#' @export
block <- function(..., leftSideHashes = 3, rightSideHashes = 0, centre = FALSE,
                  minHashes = 0, numLines = 0, center) {
  if(missing(centre) & !missing(center))
    centre <- center
  banner(..., leftSideHashes = leftSideHashes, centre = centre,
         rightSideHashes = rightSideHashes, numLines = numLines)
}

.onAttach <- function(libname, pkgname) {
  if(interactive()) {
    oldOpt <- options(warn = -1)
    on.exit(options(oldOpt))
    switch(Sys.info()["sysname"],
           Linux = {
             if(system("which xclip", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0) {
               NULL ## all OK
             } else {
               packageStartupMessage("Linux detected; no 'xclip' found.\n",
                                     " you need 'xclip' installed",
                                     " and visible on your PATH for the clipboard",
                                     " facility to work.\n Use copy-and-paste.")
             }
           },
           Windows = {
             NULL ## all OK
           },
           {  ### guessing some version of Mac OS!
             if(system("which pbcopy", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0) {
               NULL  ## all OK
             } else {
               packageStartupMessage("Mac OS assumed; no 'pbcopy' found.\n",
                                     " you need 'pbcopy' installed",
                                     " and visible on your PATH for the clipboard",
                                     " facility to work.\n Use copy-and-paste.")
             }
           })
    invisible()
  }
}

#' Print method for banner objects
#'
#' As well as printing the comment string in the console window
#' the same text strings are transferred to a clipboard, if possible,
#' ready for pasting into the R script currently being drafted.
#'
#' @param x A character string vector as procuced by banner()
#' @param ... Not used
#'
#' @return \code{x} itself, invisibly. A side effect is that \code{x}
#' is transferred to a \code{clipboard} device, if possible
#' @export
print.banner <- function(x, ...) {
  y <- unlist(strsplit(x, "\n"))
  copy_to_clipboard(y, sep = "\n")
  cat(y, sep = "\n")
  invisible(x)
}

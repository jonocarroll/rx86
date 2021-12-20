#' Convert hex values to ASCII string
#'
#' @param hex a vector of pairs of hex values (potentially prefixed with "0x").
#'
#' It is assumed that the string contains a null terminator (0x00) so this will
#' be retained. Any data following that will be discarded.
#'
#' @return the ASCII representation of the input as a single string
#' @export
hex2string <- function(hex) {
  # drop prefixes and collapse to a string
  hex_string <- paste0(gsub("0x", "", hex), collapse = "")
  # split into pairs of bytes
  hex_raw <- strsplit(split_pairs(hex_string), ",")[[1]]
  # remove all but the first 00 (null)
  hex_raw <- hex_raw[1:which(hex_raw == "00")[1]]
  # convert to integer then to raw (same values as prev, but raw)
  # then convert to characters
  rawToChar(as.raw(strtoi(hex_raw, 16L)))
}


#' Convert binary values to ASCII string
#'
#' @param bin a vector of pairs of binary values (runs of 0s and 1s, separated by spaces).
#' Any linebreaks will be removed.
#'
#' @return the ASCII representation of the input as a single UTF8 string
#' @export
bin2ascii <- function(bin) {
  nolb <- gsub("\n", " ", x)
  split <- strsplit(split_pairs(paste(nolb, collapse = " "), split = " "), ",")[[1]]
  ints <- strtoi(split, base = 2)
  intToUtf8(ints)
}

#' Shortcut for setting a value to hexmode
#'
#' @md
#' @name hex
#' @seealso [base::as.hexmode()]
#'
#' @export
hex <- as.hexmode


#' @export
`%||%` <- rlang::`%||%`

#' Split argument list into individual elements
#'
#' @param args string containing 1, or 2 comma-separated arguments
#'
#' @return a [data.frame()] containing columns `arg1` and `arg2`
#' @export
split_args <- function(args) {
  stringr::str_split(args, ", ", simplify = TRUE)  %>%
    magrittr::set_colnames(c("arg1", "arg2")) %>%
    as.data.frame()
}


#' Parse asm code and seperate opcodes from arguments
#'
#' @md
#' @param asm a [data.frame()] containing columns `addr` (addresses),
#' `bytecode` (bytecode), and `instr` (instructions).
#'
#' @param opcodes list of opcodes to recognise
#'
#' @return the input `asm` with additional columns containing identified
#'     information
#' @export
parse <- function(asm, opcodes = opcodes) {
  # locate opcodes
  asm <- dplyr::mutate(asm, opcode = stringr::str_extract(.data$instr, paste(opcodes, collapse = "|")))
  # identify arg list
  asm <- dplyr::mutate(asm, args = trimws(stringr::str_remove(.data$instr %||% " ", .data$opcode %||% " ")))
  # split arg list
  asm <- dplyr::mutate(asm, split_args(.data$args))

  asm <- as.data.frame(asm)

  # identify rows by their address
  rownames(asm) <- sapply(asm$addr, sanitize)

  asm

}

#' Split byte strings into pairs of numbers
#'
#' @md
#' @param x string of bytes
#' @param split character at which to [strsplit()]
#'
#' @return a string of paired bytes, comma-separated
#' @export
split_pairs <- function(x, split = "") {
  sst <- strsplit(x, split)[[1]]
  out <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])
  paste0(out, collapse = ",")
}

#' Split bytecode into individual rows
# '
#' @md
#' @param code parsed asm containing `addr` and `bytecode` columns.
#'
#' @return the `code` input split into single hex addresses
#' @export
split_bytecode <- function(code) {
  code[, "bytecode", drop = FALSE] %>%
    tibble::rownames_to_column("addr") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(pairs = split_pairs(.data$bytecode)) %>%
    tidyr::separate_rows(pairs, sep = ",") %>%
    dplyr::mutate(addr = sanitize(hex(1:nrow(.))-1)) %>%
    as.data.frame() %>%
    magrittr::set_rownames(.$addr) %>%
    dplyr::select(addr, pairs)
}

#' Create a memory array
#'
#' `rx86` will assume this is available globally as `mem`.
#'
#' @md
#' @param len length of array (bytes)
#'
#' @return a vector of length `len` filled with 0x00
#' @export
create_mem <- function(len = 128) {
  # create a vector of memory slots
  mem <- vector(mode = "integer", length = len)
  # set the names as hex codes - these represent the addresses
  mem <- setNames(mem, paste0("0x", hex(seq(0, len-1))))

  mem
}

#' @keywords internal
confirm_global_objs <- function() {
  stopifnot(exists("mem"))
  stopifnot(exists("registers"))
}


#' Clean print method for memory and registers
#'
#' @md
#' @return `invisible(NULL)` - called for the side-effect of printing to console
#' @export
showmem <- function() {

  confirm_global_objs()

  m <- mem
  r <- registers
  mat <- matrix(m, ncol = 8, byrow = TRUE)
  rownames(mat) <- names(m)[seq(1, length(m), 8)]
  print(mat)

  regs <- as.list(r)
  lapply(seq_along(regs), function(i) {
    cat(paste0(names(regs)[i], ": ", toString(regs[[i]]), "\n"))
  })
  return(invisible(NULL))
}


#' Sanitize memory locations to strings
#'
#' Because memory is addressed by the names of the vectors (not positions) but
#' _can_ be addressed by e.g. 0x00 (which won't exist) we need to sanitize
#' addressed before using them to access memory. This is achieved by converting
#' to character with a "0x" prefix.
#'
#' @md
#' @param x address to be sanitized
#'
#' @return a character string which can be used to lookup memory locations
#' @export
sanitize <- function(x) {

  # case: [eax] needs to return the memory _at_ register eax
  # when using multiple values, skip this step
  if (length(x) == 1 && grepl("[", x, fixed = TRUE)) {
    xaddr <- gsub("\\[|\\]", "", x)
    return(paste0("0x", reg_or_val(xaddr)))
  }

  paste0("0x", format(hex(x), width = 2))
}


#' Extract from registry or use direct val
#'
#' @md
#' @param x address to lookup
#'
#' @return the value stored at address `x`
#' @export
reg_or_val <- function(x) {

  if (grepl("[", x, fixed = TRUE)) {
    xaddr <- gsub("\\[|\\]", "", x)
    x <- hex(mem[sanitize(reg_or_val(xaddr))])
  }

  if (x %in% names(registers)) {
    return(hex(registers[[x]]))
  } else {
    return(hex(x))
  }
}


#' Create a set of registers
#'
#' Creates a new environment with the following elements (set to 0x00). An
#' environment is used so that it is properly mutable.
#'
#' eax = accumulator
#' ebx = base
#' ecx = counter
#' edx = data
#' esi = source
#' edi = destination
#' al = low bytes of accumulator
#' dl = low bytes of data
#' zf = zero flag
#' esp = stack pointer
#'
#' @md
#' @seealso https://web.archive.org/web/20191114093028/https://gerardnico.com/computer/cpu/register/general
#'
#' @return an `environment` containing zeroed registers
#' @export
create_reg <- function() {
  reg <- new.env()
  reg$eax <- vector(mode = "integer", length = 1)
  reg$eax <- hex(0)
  reg$ebx <- vector(mode = "integer", length = 1)
  reg$ebx <- hex(0)
  reg$ecx <- vector(mode = "integer", length = 1)
  reg$ecx <- hex(0)
  reg$edx <- vector(mode = "integer", length = 1)
  reg$edx <- hex(0)
  reg$esi <- vector(mode = "integer", length = 1)
  reg$esi <- hex(0)
  reg$edi <- vector(mode = "integer", length = 1)
  reg$edi <- hex(0)
  reg$al <- vector(mode = "integer", length = 1)
  reg$al <- hex(0)
  reg$dl <- vector(mode = "integer", length = 1)
  reg$dl <- hex(0)
  reg$zf <- vector(mode = "integer", length = 1)
  reg$zf <- hex(0)
  # and a stack
  reg$esp <- vector(mode = "integer", length = 1)
  reg$esp <- hex(0)
  reg
}


#' Inject code into memory
#'
#' Hex values from code will be inserted based on sanitized addresses.
#' Any remaining memory slots will be pre-allocated to 0x00
#'
#' @md
#' @param code parsed asm code with one line per `bytecode` hex value
#' @param memory memory array into which the code should be injected
#'
#' @return a copy of the `memory` object with bytes inserted at the relevant addresses.
#' @export
insert_code <- function(code, memory) {
  bytes <- split_bytecode(code)
  memory[] <- sanitize(0x00)
  memory[rownames(bytes)] <- sanitize(bytes$pairs)

  memory
}

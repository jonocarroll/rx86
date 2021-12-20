#' opcodes (mnemonics) to separate from arguments
#' @keywords internal
opcodes <- c("call", "pop", "mov", "add", "sub",
             "xor", "halt", "jmp", "jz", "jnz",
             "cmp", "a16", "movsd")


#' Opcodes (x86 assembly operations)
#'
#' @name opcodes
#' @rdname opcodes
NULL


#' @rdname opcodes
#' @export
call <- function(x) {
  assign("esp", hex(c(x, registers$esp)), envir = registers)
  return(invisible(x))
}


#' @rdname opcodes
#' @export
pop <- function(x) {
  # pop first value from the stack and save to x
  assign(x, hex(registers$esp[1]), envir = registers)
  # remove first value from the stack
  assign("esp", registers$esp[-1], envir = registers)
  return(invisible(x))
}


#' @rdname opcodes
#' @export
mov <- function(x, y) {
  # copy y into x
  res <- hex(reg_or_val(y))
  if (x %in% names(registers)) {
    assign(x, res, envir = registers)
  } else {
    mem[sanitize(x)] <<- sanitize(res)
  }
  return(invisible(NULL))
}


#' @rdname opcodes
#' @export
add <- function(x, y) {
  # add y to x and save in x
  res <- hex(reg_or_val(x)) + hex(reg_or_val(y))
  if (x %in% names(registers)) {
    assign(x, res, envir = registers)
  } else {
    mem[sanitize(x)] <<- sanitize(res)
  }
  assign("zf", hex(as.integer(res == 0x00)), envir = registers)
  return(invisible(x))
}


#' @rdname opcodes
#' @export
sub <- function(x, y) {
  # sub y from x and save in x
  res <- hex(reg_or_val(x)) - hex(reg_or_val(y))
  if (x %in% names(registers)) {
    assign(x, res, envir = registers)
  } else {
    mem[sanitize(x)] <<- sanitize(res)
  }
  assign("zf", hex(as.integer(res == 0x00)), envir = registers)
  return(invisible(x))
}

# adc <- function(x, y) {
#   # add y to x and save in x, adding carry value
#   res <- hex(reg_or_val(x)) + hex(reg_or_val(y)) + registers$cl
#   if (x %in% names(registers)) {
#     assign(x, res, envir = registers)
#   } else {
#     mem[sanitize(x)] <<- sanitize(res)
#   }
#   assign("zf", hex(as.integer(res == 0x00)), envir = registers)
#   assign("cl", hex(0x00), envir = registers)
#   return(invisible(x))
# }


#' @rdname opcodes
#' @export
xor <- function(x, y) {
  res <- hex(bitwXor(reg_or_val(x), reg_or_val(y)))
  if (x %in% names(registers)) {
    assign(x, res, envir = registers)
  } else {
    mem[sanitize(x)] <<- sanitize(res)
  }
  assign("zf", hex(as.integer(res == 0x00)), envir = registers)
  return(invisible(x))
}


#' @rdname opcodes
#' @export
cmp <- function(x, y) {
  res <- reg_or_val(x) - reg_or_val(y)
  assign("zf", hex(as.integer(res == 0x00)), envir = registers)
}


#' @rdname opcodes
#' @export
jmp <- function(x) {
  # jump to address (handled by apply_flags)
  return(invisible(NULL))
}


#' @rdname opcodes
#' @export
jnz <- jz <- jmp


#' @rdname opcodes
#' @export
halt <- function(x) {
  message("END RUN")
  return(invisible(NULL))
}


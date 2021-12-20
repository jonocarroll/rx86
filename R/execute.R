#' Run asm code
#'
#' Parses code, inserts values into memory, then proceeds to evaluate
#' the instructions in order. Each operation is followed by a check of the
#' registers to help identify the next instruction - either directed by a
#' JMP or simply the next in order.
#'
#' See `vignette("mult_code_petzold", package = "rx86")` for an example of usage.
#'
#' @md
#' @param asm x86 (disassembled) assembly code
#' @param step (logical) pause after every instruction?
#' @param prog (logical) display the memory (via [showmem()]) at every step?
#'
#' @return `invisible(NULL)` - called for the side-effect of modifying the global
#'     `mem` and `registers` objects.
#'
#' @export
runasm <- function(asm, step = FALSE, prog = FALSE) {

  # parse for opcodes and args
  code <- parse(asm, opcodes)
  # add code to memory array
  mem <<- insert_code(code, mem)
  if (prog) showmem()

  next_mem <- sanitize(0x00)
  next_instruction <- code[sanitize(next_mem), "opcode"]
  while (next_instruction != "halt") {
    next_arg1 <- code[sanitize(next_mem), "arg1"]
    next_arg2 <- code[sanitize(next_mem), "arg2"]
    if (next_arg2 == "") {
      if (prog) message(next_instruction, " ", next_arg1)
      do.call(next_instruction, args = list(x = next_arg1))
      if (prog) showmem()
    } else {
      if (prog) message(next_instruction, " ", next_arg1, ", ", next_arg2)
      do.call(next_instruction, args = list(x = next_arg1, y = next_arg2))
      if (prog) showmem()
    }
    old_mem <- next_mem
    next_mem <- apply_flags(next_instruction, next_arg1, next_arg2, registers)
    if (prog) showmem()
    if (is.null(next_mem)) {
      # no jump, move to next line
      next_mem <- rownames(code)[which(rownames(code) == old_mem) + 1]
    }
    next_instruction <- code[sanitize(next_mem), "opcode"]
    if (prog) print(code[sanitize(next_mem), ])
    if (step) {
      if (readline("next...") == "x") browser()
    }
  }
  invisible(NULL)
}

#' Apply logic of flags in registers
#'
#' @param instr asm instruction previously called
#' @param addr1 first address argument to previously called instruction
#' @param addr2 second address argument to previously called instruction
#' @param reg (global) registry environment
#'
#' @return either the address of the next instruction to call, or `NULL`
#' @export
apply_flags <- function(instr, addr1, addr2, reg) {
  ## return NULL to move to next instruction in code
  if (instr %in% c("call", "jmp")) {
    return(sanitize(addr1))
  } else if (instr == "jnz") {
    if (reg$zf != 0x01) {
      return(sanitize(addr1))
    } else {
      return(NULL)
    }
  } else if (instr == "jz") {
    if (reg$zf == 0x01) {
      return(sanitize(addr1))
    } else {
      return(NULL)
    }
  }
}

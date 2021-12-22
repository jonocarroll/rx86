
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rx86

<!-- badges: start -->
<!-- badges: end -->

The goal of rx86 is to provide a functional representation of x86
assembly and execute the instructions.

## Installation

You can install the development version of rx86 from Github with:

``` r
# install.packages("remotes")
remotes::install_github("jonocarroll/rx86")
```

## Motivation

See [this]() blogpost, but I wanted to be able to ‘run’ some x86
assembly code and wondered if I could do it entirely from R.

A small, more general example, is to perform multiplication

``` r
library(rx86)
#> 
#> Attaching package: 'rx86'
#> The following objects are masked from 'package:base':
#> 
#>     call, parse, sub, xor
## load some asm instructions which store the 
## the numbers 167 and 28, multiplies them, 
## and stores the result in 0x22
mult_asm <- suppressWarnings(
  readr::read_fwf(system.file("asm", "mult.asm", package = "rx86"), 
                  col_types = "ccc",
                  col_positions = readr::fwf_widths(c(3, 6, 20)))
)
colnames(mult_asm) <- c("addr", "bytecode", "instr")

# create blank slate of memory and registers
mem <- create_mem(len = 64)
registers <- create_reg()

runasm(mult_asm)

# extract result from memory and convert to integer
as.integer(mem[sanitize(0x22)])
#> [1] 4676
```

This is detailed in `vignette("mult_code_petzold", package = "rx86")`.

## Inspiration

After reading [‘Code: The Hidden Language of Computer Hardware and
Software’](https://en.wikipedia.org/wiki/Code:_The_Hidden_Language_of_Computer_Hardware_and_Software)
by Charles Petzold - which does a fantastic job of describing the
journey from telegraph relays through to a working computer - I was
reminded that I had once looked into some assembly for a puzzle, but
never wrote up my findings.

In an effort to better understand the solution to that puzzle (and the
way to get to it) I thought it would be neat to try to replicate some of
the functionality in a language I’m more familiar with. The operations
on data stored in memory are simple - move a value from here to there,
add some values, … so it made sense to me to see if I could replicate
that in a functional language.

This isn’t something you would use to write asm, but it very well may
help you understand what it’s doing.

## Limitations

The following is a non-exhaustive list of the known limitations of this
package.

-   This was built so support a single use-case of solving the puzzle
    described in \``vignette("dsd_ruxcon_challenge", package = "rx86")`
    so only has support for the opcodes required for that. That covers a
    lot of possibilities, but if you need a `DIV` you’re out of luck so
    far.
-   This takes as input a `data.frame` of disassembled code. This has
    only been tested with the output of

``` bash
udcli -x file.hex
```

with the `udis` tool from <http://udis86.sourceforge.net/>. \*

## Other R assemblers

-   [c64](https://github.com/coolbutuseless/r64) - a c64/6502 assembler

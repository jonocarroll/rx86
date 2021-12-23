
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

See [this](https://jcarroll.com.au/2021/12/23/adventures-in-x86-asm/)
blogpost, but I wanted to be able to ‘run’ some x86 assembly code and
wondered if I could do it entirely from R.

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
print(mult_asm)
#> # A tibble: 14 x 3
#>    addr  bytecode instr         
#>    <chr> <chr>    <chr>         
#>  1 00    101005   mov al, [0x22]
#>  2 03    201001   add al, [0x20]
#>  3 06    111005   mov [0x22], al
#>  4 09    101004   mov al, [0x21]
#>  5 0c    221000   sub al, 0x01  
#>  6 0f    111004   mov 0x21, al  
#>  7 12    101003   jnz 0x00      
#>  8 15    20001e   halt          
#>  9 18    111003   invalid       
#> 10 1b    330000   invalid       
#> 11 1e    ff00     invalid       
#> 12 20    a7       data 167      
#> 13 21    1c       data 28       
#> 14 22    00       result

# create blank slate of memory and registers
mem <- create_mem(len = 64)
registers <- create_reg()

runasm(mult_asm)

# extract result from memory and convert to integer
as.integer(mem[sanitize(0x22)])
#> [1] 4676

167*28
#> [1] 4676
```

This is detailed in `vignette("mult_code_petzold", package = "rx86")`.

What test is complete without a helloworld?

``` r
hello_asm <- suppressWarnings(
  readr::read_fwf(system.file("asm", "helloworld.asm", package = "rx86"), 
                  col_types = "ccc",
                  col_positions = readr::fwf_widths(c(3, 3, 20)))
)
colnames(hello_asm) <- c("addr", "bytecode", "instr")
print(hello_asm)
#> # A tibble: 23 x 3
#>    addr  bytecode instr        
#>    <chr> <chr>    <chr>        
#>  1 00    10       mov ecx, 0x0e
#>  2 01    10       mov al, 0x08 
#>  3 02    10       mov eax, [al]
#>  4 03    cc       int 0x80     
#>  5 04    28       sub ecx, 0x01
#>  6 05    70       jz 0x17      
#>  7 06    05       add al, 0x01 
#>  8 07    e9       jmp 0x02     
#>  9 08    48       data         
#> 10 09    65       data         
#> # … with 13 more rows

mem <- create_mem()
registers <- create_reg()

runasm(hello_asm)
#> Hello, world!
```

This is detailed in `vignette("helloworld", package = "rx86")`.

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

-   This was originally built to support a single use-case of solving
    the puzzle described in
    \``vignette("dsd_ruxcon_challenge", package = "rx86")` so only has
    support for the opcodes required for that. That covers a lot of
    possibilities, but if you need a `DIV` you’re out of luck so far.
-   The operations supported do so *in spirit* and are not exact
    reproductions of functionality - if this does something different to
    real asm, that’s not too surprising.
-   This takes as input a `data.frame` of disassembled code. This has
    only been tested with the output of

``` bash
udcli -x file.hex
```

with the `udis` tool from <http://udis86.sourceforge.net/>. \*

## Other R assemblers

-   [c64](https://github.com/coolbutuseless/r64) - a c64/6502 assembler

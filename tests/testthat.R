library(testthat)

## Source R/ directly rather than relying on a package DESCRIPTION, since
## this project is a Shiny app rather than an installable package.
source_dir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(sort(files), source))
}
source_dir(file.path("..", "R"))

## Also source common_ui.R for read_params_step() (the shared alpha/power/
## tails/allocation/multiplicity parser) -- it's plain base-R logic that
## happens to live alongside the family modules rather than in R/, since
## it's paired 1:1 with params_step_ui() in the same file. Safe without
## `library(shiny)` loaded: every shiny/bslib call in that file lives
## inside a function body (UI-construction functions), never at
## source-time, so only defining them here does not require the package.
source(file.path("..", "modules", "common_ui.R"))

test_dir("testthat")

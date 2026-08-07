library(testthat)

## Source R/ directly rather than relying on a package DESCRIPTION, since
## this project is a Shiny app rather than an installable package.
source_dir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(sort(files), source))
}
source_dir(file.path("..", "R"))

## APP_VERSION and the other app-level constants live in global.R, not in
## R/, and test-misc.R / test-project_state.R read them -- so sourcing R/
## alone leaves this entry point failing with "object 'APP_VERSION' not
## found" even though the suite is green. Defined here rather than sourcing
## global.R wholesale because global.R attaches shiny and bslib, which the
## calculation tests do not need and which this file has always been able
## to run without. Parsed out of global.R rather than duplicated, so the
## constant still has exactly one definition (test-misc.R asserts it
## matches CITATION.cff).
local({
  g <- parse(file.path("..", "global.R"))
  keep <- vapply(g, function(e) {
    is.call(e) && length(e) == 3L &&
      as.character(e[[1]]) %in% c("<-", "=") &&
      is.name(e[[2]]) && grepl("^APP_", as.character(e[[2]]))
  }, logical(1))
  for (e in g[keep]) eval(e, envir = globalenv())
})

## Also source common_ui.R for read_params_step() (the shared alpha/power/
## tails/allocation/multiplicity parser) -- it's plain base-R logic that
## happens to live alongside the family modules rather than in R/, since
## it's paired 1:1 with params_step_ui() in the same file. Safe without
## `library(shiny)` loaded: every shiny/bslib call in that file lives
## inside a function body (UI-construction functions), never at
## source-time, so only defining them here does not require the package.
source(file.path("..", "modules", "common_ui.R"))

test_dir("testthat")

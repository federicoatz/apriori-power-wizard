## The example library is a set of hard-coded instances of the same state
## object Save/Load and share-links serialize. Its characteristic failure
## mode is silent: a mistyped input name doesn't error, it just leaves that
## field at its default, so the example quietly demonstrates something
## other than what it claims. The main test below therefore checks every
## input name in every example against the ids the corresponding module
## actually creates -- which is what catches that.

# Locate modules/ from wherever testthat happens to set the working
# directory, and skip (rather than fail) if it can't be found, so a
# harness change can never turn this into a spurious red suite.
.modules_dir <- local({
  cand <- c("../../modules", "../modules", "modules")
  hit <- cand[vapply(cand, function(p) dir.exists(p), logical(1))]
  if (length(hit)) hit[[1]] else NA_character_
})

.module_input_ids <- function(family) {
  file <- switch(family,
    clustered_rct = "mod_clustered.R",
    clustered_cat = "mod_clustered_cat.R",
    paste0("mod_", family, ".R"))
  path <- file.path(.modules_dir, file)
  if (!file.exists(path)) return(NULL)
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")
  shared <- paste(readLines(file.path(.modules_dir, "common_ui.R"), warn = FALSE), collapse = "\n")
  ids <- unlist(regmatches(src, gregexpr('ns\\("[a-zA-Z_0-9]+"\\)', src)))
  ids_shared <- unlist(regmatches(shared, gregexpr('ns\\("[a-zA-Z_0-9]+"\\)', shared)))
  unique(gsub('ns\\("|"\\)', "", c(ids, ids_shared)))
}

test_that("every example is structurally complete and self-consistent", {
  lib <- example_library()
  expect_gt(length(lib), 0)
  for (nm in names(lib)) {
    ex <- lib[[nm]]
    expect_true(all(c("title", "blurb", "family", "state") %in% names(ex)),
                info = nm)
    expect_type(ex$title, "character")
    expect_type(ex$blurb, "character")
    expect_gt(nchar(ex$title), 0)
    expect_gt(nchar(ex$blurb), 0)
    # The state must declare the same family as the entry itself, since
    # apply_saved_state() routes on state$family while the UI groups on
    # the outer one.
    expect_identical(ex$state$family, ex$family, info = nm)
    expect_true(is.list(ex$state$inputs), info = nm)
    expect_gt(length(ex$state$inputs), 0)
  }
})

test_that("every example input name is an id its module actually creates", {
  skip_if(is.na(.modules_dir), "modules/ not locatable from the test wd")
  lib <- example_library()
  for (nm in names(lib)) {
    ex <- lib[[nm]]
    known <- .module_input_ids(ex$family)
    skip_if(is.null(known), paste("module source not found for", ex$family))
    # `wiz` is the sub-wizard tabsetPanel, handled specially by
    # apply_saved_state(); it is a valid key but not an ns() input.
    supplied <- setdiff(names(ex$state$inputs), "wiz")
    unknown <- setdiff(supplied, known)
    expect_identical(unknown, character(0),
                     info = sprintf("%s: unknown input id(s): %s",
                                     nm, paste(unknown, collapse = ", ")))
  }
})

test_that("examples land on a real sub-wizard step", {
  lib <- example_library()
  valid <- c("design", "params", "effect_size", "results")
  for (nm in names(lib)) {
    w <- lib[[nm]]$state$inputs$wiz
    if (!is.null(w)) expect_true(w %in% valid, info = nm)
  }
})

test_that("examples cover a spread of families, and none is duplicated wholesale", {
  lib <- example_library()
  fams <- vapply(lib, function(e) e$family, character(1))
  # The point of the library is breadth: it should demonstrate several
  # different families, not the same one repeatedly.
  expect_gte(length(unique(fams)), 5)
  titles <- vapply(lib, function(e) e$title, character(1))
  expect_identical(anyDuplicated(titles), 0L)
})

test_that("every effect-size branch named in an example is one the app supports", {
  lib <- example_library()
  for (nm in names(lib)) {
    b <- lib[[nm]]$state$inputs$es_branch
    if (!is.null(b)) {
      expect_true(b %in% c("cohen", "safeguard", "sesoi"), info = nm)
    }
  }
})

test_that("alpha, power and attrition in examples are inside the inputs' own bounds", {
  lib <- example_library()
  for (nm in names(lib)) {
    inp <- lib[[nm]]$state$inputs
    if (!is.null(inp$alpha)) {
      expect_gt(inp$alpha, 0); expect_lte(inp$alpha, 0.5)
    }
    if (!is.null(inp$power)) {
      expect_gt(inp$power, 0); expect_lt(inp$power, 1)
    }
    if (!is.null(inp$attrition)) {
      expect_gte(inp$attrition, 0); expect_lte(inp$attrition, 0.95)
    }
    if (!is.null(inp$icc)) {
      expect_gte(inp$icc, 0); expect_lt(inp$icc, 1)
    }
    if (!is.null(inp$rho)) {
      expect_gte(inp$rho, 0); expect_lt(inp$rho, 1)
    }
  }
})

test_that("the caveat text exists and actually warns rather than reassures", {
  txt <- example_library_caveat()
  expect_type(txt, "character")
  expect_gt(nchar(txt), 80)
  expect_match(txt, "illustrative", fixed = TRUE)
})

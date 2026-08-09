# Contributing

Thanks for considering a contribution. This project is small enough that
most of "the process" is: fork, change, test, open a pull request against
`main`. The notes below cover the parts that aren't obvious from the code.

## Getting set up

```bash
git clone https://github.com/federicoatz/apriori-power-wizard.git
cd apriori-power-wizard
```

```r
install.packages("renv")
renv::restore()      # installs the exact package versions in renv.lock
shiny::runApp()      # opens the app locally
```

## Where things live

- `R/` -- pure, testable calculation functions. No Shiny, no reactivity.
  Every analysis family's power/sample-size math lives in its own
  `power_<family>.R` file (`power_two_means.R`, `power_chisq.R`, ...), and
  `R/decision_helper.R` holds the Step-1 decision-tree logic
  (`recommend_family()`) separately from the calculations themselves.
- `modules/` -- Shiny modules, one per analysis family (`mod_<family>.R`),
  plus `common_ui.R`/`common_results.R` for the UI and results-step
  fragments every family shares.
- `app.R` / `global.R` -- the wizard shell (Step 1, routing, theme) and
  package loading/sourcing.
- `tests/testthat/` -- one `test-<file>.R` per `R/` file, mirroring its
  name.
- `validation/` -- longer-running scripts that aren't part of the regular
  test suite: the decision-helper scenario benchmark, the seeded Monte
  Carlo validation, and the accessibility audit. See
  [VALIDATION.md](VALIDATION.md) for what each one checks and why.

See the README's "Project structure" section for the full file listing.

## Coding style

- Base R, `snake_case` for functions and variables, no `library()` calls
  inside `R/` or `modules/` beyond what `global.R` already attaches --
  everything else uses an explicit `pkg::function()` prefix (see the note
  at the top of `global.R` for why this matters for the browser-only
  build).
- Every exported-style function gets a short roxygen block (`#'`) above
  it: one-line description, `@param` for each argument, `@return`. Look
  at any `R/power_*.R` file for the pattern.
- A short header comment (`## filename.R` + a couple of lines) at the top
  of each file explaining what it's for is preferred over inline
  narration; prefer no comment at all over one that just restates the
  code.
- Keep `R/` free of Shiny: if a function needs `input`/`output`/`session`,
  it belongs in `modules/`, not `R/`.

## Adding a new analysis family

This is the most common kind of substantial contribution, so the pattern
is worth spelling out:

1. Add `R/power_<family>.R` with `power_<family>_n()`,
   `power_<family>_at_n()`, and (where it makes sense) a
   `power_<family>_min_*()` sensitivity function, following the naming and
   argument conventions of an existing family that's structurally similar.
2. Add `tests/testthat/test-power_<family>.R` with, at minimum: a pinned
   reference value checked against an independent source (a textbook
   worked example, `pwr`, G\*Power, or a Monte Carlo simulation -- see
   `validation/monte_carlo_validation.R` for the pattern used for families
   with no existing reference implementation) and a monotonicity check
   (power increases with `n`, required `n` decreases with a larger effect
   size).
3. Add `modules/mod_<family>.R` following an existing module, and wire it
   into `app.R`'s family list and `R/decision_helper.R`'s question tree if
   it should be reachable from Step 1.
4. Add a row to Table 2 of the manuscript's methods description if you're
   also touching documentation-facing content (not required for a
   code-only contribution).

## Running the tests

```r
source("global.R")                     # required first -- see below
testthat::test_dir("tests/testthat")
```

`source("global.R")` first is required: it attaches packages and sources
every file in `R/` and `modules/` that the tests call into.
`testthat::test_dir()` on its own will fail with "could not find
function" errors.

The full suite (2,792 assertions as of v1.5.0, across value-based
and property-based layers) runs automatically on
every push and pull request via GitHub Actions
(`.github/workflows/test.yml`); a pull request that fails it won't be
merged. New families or calculation changes should include a
corresponding test file, not just a manual check.

## Pull requests

- Branch from `main`, keep the change focused (one family, one fix, one
  feature per PR is easier to review than several bundled together).
- Make sure `testthat::test_dir("tests/testthat")` passes locally before
  opening the PR -- CI will catch it either way, but it's faster to know
  first.
- Update `CHANGELOG.md` under an `[Unreleased]` (or the next version)
  heading if the change is user-visible.
- No CLA, no specific commit message format required -- a clear message
  describing *why* the change was made is enough.

## Reporting a bug instead

Use the
[bug report template](https://github.com/federicoatz/apriori-power-wizard/issues/new?template=bug_report.yml)
(also linked from the app's own footer) -- it asks for the app version,
browser, and analysis family, which is usually enough to reproduce most
issues.

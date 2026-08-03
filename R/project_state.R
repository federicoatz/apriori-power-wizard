## project_state.R
## -----------------------------------------------------------------------
## Save/load a project (as a downloadable/uploadable JSON file) and
## shareable links (the same state encoded in a URL query parameter).
## Pure, testable functions here; the thin Shiny wiring (reading
## `input`/`session`, the downloadHandler/fileInput/observers) lives in
## app.R, where the per-family lazy-loading machinery already is.
##
## What gets captured: every input belonging to the CURRENTLY ACTIVE
## family module (namespaced as "<family>-<name>" at the top-level
## `input`), minus a small, precisely-known set of transient,
## click-counter-style inputs (wizard step navigation, "back to step 1",
## the effect-size mini-helper's "Use this" button) that every family
## gets for free from the shared `wizard_nav_ui()` / `family_page_header()`
## / `wire_effect_size_helper()` helpers -- restoring a raw click counter
## for one of those via a generic input-update would fire its
## `observeEvent()` as if the button had genuinely been clicked again,
## which is exactly the bug this exclusion avoids. The internal `wiz`
## step tracker (a tabsetPanel selection, not a click counter) IS kept,
## so reopening a saved project drops the user back on the same sub-step.
## -----------------------------------------------------------------------

#' Regex (matched against a name ALREADY STRIPPED of its "<family>-"
#' prefix) for inputs that must never be captured/restored generically
#' @keywords internal
.state_exclude_re <- "^(nav_next_.*|nav_back_.*|use_90|back_to_step1|es_helper_use|share_link|save_project|load_project_file)$"

#' Capture the current state of one family's inputs into a plain list
#'
#' @param all_inputs named list, e.g. `shiny::reactiveValuesToList(input)`
#'   at the TOP-LEVEL session (so it includes every namespaced module
#'   input, prefixed "<family>-<name>")
#' @param family character, the active family key (e.g. "two_means")
#' @return list(family = family, inputs = named list, values already
#'   stripped of the "<family>-" prefix and of NULL/unset entries)
#' @export
capture_family_state <- function(all_inputs, family) {
  prefix <- paste0(family, "-")
  fam_names <- names(all_inputs)[startsWith(names(all_inputs), prefix)]
  stripped <- sub(paste0("^", prefix), "", fam_names)
  keep <- !grepl(.state_exclude_re, stripped)
  fam_names <- fam_names[keep]
  stripped <- stripped[keep]

  vals <- all_inputs[fam_names]
  names(vals) <- stripped
  vals <- vals[!vapply(vals, is.null, logical(1))]

  list(family = family, inputs = vals)
}

#' Serialize a captured state to a JSON string
#' @export
state_to_json <- function(state) {
  as.character(jsonlite::toJSON(state, auto_unbox = TRUE, null = "null"))
}

#' Parse a JSON string back into list(family=, inputs=)
#' @export
json_to_state <- function(text) {
  jsonlite::fromJSON(text, simplifyVector = TRUE)
}

#' Encode a JSON string as a URL-safe ("base64url", unpadded) string
#' @export
json_to_query <- function(json) {
  b64 <- as.character(jsonlite::base64_enc(charToRaw(json)))
  b64 <- gsub("[\r\n[:space:]]", "", b64)
  b64 <- gsub("+", "-", b64, fixed = TRUE)
  b64 <- gsub("/", "_", b64, fixed = TRUE)
  sub("=+$", "", b64)
}

#' Decode a "base64url" string produced by [json_to_query()] back to JSON
#' @export
query_to_json <- function(encoded) {
  b64 <- gsub("-", "+", encoded, fixed = TRUE)
  b64 <- gsub("_", "/", b64, fixed = TRUE)
  pad <- (4 - nchar(b64) %% 4) %% 4
  b64 <- paste0(b64, strrep("=", pad))
  rawToChar(jsonlite::base64_dec(b64))
}

## deploy/serve_static.R
## -----------------------------------------------------------------------
## Minimal static file server for a shinylive export (see
## export_shinylive.R), used only to smoke-test the built site in CI
## before it gets deployed (see smoke_test.R and
## ../.github/workflows/deploy-shinylive.yml). Not part of the app itself.
##
## Usage:
##   Rscript deploy/serve_static.R [directory] [port]
## Runs until killed; prints "SERVER-READY" once listening, which
## smoke_test.R's caller polls for before running the browser check.
## -----------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
site_dir <- if (length(args) >= 1) args[[1]] else "_site"
port <- if (length(args) >= 2) as.integer(args[[2]]) else 8917L

if (!dir.exists(site_dir)) {
  stop(sprintf("Directory '%s' does not exist -- run the export first.", site_dir))
}

library(httpuv)

mime_type <- function(file) {
  ext <- tolower(tools::file_ext(file))
  switch(ext,
    html = "text/html", js = "application/javascript",
    json = "application/json", wasm = "application/wasm",
    css = "text/css", data = "application/octet-stream",
    "application/octet-stream")
}

server <- httpuv::startServer("127.0.0.1", port, list(
  call = function(req) {
    path <- req$PATH_INFO
    if (path == "/" || path == "") path <- "/index.html"
    file <- file.path(site_dir, sub("^/", "", path))
    if (!file.exists(file) || dir.exists(file)) {
      return(list(status = 404L, body = "not found"))
    }
    list(status = 200L, headers = list("Content-Type" = mime_type(file)),
         body = readBin(file, "raw", file.info(file)$size))
  }
))

cat("SERVER-READY\n")
flush(stdout())
while (TRUE) {
  httpuv::service()
  Sys.sleep(0.05)
}

## deploy/export_shinylive.R
## -----------------------------------------------------------------------
## Builds a browser-only (no server) version of this app using the
## `shinylive` R package, which compiles the app to WebAssembly via webR
## and writes out a folder of static files you can host anywhere that
## serves plain HTML/JS -- e.g., GitHub Pages, Netlify, or any static file
## host. Run this from a NORMAL (non-web) R installation on your own
## machine, NOT inside the app itself. This is the SAME script the
## .github/workflows/deploy-shinylive.yml CI workflow runs, so a local
## export and the real deployed site are always built identically.
##
## Usage (from the project root, i.e. the folder containing app.R):
##   Rscript deploy/export_shinylive.R
## or, interactively:
##   source("deploy/export_shinylive.R")
##
## After it finishes, preview locally before deploying anywhere:
##   install.packages("httpuv")   # if not already installed
##   httpuv::runStaticServer("_site")
## then open the printed local URL in a browser and click through the
## whole wizard once, including the Results step, before trusting it.
##
## WHY THIS EXPORTS A FILTERED COPY, NOT THE WHOLE PROJECT DIRECTORY
## -----------------------------------------------------------------------
## shinylive::export() scans every .R file under appdir to figure out
## which R packages need to be bundled into the exported site. Pointed at
## the whole repo, that scan also picks up tests/testthat.R (which does
## `library(testthat)`) and anything deploy/ or .github/ workflows
## reference -- packages the running APP itself never needs, but which
## still get downloaded into the exported bundle and, in some cases,
## fetched by a visitor's browser. Copying only the files the app
## actually sources at runtime (app.R, global.R, R/, modules/, www/) into
## a clean temp directory before exporting keeps the bundle limited to
## real runtime dependencies.
##
## IMPORTANT CAVEAT -- read before relying on this build
## -----------------------------------------------------------------------
## Two packages this app used to depend on turned out to be a poor fit for
## the browser-only (shinylive/webR) build and have since been removed
## entirely, each replaced with a base-R reimplementation of the same
## published formula: WebPower (logistic regression -- its lme4/Matrix/
## lavaan/PearsonDS dependency chain crashed webR outright with a
## WebAssembly ABI error) and Superpower (factorial ANOVA -- it worked,
## but its afex/lme4/car/emmeans/Matrix chain added ~98MB to every single
## page load, regardless of whether the visitor ever opened the ANOVA
## family). See README.md, "Browser-only deployment" -> Troubleshooting,
## for the WebPower incident, and R/power_anova_factorial.R's header
## comment for the ANOVA formula and its validation. webR's package
## repository is still a moving target, so the first thing to do after
## any export is open the browser console (F12) and check for red
## package-loading errors before continuing.
## -----------------------------------------------------------------------

if (!requireNamespace("shinylive", quietly = TRUE)) {
  message("Installing the 'shinylive' package (one-time)...")
  install.packages("shinylive")
}

# Sanity check: run this from the project root (must contain app.R).
if (!file.exists("app.R")) {
  stop(
    "app.R not found in the current working directory. ",
    "Run this script from the project root, e.g.:\n",
    "  Rscript deploy/export_shinylive.R\n",
    "(from inside the power-analysis-app/ folder)."
  )
}

destdir <- "_site"

# ---- Stage only the runtime-relevant files into a clean temp directory ----
runtime_items <- c("app.R", "global.R", "R", "modules", "www")
stage_dir <- file.path(tempdir(), paste0("power-analysis-app-shinylive-src-", Sys.getpid()))
if (dir.exists(stage_dir)) unlink(stage_dir, recursive = TRUE)
dir.create(stage_dir, recursive = TRUE)
for (item in runtime_items) {
  if (!file.exists(item)) stop(sprintf("Expected runtime file/dir '%s' not found.", item))
  file.copy(item, stage_dir, recursive = TRUE)
}

message(sprintf("Exporting to ./%s (this downloads webR + package wasm binaries the first time, so it can take a few minutes)...", destdir))

# ---- Boot-time loading splash with a 0->100% progress bar -----------------
# Purely cosmetic: an overlay drawn on top of the page via shinylive's own
# `include_before_body` template hook. It never touches the Shiny/webR boot
# sequence itself, so it cannot slow down or break the real app load -- it
# only makes the (unavoidable) first-visit wait feel shorter.
#
# There is no real progress signal exposed by shinylive/webR to hook into
# (no `shiny:connected` event, no package/byte-count callback in the
# installed JS bundles), so the bar eases toward ~90% on a fixed animation
# and only reaches 100% once a REAL readiness signal fires: a
# MutationObserver watching for `#analysis_choice` (Step 1's actual content)
# to appear in the DOM. A 120s fallback timeout guarantees the splash can
# never get stuck covering an app that's already usable.
splash_html <- '
<style>
  #pw-boot-splash {
    position: fixed; inset: 0; z-index: 99999;
    display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 14px;
    background: #FFFFFF; color: #16161D;
    font-family: -apple-system, "Segoe UI", sans-serif;
    transition: opacity 0.3s ease;
  }
  #pw-boot-splash .pw-icon {
    color: #5B5BD6;
    animation: pw-pulse 1.8s ease-in-out infinite;
  }
  #pw-boot-splash .pw-title {
    font-size: 15px; font-weight: 600; margin: 2px 0 4px;
  }
  #pw-boot-splash .pw-bar-track {
    width: 240px; height: 6px; border-radius: 999px;
    background: #E8E8ED; overflow: hidden;
  }
  #pw-boot-splash .pw-bar-fill {
    position: relative;
    width: 0%; height: 100%; border-radius: 999px;
    background: linear-gradient(90deg, #5B5BD6, #8B8BE8);
    transition: width 0.25s ease;
    overflow: hidden;
  }
  #pw-boot-splash .pw-bar-fill::after {
    content: ""; position: absolute; inset: 0;
    background: linear-gradient(90deg, transparent, rgba(255,255,255,0.55), transparent);
    background-size: 60px 100%; background-repeat: no-repeat;
    animation: pw-shimmer 1.3s linear infinite;
  }
  #pw-boot-splash .pw-pct { font-size: 13px; font-variant-numeric: tabular-nums; color: #52525E; }
  #pw-boot-splash .pw-msg {
    font-size: 13px; max-width: 320px; text-align: center; min-height: 2.6em;
    color: #52525E; transition: opacity 0.3s ease;
  }
  @keyframes pw-shimmer {
    0% { background-position: -60px 0; }
    100% { background-position: 300px 0; }
  }
  @keyframes pw-pulse {
    0%, 100% { transform: scale(1); opacity: 1; }
    50% { transform: scale(1.08); opacity: 0.8; }
  }
  @media (prefers-color-scheme: dark) {
    #pw-boot-splash { background: #0B0F1A; color: #E8ECF4; }
    #pw-boot-splash .pw-icon { color: #22D3EE; }
    #pw-boot-splash .pw-bar-track { background: #232C42; }
    #pw-boot-splash .pw-bar-fill { background: linear-gradient(90deg, #22D3EE, #67E4F7); }
    #pw-boot-splash .pw-pct { color: #A8B4C8; }
    #pw-boot-splash .pw-msg { color: #A8B4C8; }
  }
</style>
<div id="pw-boot-splash">
  <svg class="pw-icon" width="30" height="30" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
    <rect x="3" y="14" width="4" height="7" rx="1" fill="currentColor"/>
    <rect x="10" y="9" width="4" height="12" rx="1" fill="currentColor"/>
    <rect x="17" y="3" width="4" height="18" rx="1" fill="currentColor"/>
  </svg>
  <div class="pw-title">A Priori Power Analysis Wizard</div>
  <div class="pw-bar-track"><div class="pw-bar-fill" id="pw-bar-fill"></div></div>
  <div class="pw-pct" id="pw-bar-pct">0%</div>
  <div class="pw-msg" id="pw-tip">Loading the app in your browser &mdash; the first visit can take up to a minute while R and its packages download. Later visits are faster (cached).</div>
</div>
<script>
  (function () {
    var fill = document.getElementById("pw-bar-fill");
    var pct = document.getElementById("pw-bar-pct");
    var tipEl = document.getElementById("pw-tip");
    var done = false;
    var current = 0;
    var cap = 90;

    function setPct(p) {
      current = p;
      if (fill) fill.style.width = p + "%";
      if (pct) pct.textContent = Math.round(p) + "%";
    }

    // Rotating facts to make the (unavoidable) first-visit wait feel less
    // dead; index 0 is the original explanatory message, shown first.
    var tips = [
      "Loading the app in your browser \\u2014 the first visit can take up to a minute while R and its packages download. Later visits are faster (cached).",
      "Did you know? Smaller effect sizes need disproportionately larger samples to detect reliably.",
      "Power (1 \\u2212 \\u03b2) and alpha (Type I error rate) are two independent knobs \\u2014 tightening one usually loosens the other unless you also grow the sample.",
      "Cohen\\u2019s small/medium/large benchmarks are convenient defaults, not universal truths for every field.",
      "A priori power analysis is meant to be done before collecting data, not after \\u2014 that is what makes it \\"a priori\\".",
      "Group-randomized (clustered) designs almost always need more participants than individually randomized ones, for the same effect size."
    ];
    var tipIdx = 0;
    function showTip(i) {
      if (!tipEl) return;
      tipEl.style.opacity = "0";
      setTimeout(function () {
        tipEl.textContent = tips[i];
        tipEl.style.opacity = "1";
      }, 280);
    }
    var tipRotator = setInterval(function () {
      if (done) return;
      tipIdx = (tipIdx + 1) % tips.length;
      showTip(tipIdx);
    }, 4500);

    // The real app runs inside a same-origin <iframe class="app-frame">
    // that shinylive creates dynamically (not in this top-level document),
    // so readiness has to be checked *inside* that iframe once it exists --
    // checking only the outer document (as an earlier version of this did)
    // meant the bar could never see #analysis_choice and got stuck at the
    // cap. Polled alongside the animation tick rather than via a nested
    // MutationObserver, since the iframe element itself, and then its
    // document, both appear asynchronously and can be re-created/navigated;
    // wrapped in try/catch as a pure safety net (should always be
    // same-origin here) so a lookup failure just falls through to the
    // fallback timeout instead of breaking anything.
    function isReady() {
      try {
        var f = document.querySelector("iframe.app-frame");
        if (!f) return false;
        var d = f.contentDocument || (f.contentWindow && f.contentWindow.document);
        if (!d) return false;
        return !!d.getElementById("analysis_choice");
      } catch (e) {
        return false;
      }
    }

    // Eases quickly at first, then decelerates -- never reaches `cap` on its
    // own, only real completion (or the fallback timeout) pushes past it.
    var tick = setInterval(function () {
      if (done) return;
      if (isReady()) { finish(); return; }
      var remaining = cap - current;
      current += remaining * 0.06;
      setPct(current);
    }, 200);

    function finish() {
      if (done) return;
      done = true;
      clearInterval(tick);
      clearInterval(tipRotator);
      setPct(100);
      setTimeout(hide, 250);
    }

    function hide() {
      var el = document.getElementById("pw-boot-splash");
      if (!el) return;
      el.style.opacity = "0";
      setTimeout(function () { el.remove(); }, 320);
    }

    setTimeout(finish, 120000);
  })();
</script>
'

shinylive::export(
  appdir = stage_dir,
  destdir = destdir,
  template_params = list(
    title = "A Priori Power Analysis Wizard",
    include_before_body = splash_html
  )
)

unlink(stage_dir, recursive = TRUE)

message(sprintf(
  "\nDone. Static site written to ./%s\n\nPreview locally with:\n  httpuv::runStaticServer(\"%s\")\n\nThen deploy the contents of ./%s to GitHub Pages, Netlify, or any static host.\nSee README.md, 'Browser-only deployment', for the GitHub Pages walkthrough\nand the GitHub Actions workflow that automates this export on every push.",
  destdir, destdir, destdir
))

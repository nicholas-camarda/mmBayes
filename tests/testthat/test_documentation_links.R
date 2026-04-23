test_that("methods guide is linked from the root readme", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    methods_path <- file.path(repo_root, "docs", "methods-and-interpretation.md")
    expect_true(file.exists(methods_path))

    readme_text <- paste(readLines(file.path(repo_root, "README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "methods-and-interpretation\\.md")
    expect_match(readme_text, "nicholas-camarda\\.github\\.io/mmBayes")
})

test_that("GitHub Pages landing page links to the public dashboards", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    index_path <- file.path(repo_root, "index.html")
    expect_true(file.exists(index_path))
    expect_true(file.exists(file.path(repo_root, ".nojekyll")))

    index_text <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
    expect_match(index_text, "output/bracket_dashboard\\.html")
    expect_no_match(index_text, "output/technical_dashboard\\.html")
    expect_no_match(index_text, "output/model_comparison_dashboard\\.html")
})

test_that("methods guide documents that betting work is parked off master", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    methods_text <- paste(readLines(file.path(repo_root, "docs", "methods-and-interpretation.md"), warn = FALSE), collapse = "\n")

    expect_match(methods_text, "betting-line experimentation is not part of the supported workflow")
    expect_match(methods_text, "betting-lines-spike")
})

test_that("README documents the dashboard regeneration workflow and command roles", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    readme_text <- paste(readLines(file.path(repo_root, "README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "scripts/regenerate_and_sync_dashboards\\.R")
    expect_match(readme_text, "Preferred dashboard-refresh command")
    expect_match(readme_text, "run_simulation\\.R.*syncs the tracked repo dashboard HTML snapshot|successful `Rscript scripts/run_simulation\\.R` updates both the runtime HTML bundle and the GitHub Pages source files")
    expect_match(readme_text, "run_bracket_candidates\\.R.*not the right command for CSS/layout-only iteration")
    expect_match(readme_text, "publish_github_pages\\.R.*Lower-level sync helper")
    expect_match(readme_text, "configured runtime output directory")
    expect_match(readme_text, "dashboard HTML snapshots")
    expect_match(readme_text, "CSV/TXT/RDS outputs live in runtime/release bundles|CSV, TXT, RDS, cache, and log artifacts")
})

test_that("README quick start explains the runtime output bundle and browser launch step", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    readme_text <- paste(readLines(file.path(repo_root, "README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "Open `bracket_dashboard\\.html` in that directory first")
    expect_match(readme_text, "Expect this step to take longer than the refresh step")
    expect_match(readme_text, "Then open `bracket_dashboard\\.html` from the configured runtime output directory in your browser")
})

test_that("README explains update_data refresh status outcomes", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    readme_text <- paste(readLines(file.path(repo_root, "README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "Refresh status: Success")
    expect_match(readme_text, "Refresh status: Degraded success")
    expect_match(readme_text, "do not continue to simulation")
})

test_that("methods guide documents operational entrypoint roles", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    methods_text <- paste(readLines(file.path(repo_root, "docs", "methods-and-interpretation.md"), warn = FALSE), collapse = "\n")

    expect_match(methods_text, "Operational Entrypoints")
    expect_match(methods_text, "scripts/run_simulation\\.R")
    expect_match(methods_text, "syncs the tracked repo dashboard HTML snapshot")
    expect_match(methods_text, "scripts/run_bracket_candidates\\.R")
    expect_match(methods_text, "scripts/regenerate_and_sync_dashboards\\.R")
    expect_match(methods_text, "scripts/publish_github_pages\\.R")
})

test_that("runtime and methods docs describe current artifact and model contracts", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    runtime_text <- paste(readLines(file.path(repo_root, "docs", "runtime-roots.md"), warn = FALSE), collapse = "\n")
    methods_text <- paste(readLines(file.path(repo_root, "docs", "methods-and-interpretation.md"), warn = FALSE), collapse = "\n")

    expect_match(runtime_text, "deliverables/")
    expect_match(runtime_text, "plain-text release manifest")
    expect_no_match(runtime_text, "data_snapshot/")
    expect_match(methods_text, "prior_type")
    expect_match(methods_text, "globalScale")
    expect_match(methods_text, "effective_historical_years")
    expect_match(methods_text, "coherent shared posterior draws")
    expect_match(methods_text, "predictive diagnostics")
})

test_that("methods guide is linked from the root readme", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    methods_path <- file.path(repo_root, "docs", "methods-and-interpretation.md")
    expect_true(file.exists(methods_path))

    readme_text <- paste(readLines(file.path(repo_root, "README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "methods-and-interpretation\\.md")
    expect_match(readme_text, "nicholas-camarda\\.github\\.io/mmBayes")
})

test_that("README table of contents only points to live headings", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    readme_lines <- readLines(file.path(repo_root, "README.md"), warn = FALSE)
    readme_text <- paste(readme_lines, collapse = "\n")

    heading_text <- sub("^#+\\s+", "", grep("^#+\\s+", readme_lines, value = TRUE))
    heading_anchors <- tolower(heading_text)
    heading_anchors <- gsub("`", "", heading_anchors, fixed = TRUE)
    heading_anchors <- gsub("[^a-z0-9 -]", "", heading_anchors)
    heading_anchors <- gsub("\\s+", "-", heading_anchors)

    toc_links <- regmatches(readme_text, gregexpr("\\[[^]]+\\]\\(#[^)]+\\)", readme_text, perl = TRUE))[[1]]
    toc_anchors <- sub("^.*\\]\\(#", "", sub("\\)$", "", toc_links))

    expect_equal(setdiff(toc_anchors, heading_anchors), character())
    expect_no_match(readme_text, "Branch Notes")
})

test_that("GitHub Pages landing page links to the public dashboards", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    index_path <- file.path(repo_root, "index.html")
    expect_true(file.exists(index_path))
    expect_true(file.exists(file.path(repo_root, ".nojekyll")))

    index_text <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
    expect_match(index_text, "output/app/index\\.html")
    expect_match(index_text, "output/app/technical\\.html")
    expect_no_match(index_text, "output/bracket_dashboard\\.html")
    expect_no_match(index_text, "output/technical_dashboard\\.html")
    expect_no_match(index_text, "Legacy HTML")
    expect_no_match(index_text, "backward compatibility")
    expect_no_match(index_text, "output/model_comparison_dashboard\\.html")
    expect_match(index_text, "color-scheme: dark")
    expect_no_match(index_text, "color-scheme: light")
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
    expect_match(readme_text, "Full pipeline: fit.*backtest.*simulate.*export")
    expect_match(readme_text, "Lighter run: model fit.*candidates, no full backtest")
    expect_match(readme_text, "configured runtime output directory")
    expect_match(readme_text, "React dashboard app")
    expect_no_match(readme_text, "Legacy HTML")
    expect_no_match(readme_text, "legacy R-rendered")
    expect_no_match(readme_text, "backward compatibility")
    expect_no_match(readme_text, "bracket_dashboard\\.html")
    expect_no_match(readme_text, "technical_dashboard\\.html")
    expect_match(readme_text, "Decision Artifacts.*CSV")
    expect_match(readme_text, "Summaries.*TXT")
    expect_match(readme_text, "Runtime Artifacts")
})

test_that("README quick start explains the runtime output bundle and browser launch step", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    readme_text <- paste(readLines(file.path(repo_root, "README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "Open `output/app/index\\.html` in your browser")
    expect_match(readme_text, "dashboard loads the latest simulation payload automatically")
    expect_match(readme_text, "dashboard-only iteration when models haven't changed")
})

test_that("methods guide documents operational entrypoint roles", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    methods_text <- paste(readLines(file.path(repo_root, "docs", "methods-and-interpretation.md"), warn = FALSE), collapse = "\n")

    expect_match(methods_text, "Operational Entrypoints")
    expect_match(methods_text, "scripts/run_simulation\\.R")
    expect_match(methods_text, "syncs the React dashboard app under `output/app/`")
    expect_match(methods_text, "scripts/run_bracket_candidates\\.R")
    expect_match(methods_text, "scripts/regenerate_and_sync_dashboards\\.R")
    expect_match(methods_text, "scripts/publish_github_pages\\.R")
    expect_match(methods_text, "publish_github_pages\\.R.*internal plumbing")
})

test_that("publish_github_pages script syncs only the React app bundle", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    script_text <- paste(readLines(file.path(repo_root, "scripts", "publish_github_pages.R"), warn = FALSE), collapse = "\n")

    expect_match(script_text, "Internal helper")
    expect_match(script_text, "output/app")
    expect_match(script_text, "default_runtime_output_root")
    expect_no_match(script_text, "dashboard_html_manifest\\(\\)")
    expect_no_match(script_text, "bracket_dashboard\\.html")
    expect_no_match(script_text, "technical_dashboard\\.html")
    expect_no_match(script_text, "which.max\\(bundle_mtime\\)")
    expect_no_match(script_text, "candidate_roots")
    expect_no_match(script_text, "workflow_redesign_preview")
    expect_match(script_text, "does not regenerate dashboards")
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

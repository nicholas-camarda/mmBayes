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

test_that("methods guide documents betting-line integration", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    methods_text <- paste(readLines(file.path(repo_root, "docs", "methods-and-interpretation.md"), warn = FALSE), collapse = "\n")

    expect_match(methods_text, "Betting-Line Integration")
    expect_match(methods_text, "blend_rounds")
    expect_match(methods_text, "p_\\{\\\\text\\{blend\\}\\}")
})

test_that("README documents the dashboard regeneration workflow and command roles", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    readme_text <- paste(readLines(file.path(repo_root, "README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "scripts/regenerate_dashboards\\.R")
    expect_match(readme_text, "Preferred dashboard-refresh command")
    expect_match(readme_text, "run_bracket_candidates\\.R.*not the right command for CSS/layout-only iteration")
    expect_match(readme_text, "publish_github_pages\\.R.*Lower-level sync helper")
    expect_match(readme_text, "~/ProjectsRuntime/mmBayes/output")
    expect_match(readme_text, "macOS: use `open`")
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
    expect_match(methods_text, "scripts/run_bracket_candidates\\.R")
    expect_match(methods_text, "scripts/regenerate_dashboards\\.R")
    expect_match(methods_text, "scripts/publish_github_pages\\.R")
})

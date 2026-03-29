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

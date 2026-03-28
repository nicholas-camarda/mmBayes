test_that("methods guide is linked from the root readme", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    methods_path <- file.path(repo_root, "docs", "methods-and-interpretation.md")
    expect_true(file.exists(methods_path))

    readme_text <- paste(readLines(file.path(repo_root, "README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "methods-and-interpretation\\.md")
})

test_that("methods guide documents betting-line integration", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    methods_text <- paste(readLines(file.path(repo_root, "docs", "methods-and-interpretation.md"), warn = FALSE), collapse = "\n")

    expect_match(methods_text, "Betting-Line Integration")
    expect_match(methods_text, "blend_rounds")
    expect_match(methods_text, "p_\\{\\\\text\\{blend\\}\\}")
})

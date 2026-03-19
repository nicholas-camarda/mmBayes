test_that("methods guide is linked from the readmes", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    methods_path <- file.path(repo_root, "docs", "methods-and-interpretation.md")
    expect_true(file.exists(methods_path))

    readme_text <- paste(readLines(file.path(repo_root, "README.md"), warn = FALSE), collapse = "\n")
    docs_readme_text <- paste(readLines(file.path(repo_root, "docs", "README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "methods-and-interpretation\\.md")
    expect_match(docs_readme_text, "methods-and-interpretation\\.md")
})

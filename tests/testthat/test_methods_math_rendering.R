test_that("methods guide does not use unescaped underscores in LaTeX text commands", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    methods_path <- file.path(repo_root, "docs", "methods-and-interpretation.md")
    expect_true(file.exists(methods_path))

    lines <- readLines(methods_path, warn = FALSE)
    for (i in seq_along(lines)) {
        line <- lines[[i]]
        text_blocks <- gregexpr("\\\\text\\{[^}]*\\}", line, perl = TRUE)[[1]]
        if (identical(text_blocks, -1L)) {
            next
        }
        blocks <- regmatches(line, gregexpr("\\\\text\\{[^}]*\\}", line, perl = TRUE))[[1]]
        for (block in blocks) {
            inner <- sub("^\\\\text\\{", "", sub("\\}$", "", block))
            expect_false(
                grepl("(?<!\\\\)_", inner, perl = TRUE),
                info = sprintf("Unescaped underscore in line %d: %s", i, block)
            )
        }
    }
})

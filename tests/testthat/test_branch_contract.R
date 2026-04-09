test_that("master test surface does not reference retired betting helpers", {
    forbidden_patterns <- c(
        paste0("american_to_", "implied_prob\\s*\\("),
        paste0("remove_vig_", "two_way\\s*\\(")
    )
    test_files <- list.files(file.path("tests", "testthat"), pattern = "\\.R$", full.names = TRUE)
    test_contents <- vapply(test_files, function(path) {
        paste(readLines(path, warn = FALSE), collapse = "\n")
    }, character(1))

    offenders <- unlist(lapply(forbidden_patterns, function(pattern) {
        names(test_contents)[grepl(pattern, test_contents, perl = TRUE)]
    }), use.names = FALSE)

    expect_true(length(unique(offenders)) == 0L)
})

test_that("load_dotenv_file loads key-value pairs without overriding by default", {
    env_file <- tempfile(fileext = ".env")
    writeLines(c(
        "# comment",
        "FOO=bar",
        "QUOTED=\"baz\"",
        "WITH_EQUALS=a=b=c"
    ), env_file)

    old <- Sys.getenv(c("FOO", "QUOTED", "WITH_EQUALS"), unset = NA_character_)
    on.exit({
        if (!is.na(old[[1]])) Sys.setenv(FOO = old[[1]]) else Sys.unsetenv("FOO")
        if (!is.na(old[[2]])) Sys.setenv(QUOTED = old[[2]]) else Sys.unsetenv("QUOTED")
        if (!is.na(old[[3]])) Sys.setenv(WITH_EQUALS = old[[3]]) else Sys.unsetenv("WITH_EQUALS")
    }, add = TRUE)

    Sys.unsetenv(c("FOO", "QUOTED", "WITH_EQUALS"))
    loaded <- load_dotenv_file(env_file, override = FALSE)

    expect_true(all(c("FOO", "QUOTED", "WITH_EQUALS") %in% loaded))
    expect_equal(Sys.getenv("FOO"), "bar")
    expect_equal(Sys.getenv("QUOTED"), "baz")
    expect_equal(Sys.getenv("WITH_EQUALS"), "a=b=c")
})

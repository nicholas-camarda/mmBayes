test_that("project roots point at the local code, runtime, cloud, and publish paths", {
    roots <- project_roots(as.Date("2026-03-27"))

    expect_equal(roots$code_root, path.expand("~/Projects/mmBayes"))
    expect_equal(roots$runtime_root, path.expand("~/ProjectsRuntime/mmBayes"))
    expect_equal(
        roots$cloud_root,
        path.expand("~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes")
    )
    expect_equal(roots$data_root, file.path(roots$cloud_root, "data"))
    expect_equal(roots$output_root, file.path(roots$runtime_root, "output"))
    expect_equal(
        roots$publish_root,
        path.expand("~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes")
    )
    expect_equal(roots$release_root, file.path(roots$publish_root, "releases", "2026-03-27"))
})

test_that("publish_release_bundle copies only approved deliverables", {
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    output_dir <- file.path(runtime_root, "output")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(output_dir, "model_cache"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(output_dir, "logs"), recursive = TRUE, showWarnings = FALSE)
    candidate_files <- c("bracket_candidate_1.csv", "bracket_candidate_2.csv", "bracket_candidate_3.csv")

    for (filename in release_deliverable_manifest()) {
        target_path <- file.path(output_dir, filename)
        if (identical(filename, "app")) {
            dir.create(target_path, recursive = TRUE, showWarnings = FALSE)
            writeLines("<html></html>", file.path(target_path, "index.html"))
            writeLines("<html></html>", file.path(target_path, "technical.html"))
            writeLines("window.__MMBAYES_PAYLOADS__ = {};", file.path(target_path, "dashboard_payloads.js"))
        } else {
            writeLines(sprintf("fixture:%s", filename), target_path)
        }
    }
    for (filename in candidate_files) {
        writeLines(sprintf("fixture:%s", filename), file.path(output_dir, filename))
    }
    writeLines("scratch", file.path(output_dir, "tournament_sim.rds"))
    writeLines("scratch", file.path(output_dir, "simulation_results.rds"))
    writeLines("scratch", file.path(output_dir, "tournament_sim_bracket.png"))
    writeLines("scratch", file.path(output_dir, "betting_bracket_impact_2026_report.md"))
    writeLines("cache", file.path(output_dir, "model_cache", "cached-fit.rds"))
    writeLines("log", file.path(output_dir, "logs", "tournament_simulation.log"))

    config <- default_project_config()
    config$output$path <- output_dir

    publish_root <- tempfile(pattern = "mmBayes-publish-")
    result <- publish_release_bundle(
        config = config,
        release_date = as.Date("2026-03-27"),
        publish_root = publish_root
    )

    expect_true(dir.exists(result$release_root))
    expect_true(file.exists(result$manifest_path))
    expect_equal(
        sort(list.files(result$deliverables_dir, recursive = TRUE)),
        sort(c(
            "app/dashboard_payloads.js",
            "app/index.html",
            "app/technical.html",
            setdiff(release_deliverable_manifest(), "app"),
            candidate_files
        ))
    )
    expect_false(file.exists(file.path(result$deliverables_dir, "tournament_sim.rds")))
    expect_false(file.exists(file.path(result$deliverables_dir, "simulation_results.rds")))
    expect_false(file.exists(file.path(result$deliverables_dir, "tournament_sim_bracket.png")))
    expect_false(file.exists(file.path(result$deliverables_dir, "betting_bracket_impact_2026_report.md")))
    expect_false(dir.exists(file.path(result$deliverables_dir, "model_cache")))
    expect_false(dir.exists(file.path(result$deliverables_dir, "logs")))
    expect_false(dir.exists(file.path(result$release_root, "data_snapshot")))
    expect_false(file.exists(file.path(result$deliverables_dir, "bracket_dashboard.html")))
    expect_false(file.exists(file.path(result$deliverables_dir, "technical_dashboard.html")))
    manifest_text <- paste(readLines(result$manifest_path, warn = FALSE), collapse = "\n")
    expect_match(manifest_text, "bracket_candidate_3\\.csv")
    expect_match(manifest_text, "source: configured runtime output directory")
    expect_match(manifest_text, "deliverables_dir: deliverables")
    expect_no_match(manifest_text, "bracket_dashboard\\.html")
    expect_no_match(manifest_text, "technical_dashboard\\.html")
    expect_false(grepl(output_dir, manifest_text, fixed = TRUE))
})

test_that("release deliverable manifest publishes only the React dashboard app", {
    manifest <- release_deliverable_manifest()
    expect_true("app" %in% manifest)
    expect_false("bracket_dashboard.html" %in% manifest)
    expect_false("technical_dashboard.html" %in% manifest)
    expect_false("model_comparison_dashboard.html" %in% manifest)
})

test_that("publish_release_bundle fails clearly when the React app deliverable is incomplete", {
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    output_dir <- file.path(runtime_root, "output")
    app_dir <- file.path(output_dir, "app")
    dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(runtime_root, recursive = TRUE), add = TRUE)

    config <- default_project_config()
    config$runtime$root <- runtime_root
    config$output$path <- output_dir
    config <- normalize_project_paths(config)

    for (filename in setdiff(release_deliverable_manifest(), "app")) {
        writeLines(sprintf("fixture:%s", filename), file.path(output_dir, filename))
    }
    writeLines("fixture:bracket_candidate_1.csv", file.path(output_dir, "bracket_candidate_1.csv"))
    writeLines("<html>bracket app</html>", file.path(app_dir, "index.html"))

    expect_error(
        publish_release_bundle(
            config = config,
            release_date = as.Date("2026-03-31"),
            publish_root = tempfile(pattern = "mmBayes-publish-")
        ),
        regexp = "React dashboard app deliverable is incomplete.*technical\\.html.*dashboard_payloads\\.js"
    )
})

test_that("publish_release_bundle defaults to the runtime output contract and fails without candidate CSVs", {
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    output_dir <- file.path(runtime_root, "output")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    for (filename in release_deliverable_manifest()) {
        target_path <- file.path(output_dir, filename)
        if (identical(filename, "app")) {
            dir.create(target_path, recursive = TRUE, showWarnings = FALSE)
            writeLines("<html></html>", file.path(target_path, "index.html"))
            writeLines("<html></html>", file.path(target_path, "technical.html"))
            writeLines("window.__MMBAYES_PAYLOADS__ = {};", file.path(target_path, "dashboard_payloads.js"))
        } else {
            writeLines(sprintf("fixture:%s", filename), target_path)
        }
    }

    config <- default_project_config()
    config$runtime$root <- runtime_root
    config$output$path <- NULL
    config <- normalize_project_paths(config)

    expect_error(
        publish_release_bundle(
            config = config,
            release_date = as.Date("2026-03-28"),
            publish_root = tempfile(pattern = "mmBayes-publish-")
        ),
        regexp = "No bracket candidate CSV files found in runtime output directory"
    )

    writeLines("fixture:bracket_candidate_1.csv", file.path(output_dir, "bracket_candidate_1.csv"))

    publish_root <- tempfile(pattern = "mmBayes-publish-")
    result <- publish_release_bundle(
        config = config,
        release_date = as.Date("2026-03-29"),
        publish_root = publish_root
    )

    expect_equal(result$output_dir, output_dir)
})

test_that("regenerate_dashboards_from_saved_results fails clearly when the saved results bundle is missing", {
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    output_dir <- file.path(runtime_root, "output")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    config <- default_project_config()
    config$runtime$root <- runtime_root
    config$output$path <- output_dir
    config$output$prefix <- "fixture"
    config <- normalize_project_paths(config)

    expect_error(
        regenerate_dashboards_from_saved_results(
            config = config,
            repo_output_dir = tempfile(pattern = "mmBayes-repo-output-")
        ),
        regexp = "Saved results bundle not found: .*fixture\\.rds.*run_simulation\\.R"
    )
})

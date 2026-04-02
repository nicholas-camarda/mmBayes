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
    expect_equal(roots$history_root, file.path(roots$runtime_root, "data", "odds_history"))
    expect_equal(
        roots$publish_root,
        path.expand("~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes")
    )
    expect_equal(roots$release_root, file.path(roots$publish_root, "releases", "2026-03-27"))
})

test_that("publish_release_bundle copies only approved deliverables and the odds-history snapshot", {
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    output_dir <- file.path(runtime_root, "output")
    history_dir <- file.path(runtime_root, "data", "odds_history")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(output_dir, "model_cache"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(output_dir, "logs"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(history_dir, "2026", "snapshots"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(history_dir, "2025"), recursive = TRUE, showWarnings = FALSE)

    for (filename in release_deliverable_manifest()) {
        writeLines(sprintf("fixture:%s", filename), file.path(output_dir, filename))
    }
    writeLines("scratch", file.path(output_dir, "tournament_sim.rds"))
    writeLines("cache", file.path(output_dir, "model_cache", "cached-fit.rds"))
    writeLines("log", file.path(output_dir, "logs", "tournament_simulation.log"))

    writeLines("lines", file.path(history_dir, "2026", "lines_matchups.csv"))
    writeLines("latest", file.path(history_dir, "2026", "latest_lines_matchups.csv"))
    writeLines("closing", file.path(history_dir, "2026", "closing_lines.csv"))
    writeLines("snapshot", file.path(history_dir, "2026", "snapshots", "odds_api_20260327_120000.json"))
    writeLines("history", file.path(history_dir, "2025", "closing_lines.csv"))

    config <- default_project_config()
    config$output$path <- output_dir
    config$betting$history_dir <- history_dir

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
        sort(release_deliverable_manifest())
    )
    expect_false(file.exists(file.path(result$deliverables_dir, "tournament_sim.rds")))
    expect_false(dir.exists(file.path(result$deliverables_dir, "model_cache")))
    expect_false(dir.exists(file.path(result$deliverables_dir, "logs")))
    expect_true(file.exists(file.path(result$data_snapshot_dir, "odds_history", "2026", "lines_matchups.csv")))
    expect_true(file.exists(file.path(result$data_snapshot_dir, "odds_history", "2026", "snapshots", "odds_api_20260327_120000.json")))
    expect_true(file.exists(file.path(result$data_snapshot_dir, "odds_history", "2025", "closing_lines.csv")))
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

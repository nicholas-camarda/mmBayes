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

    for (filename in release_deliverable_manifest()) {
        writeLines(sprintf("fixture:%s", filename), file.path(output_dir, filename))
    }
    writeLines("scratch", file.path(output_dir, "tournament_sim.rds"))
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
        sort(release_deliverable_manifest())
    )
    expect_false(file.exists(file.path(result$deliverables_dir, "tournament_sim.rds")))
    expect_false(dir.exists(file.path(result$deliverables_dir, "model_cache")))
    expect_false(dir.exists(file.path(result$deliverables_dir, "logs")))
    expect_false(dir.exists(file.path(result$release_root, "data_snapshot")))
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

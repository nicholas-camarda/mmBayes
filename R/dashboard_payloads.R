#' Drop NULL entries from a payload list
#'
#' @param payload A named list.
#'
#' @return The list with NULL members removed.
#' @keywords internal
compact_payload <- function(payload) {
    payload[!vapply(payload, is.null, logical(1))]
}

#' Build the versioned bracket dashboard payload
#'
#' @param bracket_year The active bracket year.
#' @param candidates A list of candidate bracket objects.
#' @param decision_sheet Decision sheet from [build_decision_sheet()].
#' @param dashboard_context Optional bundle from [build_bracket_dashboard_context()].
#' @param total_points_predictions Optional tiebreaker prediction bundle.
#' @param play_in_resolution Optional one-row play-in resolution tibble.
#'
#' @return A named list satisfying inst/schemas/bracket_dashboard_payload.schema.json.
#' @keywords internal
build_bracket_dashboard_payload <- function(bracket_year,
                                            candidates,
                                            decision_sheet,
                                            dashboard_context = NULL,
                                            total_points_predictions = NULL,
                                            play_in_resolution = NULL) {
    candidate_entries <- lapply(candidates, function(candidate) {
        compact_payload(list(
            candidate_id = as.integer(candidate$candidate_id),
            type = as.character(candidate$type),
            champion = as.character(candidate$champion),
            final_four = as.list(trimws(strsplit(
                as.character(candidate$final_four %||% ""), ","
            )[[1]])),
            bracket_log_prob = candidate$bracket_log_prob %||% NULL,
            mean_game_prob = candidate$mean_game_prob %||% NULL,
            title_path_mean_prob = candidate$title_path_mean_prob %||% NULL,
            path_support_label = candidate$path_support_label %||% NULL,
            matchups = as.data.frame(candidate$matchups)
        ))
    })

    matchup_context <- dashboard_context$matchup_context_rows
    candidate_summaries <- total_points_predictions$candidate_summaries

    compact_payload(list(
        dashboard_schema_version = dashboard_payload_schema_version(),
        dashboard = "bracket",
        bracket_year = as.integer(bracket_year),
        generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        build_metadata = dashboard_context$build_metadata %||% list(),
        candidates = candidate_entries,
        decision_sheet = as.data.frame(decision_sheet),
        matchup_context = if (is.data.frame(matchup_context) && nrow(matchup_context) > 0) as.data.frame(matchup_context) else NULL,
        candidate_summaries = if (is.data.frame(candidate_summaries) && nrow(candidate_summaries) > 0) as.data.frame(candidate_summaries) else NULL,
        play_in_resolution = if (is.data.frame(play_in_resolution) && nrow(play_in_resolution) > 0) as.data.frame(play_in_resolution) else NULL
    ))
}

#' Build the versioned technical dashboard payload
#'
#' @param bracket_year The active bracket year.
#' @param decision_sheet Optional decision sheet used for summary counts.
#' @param candidates Optional list of candidate bracket objects.
#' @param model_quality_context Optional resolved model-quality context.
#' @param build_metadata Optional build metadata list.
#'
#' @return A named list satisfying inst/schemas/technical_dashboard_payload.schema.json.
#' @keywords internal
build_technical_dashboard_payload <- function(bracket_year,
                                              decision_sheet = NULL,
                                              candidates = list(),
                                              model_quality_context = NULL,
                                              build_metadata = NULL) {
    decision_summary <- NULL
    if (is.data.frame(decision_sheet) && nrow(decision_sheet) > 0) {
        decision_summary <- list(
            n_decisions = nrow(decision_sheet),
            n_divergent = if ("candidate_diff_flag" %in% names(decision_sheet)) {
                sum(decision_sheet$candidate_diff_flag %in% TRUE)
            } else {
                0L
            },
            confidence_tiers = if ("confidence_tier" %in% names(decision_sheet)) {
                as.list(table(as.character(decision_sheet$confidence_tier)))
            } else {
                list()
            }
        )
    }

    model_quality <- NULL
    if (!is.null(model_quality_context)) {
        model_quality <- compact_payload(list(
            source_label = model_quality_context$source_label %||% NULL,
            used_cached_quality = isTRUE(
                model_quality_context$used_cached_quality %||% model_quality_context$used_fallback %||% FALSE
            )
        ))
    }

    compact_payload(list(
        dashboard_schema_version = dashboard_payload_schema_version(),
        dashboard = "technical",
        bracket_year = as.integer(bracket_year),
        generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        build_metadata = build_metadata %||% list(),
        model_quality = model_quality,
        decision_summary = decision_summary,
        candidate_count = length(candidates)
    ))
}

#' Serialize a dashboard payload list to JSON
#'
#' @param payload A payload list from the dashboard payload builders.
#'
#' @return A length-one JSON character string with rows-oriented data frames.
#' @keywords internal
dashboard_payload_json <- function(payload) {
    as.character(jsonlite::toJSON(
        payload,
        dataframe = "rows",
        auto_unbox = TRUE,
        na = "null",
        digits = 6,
        null = "null"
    ))
}

#' Validate and write dashboard payload artifacts
#'
#' Writes the bracket and technical payload JSON files plus a
#' dashboard_payloads.js window-global shim used by the static frontend
#' over file:// where fetch() is unavailable.
#'
#' @param bracket_payload Payload from [build_bracket_dashboard_payload()].
#' @param technical_payload Payload from [build_technical_dashboard_payload()].
#' @param output_dir Directory receiving the payload artifacts.
#'
#' @return A list with bracket, technical, and js artifact paths.
#' @keywords internal
write_dashboard_payloads <- function(bracket_payload, technical_payload, output_dir) {
    validate_dashboard_payload(bracket_payload, "bracket")
    validate_dashboard_payload(technical_payload, "technical")
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    bracket_json <- dashboard_payload_json(bracket_payload)
    technical_json <- dashboard_payload_json(technical_payload)

    bracket_path <- file.path(output_dir, "bracket_dashboard_payload.json")
    technical_path <- file.path(output_dir, "technical_dashboard_payload.json")
    js_path <- file.path(output_dir, "dashboard_payloads.js")

    writeLines(bracket_json, bracket_path, useBytes = TRUE)
    writeLines(technical_json, technical_path, useBytes = TRUE)
    writeLines(sprintf(
        "window.__MMBAYES_PAYLOADS__ = {\"bracket\": %s, \"technical\": %s};",
        bracket_json, technical_json
    ), js_path, useBytes = TRUE)

    list(bracket = bracket_path, technical = technical_path, js = js_path)
}

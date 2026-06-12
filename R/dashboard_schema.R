#' Current dashboard payload schema version
#'
#' @return The semantic version string for emitted dashboard payloads.
#' @keywords internal
dashboard_payload_schema_version <- function(dashboard = c("bracket", "technical")) {
    dashboard <- match.arg(dashboard)
    switch(
        dashboard,
        bracket = "1.1.0",
        technical = "1.1.0"
    )
}

#' Resolve the path to a dashboard payload schema file
#'
#' @param dashboard Either "bracket" or "technical".
#'
#' @return Absolute path to the JSON Schema file.
#' @keywords internal
dashboard_schema_path <- function(dashboard = c("bracket", "technical")) {
    dashboard <- match.arg(dashboard)
    file_name <- sprintf("%s_dashboard_payload.schema.json", dashboard)
    path <- system.file("schemas", file_name, package = "mmBayes")
    if (!nzchar(path)) {
        stop(sprintf("Dashboard schema file not found: %s", file_name), call. = FALSE)
    }
    path
}

#' Validate a dashboard payload against its versioned schema
#'
#' Fails closed: any missing required field, type mismatch on a required
#' field, or schema-version mismatch aborts with an informative error.
#' Optional sections may be absent.
#'
#' @param payload A named list payload built by the dashboard payload builders.
#' @param dashboard Either "bracket" or "technical".
#'
#' @return Invisibly TRUE when the payload is valid.
#' @keywords internal
validate_dashboard_payload <- function(payload, dashboard = c("bracket", "technical")) {
    dashboard <- match.arg(dashboard)
    schema <- jsonlite::fromJSON(dashboard_schema_path(dashboard), simplifyVector = FALSE)
    problems <- character(0)

    required_fields <- unlist(schema$required)
    for (field in required_fields) {
        if (is.null(payload[[field]])) {
            problems <- c(problems, sprintf("missing required field: %s", field))
        }
    }

    for (field in names(schema$properties)) {
        value <- payload[[field]]
        if (is.null(value)) next
        spec <- schema$properties[[field]]
        if (!is.null(spec$const) && !identical(as.character(value), as.character(spec$const))) {
            problems <- c(problems, sprintf(
                "field %s must equal %s (got %s)", field, spec$const, as.character(value)
            ))
        }
        expected_type <- spec$type
        if (is.null(expected_type) || length(expected_type) != 1L) next
        ok <- switch(expected_type,
            string = is.character(value) && length(value) == 1L,
            integer = is.numeric(value) && length(value) == 1L && value == as.integer(value),
            number = is.numeric(value) && length(value) == 1L,
            object = is.list(value) && !is.data.frame(value),
            array = is.data.frame(value) || (is.list(value) && is.null(names(value))) || length(value) == 0L,
            TRUE
        )
        if (!isTRUE(ok)) {
            problems <- c(problems, sprintf("field %s is not of type %s", field, expected_type))
        }
    }

    if (length(problems) > 0) {
        stop(sprintf(
            "Invalid %s dashboard payload: %s",
            dashboard, paste(problems, collapse = "; ")
        ), call. = FALSE)
    }
    invisible(TRUE)
}

service <- function(sheet_id, yt, poll) {
  project_id <- find_project_id(yt, "mrc")
  template <- paste(readLines("template"), collapse = "\n")
  repeat {
    service_step(sheet_id, yt, template, project_id)
    Sys.sleep(poll)
  }
}


service_step <- function(sheet_id, yt, template, project_id) {
  gs2yt_log("Reading googlesheet issues")
  issues <- read_googlesheet_issues(sheet_id)
  gs2yt_log(sprintf("Found %s issues", nrow(issues)))
  if (!is.null(issues)) {
    map <- read_map()
    new_rows <- setdiff(seq_len(nrow(issues)), map$row)
    new_issues <- issues[new_rows, ]
    if (nrow(new_issues) > 0L) {
      ids <- create_issues(new_issues, yt, template, project_id)
      map <- data.frame(row = new_rows, issue = ids, stringsAsFactors = FALSE)
      write_map(map)
    }
  }
}


create_issues <- function(to_import, yt, template, project_id) {
  n <- nrow(to_import)
  if (n == 0) {
    return(character(0))
  }
  gs2yt_log(ngettext(n,
                     "Importing 1 issue",
                     sprintf("Importing %d issues", n)))
  ret <- rep(NA, n)
  for (i in seq_len(n)) {
    id <- tryCatch(
      create_issue(as.list(to_import[i, ]), yt, template, project_id),
      error = identity)
    if (inherits(id, "error")) {
      gs2yt_log(sprintf(" => Error creating issue: %s", id$message))
    } else {
      ret[[i]] <- id
      gs2yt_log(sprintf(" => Created %s", id))
    }
  }
  ret
}


create_issue <- function(d, yt, template, project_id) {
  names(d) <- gsub("[ /]", "_", names(d))

  title <- clean_title(d$Description_of_bug_change)
  description <- glue::glue(template, .envir = d)

  body <- list(summary = scalar(title),
               description = scalar(description),
               project = list(id = scalar(project_id)),
               customFields = list(
                 list(
                   value = list(
                     name = scalar("HINT"),
                     "$type" = scalar("OwnedBundleElement")),
                   name = "Component",
                   "$type" = "SingleOwnedIssueCustomField")))

  query <- list(fields = "id,idReadable")
  dat <- yt$POST("/issues", body = body, query = query)

  body <- list(issues = list(
                 list(id = scalar(dat$id))),
               query = scalar("subtask of: mrc-748"))
  yt$POST("/commands", body)
  dat$idReadable
}


clean_title <- function(x) {
  x <- trimws(x)
  if (is.na(x) || !nzchar(x)) {
    return("(description not given)")
  }
  if (grepl("\n", x, fixed = TRUE)) {
    x <- strsplit(x, "\n", fixed = TRUE)[[c(1, 1)]]
  }
  if (nchar(x) > 200) {
    x <- paste0(substr(x, 1, 200), "...")
  }
  x
}


find_project_id <- function(yt, name) {
  ids <- yt$GET("/admin/projects", query = list(fields = "id,shortName"))
  i <- which(tolower(vapply(ids, "[[", "", "shortName")) == tolower(name))
  stopifnot(length(i) == 1L)
  ids[[i]]$id
}


read_googlesheet_issues <- function(id) {
  tryCatch({
    googlesheets4::sheets_deauth()
    suppressMessages(googlesheets4::read_sheet(id))
  }, error = function(e) {
    gs2yt_log(sprintf("Failed to read issues: %s", e$message))
    NULL
  })
}


gs2yt_log <- function(text) {
  message(sprintf("[%s] %s", Sys.time(), text))
}


read_map <- function() {
  if (file.exists("map.csv")) {
    map <- read.csv("map.csv", stringsAsFactors = FALSE)
  } else {
    map <- data.frame(row = integer(0),
                      issue = character(0),
                      stringsAsFactors = FALSE)
  }
}


write_map <- function(map) {
  write.csv(map, "map.csv", row.names = FALSE)
}

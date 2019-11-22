service <- function(sheet_id, yt, poll = 10 * 60) {
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
    } else {
      gs2yt_log("(no new issues)")
    }
  }
}


main <- function() {
  sheet_id <- Sys.getenv("GS2YT_SHEET_ID")
  yt_token <- Sys.getenv("GS2YT_YT_TOKEN")
  yt <- YouTrackAPI$new("vimc", yt_token)
  service(sheet_id, yt)
}


prepare <- function() {
  cl <- vaultr::vault_client(login = "github")
  Sys.setenv(c(
    "GS2YT_SHEET_ID" = cl$read("/secret/hint/gs2yt/googlesheet")$id,
    "GS2YT_YT_TOKEN" = cl$read("/secret/hint/gs2yt/youtrack")$token))
}

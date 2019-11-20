## We can do a proper youtrack client later.
YouTrackAPI <- R6::R6Class(
  "YouTrackAPI",

  private = list(
    url = NULL,
    token = NULL,
    headers = NULL),

  public = list(
    initialize = function(name, token) {
      private$url <- sprintf("https://%s.myjetbrains.com/youtrack/api", name)
      private$token <- token
      private$headers <- httr::add_headers(
        "Authorization" = paste("Bearer", private$token),
        "Accept" = "application/json")
    },

    request = function(verb, path, ...) {
      res <- verb(paste0(private$url, path), private$headers, ...)
      httr::stop_for_status(res)
      jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"),
                         simplifyVector = FALSE)
    },

    GET = function(path) {
      self$request(httr::GET, path)
    },

    POST = function(path, body) {
      self$request(httr::POST, path, body = body, encode, encode = "form")
    }
  ))

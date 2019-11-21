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

    request = function(verb, path, ..., parse = TRUE) {
      res <- verb(paste0(private$url, path), private$headers, ...)
      if (res$status_code >= 300) {
        dat <- response_to_json(res)
        stop(sprintf("%s: %s (%d)",
                     dat$error, dat$error_description, res$status_code))
      }
      if (!parse) {
        return(res)
      }
      response_to_json(res)

    },

    GET = function(path, ...) {
      self$request(httr::GET, path, ...)
    },

    POST = function(path, body, ...) {
      self$request(httr::POST, path, body = body, encode = "json", ...)
    }
  ))



response_to_json <- function(res) {
  jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"),
                     simplifyVector = FALSE)
}


scalar <- function(x) {
  jsonlite::unbox(x)
}

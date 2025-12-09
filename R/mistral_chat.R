#' Wrapper function to chat with an LLM (here, mistral-tiny)
#'
#' @description
#' Sends a prompt to the Mistral API and returns the response.
#'
#' @param prompt The text prompt to send to the Mistral API.
#' @param model The model to use (default: "mistral-tiny").
#' @param temperature Controls randomness in the response (default: 0.7).
#' @param max_tokens Maximum number of tokens in the response (default: 100).
#' @param api_key Your Mistral API key (optional, can be set globally).
#'
#' @return The API response as a character string.
#' @export
#'
#' @examples
#' mistral_chat("How do you spell backward backward?")
#'
mistral_chat <- function(prompt,
                         model = "mistral-tiny",
                         temperature = 0.7,
                         max_tokens = 100,
                         api_key = NULL) {
  api_key <- api_key %||% Sys.getenv("MISTRAL_API_KEY")

  if (is.null(api_key) || api_key == "") {
    stop("API key not provided. Set it as an argument or as an environment
         variable 'MISTRAL_API_KEY'.")
  }

  url <- "https://api.mistral.ai/v1/chat/completions"
  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )

  body <- list(
    model = model,
    messages = list(list(role = "user", content = prompt)),
    temperature = temperature,
    max_tokens = max_tokens
  )

  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)

  response <- httr::POST(url, body = body_json, headers)

  if (httr::http_status(response)$category == "Success") {
    content <- httr::content(response, "text", encoding = "UTF-8")
    json_content <- jsonlite::fromJSON(content)
    if (!is.null(json_content$choices)) {
      return(json_content$choices$message$content)
    } else {
      stop("Unexpected API response: ", content)
    }
  } else {
    stop(paste("Error:", httr::http_status(response)))
  }
}

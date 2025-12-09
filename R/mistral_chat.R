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
         variable 'MISTRAL_API_KEY'.

To create a Mistral API key, follow these steps:

Log in to the Mistral Console: Go to https://console.mistral.ai/ and log in with
your account. If you don’t have an account, you’ll need to create one first.

Navigate to the API Keys Section: Once logged in, look for the ''API Key''
section in the left-hand menu or under your workspace settings. The direct link
is often https://console.mistral.ai/api-keys/ or similar.

Generate a New API Key: In the API Keys section, click on the button to''Create
new key'' or''Generate a new key.'' You may be asked to provide a name for the
key or set an expiration date.

Copy Your API Key: After generating the key, it will be displayed on the screen.
Copy it immediately—for security reasons, you won’t be able to see it again
after leaving the page. If you lose it, you’ll need to generate a new one.

Store Your API Key Securely: Save your API key in a secure place, such as an
environment variable or a secure password manager.

In R console: usethis::edit_r_environ()

Add this line in the file: MISTRAL_API_KEY=your_api_key_here

Save the file and restart your R session for the changes to take effect.

")
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

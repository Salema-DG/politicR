
#' @title Calls the ChatGPT API
#'
#' @description
#' This function calls the chatGPT API
#' with the given prompt and returns the answer.
#'
#' @param promt Text for chat GPT. A string.
#' @param api_key When you run the function above first time, it will ask you to enter your API Key. It will save the API Key in chatGPT_API_KEY environment variable so it won't ask for API Key when you run the function next time. Sys.setenv( ) is to store API Key whereas Sys.getenv( ) is to pull the stored API Key.
#' @param cat Logical value. Apply cat() to the output. TRUE is the default.
#' @param version What is the version of chatGPT. "gpt-3.5-turbo" is the default.
#' @param temperature Number from 0 to 2. Detault is 0. Used to control the creativity or randomness of the generated text. A higher temperature value will make the model more likely to generate more surprising and unexpected responses.
#' @param system_role Provide a string for the system role. Here you can say "act like the president" or "answer in a funny way".
#' @param full_response Default is false. If true, returns every parameter of the response.
#' @param stop_if_incomplete Logical value indication whether a response should be returned if not complete.
#'
#' @return The response of chat GPT.
#'
#' @source https://www.listendata.com/2023/05/chatgpt-in-r.html
#'
#' @export
#'
#' @examples
#' # You can set the API key when the function asks so. but this is how to do it in a script.
#' # Sys.setenv(chatGPT_API_KEY = "APIKey") # Set API Key
#' # Sys.getenv("chatGPT_API_KEY") # Get API Key
#'
#' # usage of the function:
#' # ask_chatgpt("Faz-me uma lista de frutas")
#' # ask_chatgpt("Faz-me uma lista de frutas", cat = F, temperature = 1.6)

ask_chatgpt <- function(prompt,
                        api_key = Sys.getenv("chatGPT_API_KEY"),
                        cat = TRUE,
                        version = "gpt-3.5-turbo",
                        temperature = 1,
                        system_role = NULL,
                        full_response = F,
                        stop_if_incomplete = F) {

  # For the API key
  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(chatGPT_API_KEY = api_key)
  }


  # rate limits:
  # https://platform.openai.com/docs/guides/rate-limits?context=tier-free

  stopifnot()

  if (is.null(system_role)) {
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(Authorization = paste("Bearer", api_key)),
      httr::content_type_json(),
      encode = "json",
      body = list(
        model = version,
        temperature = temperature,
        messages = list(list(
          role = "user",
          content = prompt
        ))
      )
    )
  } else{
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(Authorization = paste("Bearer", api_key)),
      httr::content_type_json(),
      encode = "json",
      body = list(
        model = version,
        temperature = temperature,
        messages = list(
          list(
            "role" = "system",
            "content" = system_role
          ),
          list(
            role = "user",
            content = prompt
        ))
      )
    )
  }

  finish <- httr::content(response)$choices[[1]]$finish_reason
  if (finish != "stop") { # stop means the API returned the full chat completion

    if (finish == "length") {
      cat("The maximum tokens of the conversation were exceeded. Showing only a partial answer.")
    }

  }

  stopifnot(stop_if_incomplete == F)

  if(httr::status_code(response)>200) {
    stop(httr::content(response))
  }

  if (full_response == T) {
    return(httr::content(response))
    stop()
  }

  response <- stringr::str_trim(httr::content(response)$choices[[1]]$message$content)

  if (cat == T) {
    return(cat(response))
  } else {
    return(response)
  }



}

























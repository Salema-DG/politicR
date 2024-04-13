
#' @title Calls the ChatGPT API
#'
#' @description
#' This function calls the chatGPT API
#' with the given prompt and returns the answer.
#' Taken from: https://www.r-bloggers.com/2023/03/call-chatgpt-or-really-any-other-api-from-r/
#'
#' @param promt Text for chat GPT. A string.
#' @param api_key A string. Secret key for chatGPT API account. By default, it's an object named key.
#' @param cat Logical value. Apply cat() to the output. TRUE is the default.
#' @param version What is the version of chatGPT. "gpt-3.5-turbo" is the default.
#'
#' @return The response of chat GPT.
#'
#' @examples
#' # key <- "sk-5-your-actual-api-key-Fvau6"
#' # ask_chatgpt("Faz-me uma lista de frutas")
#' # ask_chatgpt("Faz-me uma lista de frutas", cat = F)

ask_chatgpt <- function(prompt,
                        api_key = key,
                        cat = TRUE,
                        version = "gpt-3.5-turbo") {


  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = version,
      messages = list(list(
        role = "user",
        content = prompt
      ))
    )
  )

  response <- stringr::str_trim(httr::content(response)$choices[[1]]$message$content)

  if (cat == T) {
    return(cat(response))
  } else {
    return(response)
  }

}

globalVariables(c("key"))



























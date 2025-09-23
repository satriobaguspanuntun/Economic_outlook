library(ollamar)
library(httr2)
library(tidyverse)

test_connection()
list_models()

resp <- generate("gpt-oss:20b", "are you aware that you're being called from Rstudio? If so, if i want to pass vector or data to you how do i do it?", output = "text")
resp

# define a JSON schema as a list to constrain a model's output
format <- list(
  type = "object",
  properties = list(
    name = list(type = "string"),
    capital = list(type = "string"),
    languages = list(type = "array",
                     items = list(type = "string")
    )
  ),
  required = list("name", "capital", "languages")
)

generate("gpt-oss:20b", "tell me about Canada", output = "structured", format = format)

msg <- create_message("tell me about Canada")
chat("gpt-oss:20b", msg, format = format, output = "structured")


# text to classify
texts <- c('I love this product', 'I hate this product', 'I am neutral about this product')

# create system prompt
chat_history <- create_message("Your only task/role is to evaluate the sentiment of product reviews provided by the user. Your response should simply be 'positive', 'negative', or 'other'.", "system")

# create httr2_request objects for each text, using the same system prompt
reqs <- lapply(texts, function(text) {
  messages <- append_message(text, "user", chat_history)
  chat("gpt-oss:20b", messages, output = "req")
})

# make parallel requests and get response
resps <- req_perform_parallel(reqs)  # list of httr2_request objects

# process the responses
bind_rows(lapply(resps, resp_process, "df"))  # get responses as dataframes


messages <- create_message("Tell me a 1-paragraph story.")

# use "llama3.1" model, provide list of messages, return text/vector output, and stream the output
chat("gpt-oss:20b", messages, output = "text", stream = TRUE)
# chat(model = "llama3.1", messages = messages, output = "text", stream = TRUE)  # same as above

##### GOAL ######
# generate analysis and commentary for each economic themes
# 














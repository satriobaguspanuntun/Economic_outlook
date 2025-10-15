library(ollamar)
library(httr2)
library(tidyverse)
library(jsonlite)
library(glue)

test_connection()
list_models()

ollamar::pull("gpt-oss:20b-cloud")

resp <- generate("gemma", "are you aware that you're being called from Rstudio? If so, if i want to pass vector or data to you how do i do it?", output = "text")
resp

live_browser(chat = chat_ollama())

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


### commentary function 

# Real GDP analysis
prompt_gdp_analysis <- "You are a expert macroeconomist and you are tasked to
                        generate a report on US economy. The first thing you will need
                        to do is analysing US GDP and the GDP section is divided into
                        5 paragraph in total. Right now, you are in the first paragraph and
                        you required to explain why GDP analysis is important for this economic report.
                        generate paragraph consisting 100 maximum words. Please return only the paragpraph, 
                        no title and omit the first sentence like 'Okay, here's the first'."

resp_gdp_analysis <- generate("gemma3:1b", prompt_gdp_analysis, output = "text")


# create chat history
messages <- create_messages(
  create_message("end all your sentences with !!!", role = "system"),
  create_message("Hello!"),  # default role is user
  create_message("Hi, how can I help you?!!!", role = "assistant"),
  create_message("What is the capital of Australia?"),
  create_message("Canberra!!!", role = "assistant"),
  create_message("what is your name?")
)
cat(chat("gpt-oss:20b", messages, output = "text"))  # print the formatted output


prompt_first_par <- create_messages(
  create_message("You are an expert macroeconomist and you are tasked to create a
                 macroeconomic outlook report. Your answer should be in line with
                 the following rules:
                 
                 * Return only in paragraph.
                 * Be concise and coherent.
                 * Do not generate title.
                 * Omit the first sentence of the answer like 'Okay here is the analysis etc'.",
                 role = "system"),
  # first paragraph
  create_message("You are working on writing a macroeconomic outlook report for the USA.
                 Right now, you are in the GDP analysis section and this section will be
                 divided into 5 paragraph. For the first paragraph, write up an explanation
                 on why GDP analysis is important for 100 words. Also, no need for summarization on the last sentence
                 of the paragraph.")
)

chat("gpt-oss:20b", prompt_first_par, output = "text")


library(ellmer)

chat <- chat_ollama(model = "gpt-oss:120b-cloud", 
                    system_prompt = 
                 "You are an expert macroeconomist and you are tasked to create a
                 macroeconomic outlook report. Your answer should be in line with
                 the following rules:
                 
                 * Return only in paragraph.
                 * Be concise and coherent.
                 * Do not generate title.
                 * Return the answer in a nice markdown text, so that it can easily intergrated into Rmarkdown.
                 * Omit the first sentence of the answer like 'Okay here is the analysis etc'.")

first_prompt <- chat$chat("You are working on writing a macroeconomic outlook report for the USA.
                 Right now, you are in the GDP analysis section and this section will be
                 divided into 5 paragraph. For the first paragraph, write up an explanation
                 on why GDP analysis is important for 100 words. Also, no need for summarization on the last sentence
                 of the paragraph.")

second_prompt <- chat$chat(paste0("After that brief explanation on GDP analysis, you are asked to write up 
                                  2 paragraphs. For the first paragraph, briefly give an introduction on
                                  USA economy in the context of GDP. For example latest data movements and recent
                                  trends. As for the second paragraph, You will dig deeper into numbers
                                  showcasing movements and direction of the both Nominal and Real GDP growth rates.
                                  The first paragraph should consist at least 100 words whereas second paragraph cover
                                  at least 200 words. the dataset is provided to you in JSON format.
                                  Here is the JSON data:",  gdp_json_second_par))

third_prompt <- chat$chat("Now, you need to bridge previous analysis on Nominal and Real GDP growth rate to 
                          Real GDP components contribution towards Real GDP Growth Rate, 
                          which includes Consumption, Government spending, Investment, Net Export and Import, and
                          lastly the Residual. Write up a paragraph which consist of 70 words on why analysing 
                          contribution to real gdp growth from each components is important.")



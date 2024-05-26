
pacman::p_load(pagedown,
               htmltools,
               tidyverse,
               rvest,
               xml2,
               here,
               httr)

####################################################
## Functions - original
####################################################

price <- function(realtor, url, element){
  remDr$navigate(url)
  remDr$getPageSource(header = TRUE)[[1]] %>% 
    read_html() %>%
    html_elements(element) %>% # assumes element (not xpath)
    html_text() %>%
    str_extract('(\\$[0-9]+(.[0-9]+)?)') %>%
    .[which(!is.na(.))] %>%
    table() %>%
    as.data.frame(,stringAsFactors = TRUE) %>%
    rename(, "{{realtor}}" :="Freq")
}

address_price <- function(realtor, url, price_element, address_element){
  
  remDr$navigate(url = row$url)
  
  Sys.sleep(5)
  
  #address
  address <- remDr$getPageSource(header = TRUE)[[1]] %>% 
    read_html() %>%
    html_elements(row$element_address) %>%
    html_text() %>%
    #str_extract('(\\$[0-9]+(.[0-9]+)?)') %>%
    .[which(!is.na(.))] %>%
    as.data.frame(,stringAsFactors = TRUE) %>%
    rename("address" = ".") %>%
    mutate_all(~ gsub("\n", "", .)) %>%
    filter(str_detect(address, "^[0-9]")) %>% 
    mutate(index = row_number())
  
  #price
  price <- remDr$getPageSource(header = TRUE)[[1]] %>% 
    read_html() %>%
    html_elements(row$element_price) %>%
    html_text() %>%
    str_extract('(\\$[0-9]+(.[0-9]+)?)') %>%
    .[which(!is.na(.))] %>%
    as.data.frame(,stringAsFactors = TRUE) %>%
    rename("price" = ".") %>% 
    mutate(index = row_number())
  
  # Perform a full join on the 'index' column
  address_price <- full_join(address, price, by = "index") %>%
                    mutate(realtor = !!row$realtor) %>% 
                    select(-index) %>%
                    mutate(Timestamp = Sys.time())
  
  # # Remove the 'index' column if not needed
  # result <- result 
  # 
  # #merge
  # address_price <- bind_cols(address,price) 
  
}

#listings_checker <- function(df){
#                                if(nrow(df) <1){"FALSE"}
#                                else {"TRUE"}
#}

####################################################
## Functions - rvest
####################################################


address_price_rvest <- function(realtor, url, element_price, element_address){
  # Initialize address_price object
  address_price <- NULL
  
  # Error handling
  tryCatch({
    sess <- rvest::read_html_live(url)
    
    # Extract price data
    price <- sess %>% 
      html_elements(element_price) %>%
      xml_text(.) %>%
      map_chr(~as.character(as.numeric(gsub("[^0-9.]", "", .)))) %>%
      map(~as.numeric(.x)) %>%
      unlist(.) %>%
      .[which(!is.na(.))] %>%
      as.data.frame(., stringAsFactors = TRUE) %>%
      rename("price" = ".") %>% 
      mutate(index = row_number())
    
    if (nrow(price) == 0) {
      price <- tibble(price = numeric(), index = numeric())
    }
    
    # Extract address data
    address <- sess %>% 
      html_elements(element_address) %>%
      xml_text(.) %>%
      .[which(!is.na(.))] %>%
      as.data.frame(., stringAsFactors = TRUE) %>%
      rename("address" = ".") %>%
      mutate_all(~ gsub("\n", "", .)) %>%
      mutate(index = row_number())
    
    # Perform a full join on the 'index' column
    address_price <- full_join(address, price, by = "index") %>%
      mutate(realtor = realtor) %>% 
      select(-index) %>%
      mutate(Timestamp = Sys.time())
    
  }, error = function(e) {
    # Handle errors gracefully
    warning("An error occurred: ", conditionMessage(e))
  })
  return(address_price)
}


####################################################
## Functions - utilities
####################################################


download_webpage <- function(realtor, 
                             url) {
  # Read realtors metadata from csv file
  #meta <- read.csv(paste0(here::here(),"/realtors_meta.csv"))
  
  # URL of the webpage you want to download (using the first URL from the metadata)
  # url <- meta %>%
  #   filter(realtor == realtor_name,
  #          type == buying_leasing) |>
  #   pull(url)
  # #meta$url[1]
  
  # Create a sub-folder named by today's date
  today_date <- format(Sys.Date(), "%Y-%m-%d")
  sub_folder <- paste0(here::here(),"/page_dumps/",today_date)
  if (!dir.exists(sub_folder)) {
    dir.create(sub_folder)
  }
  
  # Define the file name with realtor's name and timestamp down to the minute
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  file_name <- paste0(realtor, "_", timestamp, ".html")
  
  # Define the file path where you want to save the downloaded HTML in the sub-folder
  file_path <- paste0(sub_folder, "/", file_name)
  
  # Use rvest to download the webpage
  webpage <- read_html(url)
  
  # Save the HTML content to a file
  write_html(webpage, file_path)
  
  # Read the contents of the HTML file into R
  html_content <- readLines(file_path, warn = FALSE)
  
  # Concatenate the lines into a single character string
  html_content <- paste(html_content, collapse = "\n")
  
  # Print the HTML content
  print(html_content)
}


####################################################
## Functions - ChatGPT
####################################################

# Calls the ChatGPT API with the given prompt and returns the answer
ask_chatgpt <- function(prompt) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", #"https://api.openai.com/v1/completions", 
    add_headers(Authorization = paste("Bearer", keyring::key_get("chatgpt"))),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user", 
        content = prompt
      ))
    )
  )
  str_trim(content(response)$choices[[1]]$message$content)
}

# name <- "Belle"
# file_location <- scrape_list[1]

extract_information <- function(file_location, prompt_precursor, name) {
  # Read the contents of the HTML file into R
  html_content <- readLines(file_location, warn = FALSE)
  
  # Concatenate the lines into a single character string
  html_content <- paste(html_content, collapse = "\n")
  
  # Generate timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  
  # Create a filename based on the argument name and timestamp
  name <- paste0(name, "_", timestamp)
  
  # Ask ChatGPT the specified prompt
  response <- ask_chatgpt(paste0(prompt_precursor, 
                                     html_content))
  
  # Write the HTML content to a file with the generated filename
  # writeLines(html_content, file.path(here(), paste0("output/", file_name, ".txt")))
  #assign(name, html_content, envir = .GlobalEnv)
  
  # Return the HTML content
  return(response)
}

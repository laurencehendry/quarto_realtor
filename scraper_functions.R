

library(pagedown)
library(htmltools)
library(tidyverse)
library(rvest)
library(xml2)



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

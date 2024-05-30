# Load the httr package
pacman::p_load(
  rio,
  here,
  httr,
  jsonlite,
  stringr,
  tidyverse)

# Function to get news data from a specific URL
get_who_news <- function(url) {
  # Make the GET request to the API
  response <- GET(url)
  
  # Check the status of the response
  if (status_code(response) == 200) {
    # Parse the JSON content
    content <- content(response, "text")
    data <- fromJSON(content)
    return(data)
  } else {
    # Return NULL if the request was unsuccessful
    return(NULL)
  }
}

# Initialize variables
base_url <- "https://www.who.int/api/news/diseaseoutbreaknews"
all_news <- list()
next_url <- base_url
keep_fetching <- TRUE

# Loop to fetch all pages
while (keep_fetching) {
  data <- get_who_news(next_url)
  
  if (!is.null(data) && "value" %in% names(data)) {
    all_news <- c(all_news, data$value)
    
    # Check if there is a next page link
    if ("@odata.nextLink" %in% names(data)) {
      next_url <- data[["@odata.nextLink"]]
    } else {
      keep_fetching <- FALSE
    }
  } else {
    keep_fetching <- FALSE
  }
}

# Define a function to convert the nested list to a data frame
convert_to_df <- function(news_list) {
  # Initialize an empty list to hold data frames
  df_list <- list()
  
  # Determine the segment length
  segment_length <- 22
  
  # Iterate through the list in steps of segment_length
  for (i in seq(1, length(news_list), by = segment_length)) {
    # Extract the current segment
    segment <- news_list[i:(i + segment_length - 1)]
    
    # Convert the segment to a data frame and add it to the list
    df_list[[length(df_list) + 1]] <- as.data.frame(segment)
  }
  
  # Combine all data frames into one
  combined_df <- do.call(rbind, df_list)
  return(combined_df)
}

# Apply the function to all_news
all_news_df <- convert_to_df(all_news)

# Function to remove HTML tags
remove_html_tags <- function(text) {
  return(str_replace_all(text, "<[^>]*>", ""))
}

all_news_df %>% 
  mutate(across(where(is.character), remove_html_tags)) %>%
  arrange(desc(PublicationDate)) %>%
  rio::export("who_dons.csv")



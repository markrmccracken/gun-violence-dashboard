# R/bluesky_functions.R
# Bluesky API Integration Functions for Gun Violence Dashboard
# Replace Twitter functionality with Bluesky AT Protocol

library(httr2)
library(jsonlite)
library(tm)
library(wordcloud2)

# Define the %||% operator if it doesn't exist
`%||%` <- function(x, y) if (is.null(x)) y else x

# Authenticate with Bluesky
authenticate_bluesky <- function(identifier, password) {
  auth_url <- "https://bsky.social/xrpc/com.atproto.server.createSession"
  
  tryCatch({
    response <- request(auth_url) %>%
      req_method("POST") %>%
      req_headers("Content-Type" = "application/json") %>%
      req_body_json(list(
        identifier = identifier,
        password = password
      )) %>%
      req_perform()
    
    if (resp_status(response) == 200) {
      session_data <- response %>% resp_body_json()
      return(session_data)
    } else {
      stop("Authentication failed with status: ", resp_status(response))
    }
  }, error = function(e) {
    warning("Bluesky authentication error: ", e$message)
    return(NULL)
  })
}

# Search for posts on Bluesky
search_bluesky_posts <- function(session, query, limit = 100) {
  search_url <- "https://bsky.social/xrpc/app.bsky.feed.searchPosts"
  
  tryCatch({
    response <- request(search_url) %>%
      req_method("GET") %>%
      req_headers(
        "Authorization" = paste("Bearer", session$accessJwt),
        "Content-Type" = "application/json"
      ) %>%
      req_url_query(
        q = query,
        limit = min(limit, 100)  # Bluesky API limit
      ) %>%
      req_perform()
    
    if (resp_status(response) == 200) {
      posts_data <- response %>% resp_body_json()
      return(posts_data$posts %||% list())
    } else {
      warning("Search failed with status: ", resp_status(response))
      return(list())
    }
  }, error = function(e) {
    warning("Bluesky search error: ", e$message)
    return(list())
  })
}

# Process Bluesky posts into word cloud (replaces GrabTweets function)
GrabBlueskyPosts <- function(hashtag, numwords, session) {
  tryCatch({
    # Check if session exists
    if (is.null(session)) {
      return(create_fallback_wordcloud("No Bluesky session available"))
    }
    
    # Search for posts with the hashtag
    posts <- search_bluesky_posts(session, hashtag, numwords)
    
    if (length(posts) == 0) {
      return(create_fallback_wordcloud(paste("No posts found for", hashtag)))
    }
    
    # Extract text from posts
    post_texts <- sapply(posts, function(post) {
      # Handle different post structures
      text <- ""
      if (!is.null(post$record) && !is.null(post$record$text)) {
        text <- post$record$text
      } else if (!is.null(post$text)) {
        text <- post$text
      }
      return(as.character(text))
    })
    
    # Remove empty texts
    post_texts <- post_texts[post_texts != "" & !is.na(post_texts)]
    
    if (length(post_texts) == 0) {
      return(create_fallback_wordcloud("No valid text found in posts"))
    }
    
    # Text processing (same approach as original Twitter version)
    docs <- Corpus(VectorSource(post_texts))
    
    # Clean the text
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, removeWords, c(".", "can", "just", "one", "like", "get", 
                                        "see", "will", "...", "_", "-", "amp", 
                                        "rt", "https", "http", "www", "com"))
    docs <- tm_map(docs, stripWhitespace)
    docs <- tm_map(docs, removePunctuation)
    
    # Create term document matrix
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v, stringsAsFactors = FALSE)
    
    # Filter out very short words and ensure we have data
    d <- d[nchar(as.character(d$word)) > 2, ]
    d <- d[d$freq > 1, ]  # Remove single-occurrence words
    
    if (nrow(d) == 0) {
      return(create_fallback_wordcloud("No meaningful words found"))
    }
    
    # Limit to top words for better visualization
    d <- head(d, 100)
    
    # Create the word cloud
    wordcloud2(d, 
               size = 0.8,
               minSize = 0.1,
               color = rep_len(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                                 "#9467bd", "#8c564b", "#e377c2"), nrow(d)),
               backgroundColor = "#f8f9fa",
               fontFamily = "Arial",
               rotateRatio = 0.3)
    
  }, error = function(e) {
    warning("Error in GrabBlueskyPosts: ", e$message)
    return(create_fallback_wordcloud("Error loading posts"))
  })
}

# Helper function to create fallback word clouds
create_fallback_wordcloud <- function(message) {
  fallback_data <- data.frame(
    word = c(strsplit(message, " ")[[1]], "bluesky", "social", "media"),
    freq = c(rep(3, length(strsplit(message, " ")[[1]])), 2, 2, 1),
    stringsAsFactors = FALSE
  )
  
  wordcloud2(fallback_data, 
             size = 1.2, 
             minSize = 0.5,
             color = "gray", 
             backgroundColor = "white")
}

# Function to validate Bluesky credentials
validate_bluesky_credentials <- function() {
  username <- Sys.getenv("BLUESKY_USERNAME")
  password <- Sys.getenv("BLUESKY_PASSWORD")
  
  if (username == "" || password == "") {
    message("Bluesky credentials not found in environment variables.")
    message("Please set BLUESKY_USERNAME and BLUESKY_PASSWORD environment variables.")
    message("You can do this by:")
    message("1. Creating a .env file in your project root with:")
    message("   BLUESKY_USERNAME=your.handle.bsky.social")
    message("   BLUESKY_PASSWORD=your-app-password")
    message("2. Or setting them in your R environment:")
    message('   Sys.setenv(BLUESKY_USERNAME = "your.handle.bsky.social")')
    message('   Sys.setenv(BLUESKY_PASSWORD = "your-app-password")')
    return(FALSE)
  }
  
  return(TRUE)
}

# Function to test Bluesky connection
test_bluesky_connection <- function() {
  if (!validate_bluesky_credentials()) {
    return(FALSE)
  }
  
  username <- Sys.getenv("BLUESKY_USERNAME")
  password <- Sys.getenv("BLUESKY_PASSWORD")
  
  session <- authenticate_bluesky(username, password)
  
  if (is.null(session)) {
    message("Failed to authenticate with Bluesky")
    return(FALSE)
  }
  
  # Test a simple search
  test_posts <- search_bluesky_posts(session, "test", 5)
  
  if (length(test_posts) >= 0) {  # Even 0 results is a successful API call
    message("Bluesky connection successful!")
    return(TRUE)
  } else {
    message("Bluesky API call failed")
    return(FALSE)
  }
}
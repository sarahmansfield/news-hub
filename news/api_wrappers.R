get_top_headlines <- function(country = "", category = "", sources = "", 
                              q = "", pageSize = 0, page = 0, apiKey = "") {
  # check for invalid request parameters
  if (apiKey == "") {
    stop("An API key is required")
  }
  if (country == "") {
    if (all(list(category, sources, q) == "")) {
      stop("If country is not specified, at least one other categorical
           parameter must be specified")
    }
  }
  if (!all(sapply(list(country, category, sources, q, apiKey), 
                 is.character))) {
    stop("country, category, sources, q, and apiKey must be character values")
  }
  if (pageSize %% 1 != 0 | page %% 1 != 0) {
    stop("pageSize and page must be integer values")
  }
  if (country != "" & sources != "") {
    stop("You can't mix the country and sources parameters")
  }
  if (category != "" & sources != "") {
    stop("You can't mix the category and sources parameters")
  }
  if (pageSize < 0 | page < 0) {
    stop("page and pageSize cannot be negative values")
  }
  if (pageSize > 100) {
    stop("Page size must not exceed 100")
  }
  
  base_url <- "https://newsapi.org/v2/top-headlines"
  param <- c("country=", "category=", "sources=", "q=", "pageSize=", 
             "page=", "apiKey=")
  vals <- c(country, category, sources, q, pageSize, page, apiKey)
  query <- "?"
  
  #build query string
  for (i in seq_along(param)) {
    # check for nonempty parameters
    if (!(param[i] %in% c("pageSize=", "page=")) & vals[i] != "" |
        (param[i] %in% c("pageSize=", "page=")) & vals[i] > 0) {
      # check if current query string already has parameters
      if (nchar(query) > 1) {
        addQuery <- str_c("&", param[i], vals[i])
      } else {
        addQuery <- str_c(param[i], vals[i])
      }
      query <- str_c(query, addQuery)
    }
  }
  query <- URLencode(query)

  fromJSON(str_c(base_url, query)) %>%
    as_tibble() %>%
    flatten() %>%
    as_tibble() %>%
    dplyr::rename(author = articles.author,
           title = articles.title,
           description = articles.description,
           url = articles.url,
           image = articles.urlToImage,
           publishDate = articles.publishedAt,
           content = articles.content,
           sourceID = articles.source.id,
           sourceName = articles.source.name) %>%
    mutate(publishDate = ymd_hms(publishDate))
}


get_sources <- function(category = "", country = "", apiKey = "") {
  # check for invalid request parameters
  if (apiKey == "") {
    stop("An API key is required")
  }
  if (!all(sapply(list(category, country, apiKey), is.character))) {
    stop("category, country, and apiKey must be character values")
  }
  
  base_url <- "https://newsapi.org/v2/sources"
  param <- c("category=", "country=", "apiKey=")
  vals <- c(category, country, apiKey)
  query <- "?language=en"
  
  #build query string
  for (i in seq_along(param)) {
    # check for nonempty parameters
    if (vals[i] != "") {
      addQuery <- str_c("&", param[i], vals[i])
      query <- str_c(query, addQuery)
    }
  }

  fromJSON(str_c(base_url, query)) %>% 
    as_tibble() %>%
    flatten() %>%
    as_tibble() %>%
    dplyr::rename(sourceID = sources.id,
           sourceName = sources.name,
           description = sources.description,
           url = sources.url,
           category = sources.category,
           language = sources.language,
           country = sources.country)
}


get_sentim <- function(x) {
  # make POST request and save response
  r <- POST(
    url    = "https://sentim-api.herokuapp.com/api/v1/",
    config = add_headers("Accept"       = "application/json",
                         "Content-Type" = "application/json"),
    body   = list(text = x),
    encode = "json")
  
  # extract content from POST response
  content <- content(r, "text", encoding = "UTF-8")
  
  fromJSON(content) %>%
    flatten_dfr() %>% 
    flatten() %>% 
    as_tibble() %>%
    dplyr::rename(overallPolarity = polarity,
           overallType = type,
           sentencePolarity = sentiment.polarity,
           sentenceType = sentiment.type)
}
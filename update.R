source("init.R")

if (!dir.exists('data')) system('mkdir -p data')
setCacheDir('data')
cacheFile <- 'data/rstats_tweets.rds'

# This should go in ~/.Renviron but...
# .TWITTER_APP="rtweet_things"
# .TWITTER_CONSUMER_KEY="xxx"
# .TWITTER_CONSUMER_SECRET="xxx"
# .TWITTER_PAT="./rtweet.rds"

# Twitter token created with...
# twitter_token <- create_token(
#   app = .TWITTER_APP,
#   consumer_key = .TWITTER_CONSUMER_KEY,
#   consumer_secret = .TWITTER_CONSUMER_SECRET
# )
# saveRDS(twitter_token, .TWITTER_PAT)
if (TWEET_REFRESH_ENABLED) {
  if (file.exists('twitter_secrets.R')) source('twitter_secrets.R')
  if (!exists('.TWITTER_PAT')) {
    .TWITTER_PAT <- Sys.getenv('TWITTER_PAT')
    if (.TWITTER_PAT == "") .TWITTER_PAT <- './rtweet.rds'
  }
  twitter_token <- readRDS(.TWITTER_PAT)
}

get_new_tweets <- function(max_id) {
  tip_words <- "(TIL|DYK|[Tt]ip|[Ll]earned|[Uu]seful|[Kk]now|[Tt]rick)"
  session_words <- "([Aa]vailable|[Oo]nline|[Ll]ink|[Ss]lide|[Ss]ession)"
  rstats_search <- c("#rstats")
  rstats_search <- paste(rstats_search, collapse = " OR ")
  
  rstats_tweets <- search_tweets(q = rstats_search, token = twitter_token, n = 1e5, max_id = max_id, include_rts = FALSE)
  rstats_tweets %>% 
    mutate(
      relates_tip = str_detect(text, tip_words),
      relates_session = str_detect(text, session_words),
      relates_github = str_detect(text, "[Gg]it[Hh]ub") | str_detect(urls_expanded_url, 'github.com'),
      hashtags = map(hashtags, tolower)
    )
}

needs_pulled <- FALSE
if (file.exists(cacheFile)) {
  cacheTime = file.info(cacheFile)$mtime
  cacheAge = difftime(Sys.time(), cacheTime, units="min")
  initRAge = difftime(Sys.time(), file.info('init.R')$mtime, units = 'min')
  needs_pulled <- (as.numeric(cacheAge) > 15 | as.numeric(initRAge) < as.numeric(cacheAge)) && TWEET_REFRESH_ENABLED
  rstats_tweets <- readRDS(cacheFile)
} else {
  rstats_tweets <- NULL
  cacheAge <- 0
  needs_pulled <- TRUE
}

if (needs_pulled) {
  max_id <- if (as.numeric(cacheAge) < 60) rstats_tweets$status_id
  if (!is.null(max_id)) {
    max_id <- max(max_id)
    message("Getting just new tweets, starting with ", max_id)
  }
  max_id <- NULL
  new_tweets <- get_new_tweets(max_id)
  if (!is.null(rstats_tweets)) {
    rstats_tweets <- bind_rows(
      semi_join(new_tweets, rstats_tweets, by = 'status_id'), # updates old tweets
      anti_join(rstats_tweets, new_tweets, by = 'status_id'), # keeps old tweets
      anti_join(new_tweets, rstats_tweets, by = 'status_id')  # adds new tweets
    ) %>% 
      arrange(desc(created_at))
  } else {
    rstats_tweets <- arrange(new_tweets, desc(created_at))
  }
  saveRDS(rstats_tweets, "data/rstats_tweets.rds")
  cacheTime <- Sys.time()
}

simpleCache('top_hashtags', {
  rstats_tweets %>% 
    filter(!is.na(hashtags)) %>% 
    pull(hashtags) %>% 
    unlist %>% 
    data_frame(`Top Hashtags` = .) %>% 
    filter(!`Top Hashtags` %in% c('rstats')) %>%
    group_by(`Top Hashtags`) %>% 
    tally(sort = TRUE) %>% 
    top_n(25, n) %>% select(-n)
}, recreate = needs_pulled)

simpleCache('related_hashtags', {
  rstats_tweets %>% 
    filter(map_lgl(hashtags, function(hl) length(intersect(hl, top_hashtags$`Top Hashtags`)) > 0)) %>% 
    pull(hashtags) %>% 
    map_dfr(function(hs) {
      x <- map(seq_along(hs), function(i) c(paste(hs[i], hs[i:length(hs)]), paste(hs[i:length(hs)], hs[i])))
      data_frame(tags = unlist(x))
    }) %>% 
    arrange(tags) %>% 
    filter(!duplicated(tags)) %>% 
    mutate(
      tags = str_split(tags, ' '),
      tag = map_chr(tags, ~ .[1]),
      related = map_chr(tags, ~ .[2])
    ) %>% 
    select(-tags) %>% 
    filter(tag != related, 
           tag %in% top_hashtags$`Top Hashtags`,
           related %in% top_hashtags$`Top Hashtags`)
}, recreate = needs_pulled)

# simpleCache('users_there_IRL', {
#   # Get twitter people who are there from https://twitter.com/dataandme/lists/rstudioconf18
#   lists_members(908723077084827648, token = twitter_token) %>% pull(user_id)
# }, recreate = needs_pulled)

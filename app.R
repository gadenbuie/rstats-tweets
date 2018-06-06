library(shiny)
library(rtweet)
library(dplyr)
library(stringr)
library(purrr)
requireNamespace('httr', quietly = TRUE)
requireNamespace('shinythemes', quietly = TRUE)
requireNamespace('DT', quietly = TRUE)
library(glue)
source("init.R")
setCacheDir('data')
simpleCache("rstats_tweets", source("update.R"))
simpleCache("top_hashtags")
simpleCache("related_hashtags")

get_tweet_blockquote <- function(screen_name, status_id) {
  bq <- httr::GET(glue("https://publish.twitter.com/oembed?url=https://twitter.com/{screen_name}/status/{status_id}?omit_script=true"))
  if (bq$status_code >= 400)
    '<blockquote style="font-size: 90%">Sorry, unable to get tweet ¯\\_(ツ)_/¯</blockquote>'
  else {
    httr::parsed_content(bq)$html
  }
}

close_to_lat_lng <- function(lat, lng, this_place) {
  this_place <- rtweet::lookup_coords(this_place)$point %>% as_radian
  # from http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates
  
  delta_lat <- 50/3958.76
  delta_lng <- asin(sin(as_radian(lat))/cos(delta_lat))
  lat <- as_radian(lat)
  lng <- as_radian(lng)
  lat >= this_place['lat'] - delta_lat &
    lat <= this_place['lat'] + delta_lat &
    lng >= this_place['lng'] - delta_lng & 
    lng <= this_place['lng'] + delta_lng
}

as_radian <- function(degree) degree * pi / 180

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src="twitter.js")),
  titlePanel("rstats tweets"),
  theme = shinythemes::shinytheme('yeti'),
  
  
  column(
    width = 4,
    wellPanel(
      selectInput('view', 'Tweet Group', c('Popular', 'Tips', "Talks", "Pictures", "All")),
      uiOutput('help_text'),
      uiOutput('filters')
    ),
    wellPanel(class = 'tweetbox', htmlOutput('tweet')),
    tags$div(class = 'colophon', 
             tagList(
               tags$p(
                 "Made with", HTML("&#x2764;&#xFE0F;"), "+",  HTML("\u2615\uFE0F"), "by", 
                 tags$a(href = 'https://twitter.com/grrrck/', '@grrrck'),
                 'with',  HTML("&#x1F4AA;"), 'from',
                 HTML(paste(
                   tags$a(href = 'http://rtweet.info/', 'rtweet'),
                   tags$a(href = 'https://www.rstudio.com/', 'RStudio'),
                   tags$a(href = 'https://shiny.rstudio.com/', 'Shiny'),
                   tags$a(href = 'https://www.tidyverse.org/', 'tidyverse'),
                   tags$a(href = 'http://code.databio.org/simpleCache/', 'simpleCache'),
                   sep = ', '
                 ))),
               tags$p(
                 HTML("&#x1F4BE;"), tags$a(href = GITHUB_REPO_URL, 'View on GitHub')
                 , "or", downloadLink('download_tweets', "Download Tweets")
               ),
               tags$p(
                 "Updated:", strftime(file.info('data/rstats_tweets.rds')$mtime, "%F %T %Z", tz = 'America/New_York')
               )
             )
    )
  ),
  
  column(8, DT::dataTableOutput('tweets'))
)

server <- function(input, output, session) {
  output$help_text <- renderUI({
    req(input$view)
    switch(
      input$view,
      'Popular' = helpText(HTML("&#x1F4AF;"),  "Most popular (retweets + favs) first"),
      'Tips' = helpText(HTML("&#x1F4A1;"), "Original or quote tweets that mention a tip"),
      'Talks' = helpText(HTML("&#x1F393;"),  "Original or quote tweets that mention \"slides\", \"presentations\", etc."),
      'Pictures' = helpText(HTML("&#x1F4F8;"),  "Tweets that come with a picture"),
      'All' = helpText(HTML("&#x1F917;"), "All the tweets"),
      NULL
    )
  })
  
  rstats_tweets <- reactiveFileReader(300000, session, "data/rstats_tweets.rds", readRDS)
  
  tweets <- reactive({
    x <- switch(
      input$view,
      'Popular' = rstats_tweets() %>% 
        arrange(desc(retweet_count + favorite_count), 
                -map_int(mentions_screen_name, length)),
      'Tips' = rstats_tweets() %>% filter(relates_tip, !is_retweet),
      'Talks' = rstats_tweets() %>% filter(relates_session, !is_retweet),
      'Pictures' = rstats_tweets() %>% filter(!is_retweet, !is.na(media_url)),
     rstats_tweets()
    ) 
    
    if (input$view %in% c('All', 'Popular')) {
      if (length(input$filter_binary)) {
        for (filter_binary in input$filter_binary) {
          x <- switch(
            filter_binary,
            'Not Retweet' = filter(x, !is_retweet),
            'Not Quote' = filter(x, !is_quote),
            'Has Media' = filter(x, !is.na(media_url)),
            'Has Link' = filter(x, !is.na(urls_url)),
            'Has Github Link' = filter(x, str_detect(urls_url, "github")),
            "Retweeted" = filter(x, retweet_count > 0),
            "Favorited" = filter(x, favorite_count > 0)
            # "Most Popular Users" = x %>% lat_lng() %>% 
            #   filter( 
            #     str_detect(tolower(place_full_name), "portland|oregon") | 
            #       close_to_lat_lng(lat, lng, "Portland, OR")
            #   )
          )
        }
      }
      if (length(input$filter_hashtag)) {
        x <- filter(x, !is.null(hashtags))
        for (filter_hashtag in input$filter_hashtag) {
          x <- filter(x, map_lgl(hashtags, function(h) filter_hashtag %in% h))
        }
      }
    }
    x
  })
  
  hashtags_related <- reactive({
    req(input$view %in% c('All', 'Popular'))
    if (is.null(input$filter_hashtag) || input$filter_hashtag == '') return(top_hashtags)
    limit_to_tags <- related_hashtags %>% 
      filter(tag %in% input$filter_hashtag) %>% 
      pull(related) %>% 
      unique()
    top_hashtags %>% 
      filter(`Top Hashtags` %in% c(limit_to_tags, input$filter_hashtag)) %>% 
      pull(`Top Hashtags`)
  })
  
  output$filters <- renderUI({
    selected_hashtags <- isolate(input$filter_hashtag)
    selected_binary <- isolate(input$filter_binary)
    if (input$view %in% c('All', 'Popular')) {
      tagList(
        checkboxGroupInput('filter_binary', 'Tweet Filters', 
                           choices = c("Not Retweet", "Not Quote", "Has Media", "Has Link", "Has Github Link", "Retweeted", "Favorited"), 
                           selected = selected_binary,
                           inline = TRUE),
        selectizeInput('filter_hashtag', 'Hashtags', choices = c("", hashtags_related()), selected = selected_hashtags, 
                       multiple = TRUE, options = list(plugins = list('remove_button')), width = "100%")
      )
    }
  })
  
  output$tweets <- DT::renderDataTable({
    tweets() %>% 
      select(created_at, screen_name, text, retweet_count, favorite_count, mentions_screen_name, urls_expanded_url) %>% 
      mutate(created_at = strftime(created_at, '%F %T', tz = 'America/New_York'),
             mentions_screen_name = map_chr(mentions_screen_name, paste, collapse = ', '),
             mentions_screen_name = ifelse(mentions_screen_name == 'NA', '', mentions_screen_name),
             urls_expanded_url = map_chr(urls_expanded_url, ~ paste(glue::glue('<a href="{.}" target="_blank">{.}</a>'), collapse = ", ")))
  },
  selection = 'single', 
  rownames = FALSE, 
  colnames = c("Timestamp", "User", "Tweet", "RT", "Fav", "Mentioned", "URL"), 
  escape = FALSE,
  filter = 'top',
  options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5)
  )
  
  output$tweet <- renderText({
    if (!is.null(input$tweets_rows_selected)) {
      tweets() %>% 
        slice(input$tweets_rows_selected) %>% 
        mutate(
          html = suppressWarnings(get_tweet_blockquote(screen_name, status_id))
        ) %>% 
        pull(html)
    } else {
      HTML('<blockquote style="font-size: 90%">Choose a tweet from the table...</blockquote>')
    }
  })
  
  output$download_tweets <-  downloadHandler(
    filename = function() {
      paste("rstats_tweets-", Sys.Date(), ".RDS", sep="")
    },
    content = function(file) {
      saveRDS(rstats_tweets(), file)
    }
  )
}

shinyApp(ui = ui, server = server)


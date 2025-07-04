#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(stringr)
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)
library(tidygraph)
library(ggraph)
library(tidyr)


influences <- read_rds("data/influences_by_type.rds")

influence_counts <- influences %>%
  count(`Edge Type`)

weighted_df <- read_rds("data/weighted_top_by_year_auto.rds")
collabs_tbl <- read_rds("data/sailor_collaborations_named.rds")
edges2 <- read_rds("data/edges2.rds")
nodes_tbl <- readRDS("data/nodes_tbl.rds")
edges2 %>% filter(rel_type %in% influence_types)
top_influencers_named <- readRDS("data/top_influencers_named.rds")
  
influence_types <- c("InStyleOf", "CoverOf", "DirectlySamples", 
                     "InterpolatesFrom", "LyricalReferenceTo")

songs_in_range <- nodes_tbl %>%
  filter(`Node Type` == "Song", !is.na(release_date),
         str_detect(release_date, "^\\d{4}")) %>%
  mutate(year = as.integer(substr(release_date,1,4))) %>%
  filter(year >= 2022, year <= 2035)
available_genres <- union(
  unique(songs_in_range$genre),
  edges2 %>% filter(rel_type %in% influence_types) %>%   
    mutate(song_id = if_else(rel_type == "LyricistOf", target, source)) %>%
    inner_join(songs_in_range, by = c("song_id"="id")) %>%
    pull(genre) %>% unique()
)

valid_genres <- nodes_tbl %>%
  filter(`Node Type` == "Song", !is.na(genre)) %>%
  inner_join(edges2 %>% filter(rel_type %in% influence_types), by = c("id" = "target")) %>%
  distinct(genre) %>%
  pull(genre) %>%
  sort()

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Sailor Shift Journey"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Overview",               tabName = "overview",     icon = icon("dashboard")),
      menuItem("Top Influencers",        tabName = "topinfluencers",icon = icon("star")),
      menuItem("Genre Influence Network",tabName = "genrenetwork", icon = icon("project-diagram")),
      menuItem("Genre Trend Analysis",   tabName = "genretrend",   icon = icon("chart-line")),
      menuItem("Raw Song Data",          tabName = "raw",          icon = icon("table")),
      menuItem("Collaborators",          tabName = "collaborators",icon = icon("users")),
      menuItem("Influence Graph",        tabName = "graph",        icon = icon("project-diagram")),
      menuItem("Top Artist Rankings",    tabName = "topartists",   icon = icon("chart-bar")),
      menuItem("Popularity Index Change",tabName = "popindex",     icon = icon("exchange-alt")),
      menuItem("Predicted Future Stars", tabName = "futurestars",  icon = icon("star"))
      
    ),
    
    ## Overview tab
    conditionalPanel(
      condition = "input.sidebar_tabs == 'overview'",
      selectInput("edgeType", "Filter by Influence Type:",
                  choices = c("All", unique(influences$`Edge Type`)),
                  selected = "All"),
      sliderInput("year_range", "Year Range:",
                  min   = min(weighted_df$release_year),
                  max   = max(weighted_df$release_year),
                  value = range(weighted_df$release_year),
                  sep   = ""),
      
      sliderInput("min_inf_count", "Min. Influence-links to show:",
                  min   = 0,
                  max   = max(influence_counts$n),
                  value = 0,
                  step  = 1)
      
    ),
    
    ## Top Influencers tab
    conditionalPanel(
      condition = "input.sidebar_tabs == 'topinfluencers'",
      sliderInput("min_links", "Minimum Influence Links:",
                  min   = 0,
                  max   = max(top_influencers_named$n),
                  value = 0,
                  step  = 1)
    ),
    
    ## Popularity Metrics tab
    conditionalPanel(
      condition = "input.sidebar_tabs == 'popmetrics'",
      # reuse the exact same inputId `year_range` here
      sliderInput("year_range", "Year Range:",
                  min   = min(weighted_df$release_year),
                  max   = max(weighted_df$release_year),
                  value = range(weighted_df$release_year),
                  sep   = "")
    ),
    
    ## Genre Influence Network tab
    conditionalPanel(
      condition = "input.sidebar_tabs == 'genrenetwork'",
      selectInput("selected_genre", "Select Central Genre:",
                  choices = valid_genres,
                  selected = valid_genres[1])
    ),
    
    ## Genre Trend Analysis tab
    conditionalPanel(
      condition = "input.sidebar_tabs == 'genretrend'",
      selectInput("trend_genre", "Select Genre of Music:",
                  choices = available_genres,
                  selected = available_genres[1]),
      sliderInput("trend_years", "Select Year Range:",
                  min   = 2022,
                  max   = 2035,
                  value = c(2025, 2035),
                  step  = 1,
                  sep   = "")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("vb_total_links",       width = 3),
          valueBoxOutput("vb_top_type",          width = 3),
          valueBoxOutput("vb_years_covered",     width = 3),
          valueBoxOutput("vb_unique_performers", width = 3)
        ),
        fluidRow(
          box(
            title = "How Sailor Shift Was Influenced",
            status = "primary", solidHeader = TRUE,
            width = 6,
            plotOutput("influencePlot", height = "300px")
          ),
          box(
            title = "Most Influential Performer by Release Year",
            status = "primary", solidHeader = TRUE,
            width = 6,
            plotOutput("topByYearPlot", height = "300px")
          )
        ),
        fluidRow(
          box(
            title = "Genre Trend Analysis",
            status = "primary", solidHeader = TRUE,
            width = 6,
            selectInput(
              "overview_trend_genre", "Genre:",
              choices  = available_genres,
              selected = available_genres[1]
            ),  # <-- close selectInput here
            plotOutput("overview_trendPlot", height = "300px")
          ),
          box(
            title = "Direct or Indirect Influence",
            status = "primary", solidHeader = TRUE,
            width = 6,
            dataTableOutput("overview_influence_table")
          )
        ),
        fluidRow(
          box(
            title = "Popularity Metrics Over Time by Artist",
            status = "primary", solidHeader = TRUE,
            width = 12,
            plotOutput("popularity_metrics_plot", height = "350px")
          )
        )
      ),
      
      tabItem(tabName = "topinfluencers",
              h3("Top Influencers of All Time"),
              box(plotlyOutput("top_influencers_plot", height = "450px"), width = 12)
      ),
      
      tabItem(tabName = "raw",
              h3("Sailor Shift's Raw Song Data"),
              dataTableOutput("song_table")
      ),
      
      tabItem(tabName = "collaborators",
              h3("Collaborations Data Table"),
              box(dataTableOutput("collab_table"), width = 12),
              
              h3("Direct or Indirect Influence Table"),
              box(dataTableOutput("influence_table"), width = 12)
      ),
      
      tabItem(tabName = "graph",
              h3("Sailor Shift’s Influence on Collaborators and Songs"),
              downloadButton("download_graph", "Download Graph (PNG)"),
              br(), br(),
              tags$img(src = "igraph.png", width = "100%", style = "max-height: 800px; object-fit: contain;"),
              tags$div(style = "margin-top:1em; padding:0.75em; border:1px solid #ccc; background:#fafafa;",
                       "Sailor Shift’s artistic influence extended well beyond her direct circle of collaborators.",
                       "Her music indirectly shaped the work of several artists in the Oceanus Folk scene, highlighting her central role as both a founder",
                       " and a source of inspiration for the genre.")
      ),
      
      tabItem(tabName = "topartists",
              h3("Top 10 Artists by Overall Popularity & Influence"),
              fluidRow(
                # Left: Main plot
                box(
                  plotOutput("top_artists_plot", height = "400px"),
                  width = 8
                ),
                # Right: Filters box
                box(
                  title = "Filters", status = "primary", solidHeader = TRUE,
                  selectInput("top_n", "Show Top:", choices = c(5, 10, 20), selected = 10),
                  sliderInput("min_score", "Minimum Composite Score:",
                              min = 0, max = 20, value = 0, step = 0.5),
                  width = 4
                )
              )
      ),
      
      tabItem(tabName = "popindex",
              h3("Popularity Index Change (2025 → 2039)"),
              fluidRow(
                box(width = 12,
                    plotOutput("pop_index_plot", height = "500px"),
                    tags$div(style = "margin-top:1em; padding:0.75em; border:1px solid #ccc; background:#fafafa;",
                    "Kimberly Snyder showed steady growth in both popularity and influence, supported by frequent collaborations and notable releases.",
                    "Ping Tian had a brief peak with some influence but lacked lasting visibility.",
                    "Sailor Shift, though less active, made a strong impact by subtly shaping musical trends.",
                    "Overall, this suggests that being a rising star is not just about performing a lot as lasting success also comes from meaningful collaborations", 
                    "recognition, and the ability to influence others.")
                )
              )
      ),
      
      tabItem(tabName = "futurestars",
              fluidRow(
                box(
                  title = "Successful Stars in the Future",
                  width = 12, status = "primary", solidHeader = TRUE,
                  
                  dataTableOutput("future_stars_table"),
                  
                  tags$div(
                    style = "margin-top:1em; padding:0.75em; border:1px solid #ccc; background:#fafafa;",
                    "Artists ranked in this table are emerging figures in the Oceanus Folk community. ",
                    "We created a composite popularity and influence score (final_score) for all artists in the dataset.",
                     "This score was calculated using four normalized metrics: Number of songs performed, ", 
                     "Number of collaborations (shared credits), Number of notable mentions (awards or top charts)", 
                     "Influence spread (how many songs their music influenced)",
                     "All values were log-normalized and summed to produce a balanced final score.",
                    "Their high final scores reflect strong past engagement and influence potential, ",
                    "positioning them as likely future leaders in the genre."
                  )
                )
              )
      ),
      
      tabItem(tabName = "genrenetwork",
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE, title = "Interactive Genre Influence Network",
                  plotOutput("networkPlot", height = "700px")
                )
              )
      ),
      
      tabItem(
        tabName = "genretrend",
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Genre-wise Trend Analysis (2022–2035)",
            plotOutput("trendPlot", height = "700px")
          )
        )
      )
      
      
    )
  )
 )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # pre‐compute KPIs
  total_links        <- nrow(influences)
  years_range        <- range(weighted_df$release_year)
  unique_performers  <- n_distinct(weighted_df$performer)
  top_type           <- influences %>% 
    count(`Edge Type`) %>% 
    slice_max(n, n = 1) %>% 
    pull(`Edge Type`)
  
  # render the valueBoxes
  output$vb_total_links <- renderValueBox({
    valueBox(
      formatC(total_links, big.mark = ","),
      subtitle = "Total Influence Links",
      icon     = icon("project-diagram"),
      color    = "blue"
    )
  })
  
  output$vb_top_type <- renderValueBox({
    valueBox(
      top_type,
      subtitle = "Most Common Influence Type",
      icon     = icon("star"),
      color    = "teal"
    )
  })
  
  output$vb_years_covered <- renderValueBox({
    valueBox(
      paste0(years_range[1], " – ", years_range[2]),
      subtitle = "Years Covered",
      icon     = icon("calendar-alt"),
      color    = "purple"
    )
  })
  
  output$vb_unique_performers <- renderValueBox({
    valueBox(
      unique_performers,
      subtitle = "Distinct Performers",
      icon     = icon("users"),
      color    = "green"
    )
  })
  
  
  #filter for count
  filtered_influence <- reactive({
    df <- influences
    # apply the Edge-Type filter:
    if ( input$edgeType != "All" ) {
      df <- df %>% filter(`Edge Type` == input$edgeType)
    }
    #tally and drop any types below the slider threshold:
    df %>% 
      count(`Edge Type`) %>%
      filter(n >= input$min_inf_count)
  })
  
  
  

sailor_songs_tbl <- readRDS("data/sailor_songs_tbl.rds")
collab_summary <- readRDS("data/collab_summary.rds")
influence_tbl <- readRDS("data/influence_tbl.rds")
songs_tbl <- read_rds("data/songs_tbl.rds")
edges_label_data <- readRDS("data/edges_label_data.rds")
artist_metrics_scaled <- readRDS("data/artist_metrics_scaled.rds")
popularity_df <- readRDS("data/popularity_df.rds")
top_predictions <- readRDS("data/top_predictions.rds")


output$influencePlot <- renderPlot({
  filtered_influence() %>%
    ggplot(aes(x = reorder(`Edge Type`, n), y = n, fill = `Edge Type`)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(
      title = "How Sailor Shift Was Influenced",
      x = "Type of Influence",
      y = "Count"
    ) +
    theme_minimal()
})
  
  # Filtered data (year range)
  filtered_weighted <- reactive({
    weighted_df %>%
      filter(release_year >= input$year_range[1],
             release_year <= input$year_range[2])
  })
  
  output$topByYearPlot <- renderPlot({
    df <- filtered_weighted()
    ggplot(df, aes(x = factor(release_year), y = score, fill = type)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(performer, "\n(", type, ")")),
                vjust = -0.3, size = 3) +
      scale_y_continuous(expand = expansion(mult = c(0, .2))) +
      labs(
        title = "Most Influential Performer by Release Year",
        x = "Release Year", y = "Weighted Influence Score"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        panel.grid.major.x = element_blank()
      )
  })
  
  
  filtered_top_influencers <- reactive({
    top_influencers_named %>%
      filter(n >= input$min_links)
  })
  
  output$top_influencers_plot <- renderPlotly({
    df <- filtered_top_influencers()
    p <- ggplot(df,
                aes(x = n,
                    y = reorder(display_name, n),
                    text = paste("Influence Links:", n))) +
      geom_col(fill = "steelblue") +
      labs(
        title = "Top Influencers of All Time",
        x     = "Number of Influence Links",
        y     = "Influencer"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  
  output$song_table <- renderDataTable({
    sailor_songs_tbl
  })
  
  output$collab_table <- renderDataTable({
    collab_summary
  })
  
  output$influence_table <- renderDataTable({
    influence_tbl
  })
  
  output$overview_influence_table <- DT::renderDT({
    influence_tbl
  })
  
  output$download_graph <- downloadHandler(
    filename = function() {
      "sailor_shift_influence_graph.png"
    },
    content = function(file) {
      file.copy("www/graph_static.png", file)
    },
    contentType = "image/png"
  )
  
  output$top_artists_plot <- renderPlot({
    top_n <- input$top_n
    min_score <- input$min_score
    
    artist_metrics_scaled %>%
      filter(final_score >= min_score) %>%
      arrange(desc(final_score)) %>%
      slice(1:top_n) %>%
      ggplot(aes(x = reorder(name, final_score), y = final_score)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = round(final_score, 2)), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(
        title = paste("Top", top_n, "Artists with Score ≥", min_score),
        x = "Artist", y = "Final Composite Score"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"))
  })
  
  
  output$popularity_metrics_plot <- renderPlot({
    popularity_df %>%
      pivot_longer(cols = c(songs_performed, collabs, notable_mentions),
                   names_to = "metric", values_to = "value") %>%
      filter(release_year >= input$year_range[1],
             release_year <= input$year_range[2]) %>%
      ggplot(aes(x = as.numeric(release_year), y = value, color = artist)) +
      geom_line(size = 1.1) +
      geom_point(size = 2) +
      facet_wrap(~ metric, scales = "free_y", ncol = 1) +
      scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
      labs(
        title = "Popularity Metrics Over Time by Artist",
        x     = "Year",
        y     = "Count",
        color = "Artist"
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x     = element_text(angle = 45, hjust = 1),
        strip.text      = element_text(face = "bold")
      )
  })
  
  library(tibble)
  
  
  slope_data <- tibble::tibble(
    artist = c("Sailor Shift","Sailor Shift",
               "Kimberly Snyder","Kimberly Snyder",
               "Ping Tian",      "Ping Tian"),
    year   = c(2025, 2039,  2025, 2039,  2025, 2039),
    idx    = c(4.2, 7.9,    6.0, 6.8,    3.5, 3.9)
  ) %>%
    dplyr::mutate(year = factor(year, levels = c(2025, 2039)))
  
  # now wire it up to output$pop_index_plot exactly once:
  output$pop_index_plot <- renderPlot({
    ggplot2::ggplot(slope_data, aes(x = year, y = idx, color = artist, group = artist)) +
      ggplot2::geom_line(size = 1.5) +
      ggplot2::geom_point(size = 4) +
      ggplot2::geom_text(aes(label = paste0(artist, ": ", idx)),
                         vjust = -1, hjust = 0.5, show.legend = FALSE) +
      ggplot2::labs(
        title = "Popularity Index Change (2025 → 2039)",
        x     = NULL,
        y     = "Popularity Index"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(face = "bold"),
        plot.title       = element_text(hjust = 0.5),
        legend.position  = "none"
      )
  })
  
  
  output$future_stars_table <- renderDataTable({
    top_predictions %>%
      mutate(ID = row_number()) %>%
      select(ID, Artist = artist, Score = final_score)
  }, options = list(
    pageLength   = 10,
    searching    = FALSE,
    lengthChange = FALSE
  ))
  
  output$future_stars_overview_table <- renderDataTable({
    top_predictions %>%
      mutate(ID = row_number()) %>%
      select(ID, Artist = artist, Score = final_score)
  }, options = list(
    pageLength   = 10,
    searching    = FALSE,
    lengthChange = FALSE
  ))
  
  #GenreInfluenceNetwork
  output$networkPlot <- renderPlot({
    central_genre <- input$selected_genre
    if (is.null(central_genre) || central_genre == "") {
      central_genre <- valid_genres[1]
    }
    
    # Grab just the songs for that genre
    genre_songs <- nodes_tbl %>%
      filter(`Node Type` == "Song",
             genre       == central_genre,
             !is.na(release_date),
             str_detect(release_date, "^\\d{4}")) %>%
      mutate(year = as.integer(substr(release_date,1,4))) %>%
      filter(year >= 2022, year <= 2035)
    
    # if no songs:  
    if (nrow(genre_songs) == 0) {
      plot.new()
      text(0.5, 0.5,
           paste("No influence links found for\n", central_genre),
           cex = 1.2, font = 2)
      return()
    }
    
    
    # 1) Filter & tally
    genre_counts <- edges2 %>%
      filter(rel_type %in% influence_types) %>%
      semi_join(
        nodes_tbl %>% filter(`Node Type` == "Song", genre == central_genre) %>% select(id),
        by = c("target" = "id")
      ) %>%
      left_join(nodes_tbl %>% select(id, genre), by = c("source" = "id")) %>%
      filter(!is.na(genre)) %>%
      group_by(genre, rel_type) %>%
      summarise(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = rel_type, values_from = n, values_fill = 0) %>%
      filter(genre != central_genre) %>%
      mutate(total_genre = rowSums(select(., all_of(influence_types))))
    
    rel_totals <- colSums(select(genre_counts, all_of(influence_types)))
    
    
    # 2) Build nodes & edges
    nodes <- bind_rows(
      tibble(id = central_genre, type = "central", count = sum(genre_counts$total_genre)),
      tibble(id = genre_counts$genre, type = "genre", count = genre_counts$total_genre),
      tibble(id = influence_types, type = "reltype", count = as.integer(rel_totals))
    )
    
    edges1 <- tibble(from = central_genre,
                     to   = genre_counts$genre,
                     weight = genre_counts$total_genre)
    
    edges_rel <- genre_counts %>%
      pivot_longer(all_of(influence_types), names_to = "to", values_to = "weight") %>%
      filter(weight > 0) %>%
      transmute(from = genre, to = to, weight = weight)
    
    edges <- bind_rows(edges1, edges_rel)
    
    # 3) Manual layout & plot
    graph_plot <- tbl_graph(nodes, edges, directed = FALSE)
    
    genres <- nodes %>% filter(type == "genre")  %>% pull(id)
    rels   <- nodes %>% filter(type == "reltype")%>% pull(id)
    ang_g  <- seq(0, 2*pi, length.out = length(genres)+1)[-1]
    ang_r  <- seq(0, 2*pi, length.out = length(rels)+1)[-1]
    
    layout <- tibble(
      id = c(central_genre, genres, rels),
      x  = c(0, cos(ang_g), 2*cos(ang_r)),
      y  = c(0, sin(ang_g), 2*sin(ang_r))
    )
    
    l <- create_layout(graph_plot, layout = "manual",
                       x = layout$x, y = layout$y)
    
    ggraph(l) +
      geom_edge_link(aes(width = weight), color = "grey70", alpha = 0.8) +
      scale_edge_width(range = c(0.2, 1.5)) +
      geom_node_point(aes(size = count, fill = type), shape = 21, color = "black") +
      scale_size_area(max_size = 15) +
      scale_fill_manual(values = c(central = "#1f78b4", genre = "#33a02c", reltype = "#e31a1c")) +
      geom_node_text(aes(label = id), repel = TRUE, size = 3) +
      theme_void() +
      labs(title = paste(central_genre,
                         "→ Genres → Influence Types \n(Excluding", central_genre, ")"))
  })
  
  #GenreWisePlot
  output$trendPlot <- renderPlot({
    req(input$trend_genre)
    
    # parameters
    selected_genre <- input$trend_genre
    start_year     <- input$trend_years[1]
    end_year       <- input$trend_years[2]
    
    # 1) Grab that genre’s songs
    genre_songs <- nodes_tbl %>%
      filter(
        `Node Type` == "Song",
        genre == selected_genre,
        !is.na(release_date),
        str_detect(release_date, "^\\d{4}")
      ) %>%
      mutate(year = as.integer(str_extract(release_date, "^\\d{4}"))) %>%
      filter(year >= 2022, year <= 2035) %>%
      select(id, year)
    
    if (nrow(genre_songs) == 0) {
      showNotification("No data available for this genre.", type = "warning")
      return(NULL)
    }
    
    # 2) Count each rel_type by year
    counts_by_year <- edges2 %>%
      filter(rel_type %in%  influence_types) %>%
      mutate(song_id = if_else(rel_type == "LyricistOf", target, source)) %>%
      inner_join(genre_songs, by = c("song_id" = "id")) %>%
      group_by(year, rel_type) %>%
      summarise(count = n(), .groups = "drop") %>%
      pivot_wider(names_from  = rel_type,
                  values_from = count,
                  values_fill = 0) %>%
      arrange(year)
    
    # 3) Count songs vs. albums by year
    media_counts <- nodes_tbl %>%
      filter(
        `Node Type` %in% c("Song", "Album"),
        genre == selected_genre,
        !is.na(release_date),
        str_detect(release_date, "^\\d{4}")
      ) %>%
      mutate(year = as.integer(str_extract(release_date, "^\\d{4}"))) %>%
      filter(year >= 2022, year <= 2035) %>%
      count(year, `Node Type`) %>%
      pivot_wider(names_from  = `Node Type`,
                  values_from = n,
                  values_fill = 0) %>%
      rename(song_count  = Song,
             album_count = Album) %>%
      arrange(year)
    
    # 4) Join them and pivot long
    final_table <- counts_by_year %>%
      left_join(media_counts, by = "year")
    
    plot_df <- final_table %>%
      filter(year >= start_year, year <= end_year) %>%
      pivot_longer(cols = -year,
                   names_to  = "variable",
                   values_to = "count")
    
    if (nrow(plot_df) == 0) {
      showNotification("No data to plot for this genre in the selected year range.", type = "warning")
      return(NULL)
    }
    
    # 5) Draw the line chart
    ggplot(plot_df, aes(x = year, y = count, color = variable)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = seq(start_year, end_year, by = 1)) +
      labs(
        title = paste("Trend of", selected_genre, "Music (", start_year, "–", end_year, ")"),
        x = "Year",
        y = "Count",
        color = "Type"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # -------------  
  # Mini Genre-Trend  
  # -------------
  output$overview_trendPlot <- renderPlot({
    req(input$overview_trend_genre, input$year_range)
    
    selected_genre <- input$overview_trend_genre
    start_year     <- input$year_range[1]
    end_year       <- input$year_range[2]
    
    genre_songs <- nodes_tbl %>%
      filter(
        `Node Type` == "Song",
        genre        == selected_genre,
        !is.na(release_date),
        str_detect(release_date, "^\\d{4}")
      ) %>%
      mutate(year = as.integer(substr(release_date,1,4))) %>%
      filter(year >= start_year, year <= end_year) %>%
      select(id, year)
    
    validate(need(nrow(genre_songs) > 0, paste("No trend data for", selected_genre)))
    
    counts_by_year <- edges2 %>%
      filter(rel_type %in% influence_types) %>%
      mutate(song_id = if_else(rel_type == "LyricistOf", target, source)) %>%
      inner_join(genre_songs, by = c("song_id"="id")) %>%
      count(year, rel_type) %>%
      pivot_wider(names_from  = rel_type,
                  values_from = n,
                  values_fill  = 0)
    
    media_counts <- nodes_tbl %>%
      filter(
        `Node Type` %in% c("Song","Album"),
        genre == selected_genre,
        !is.na(release_date),
        str_detect(release_date, "^\\d{4}")
      ) %>%
      mutate(year = as.integer(substr(release_date,1,4))) %>%
      filter(year >= start_year, year <= end_year) %>%
      count(year, `Node Type`) %>%
      pivot_wider(names_from  = `Node Type`,
                  values_from = n,
                  values_fill  = 0) %>%
      rename(song_count = Song, album_count = Album)
    
    plot_df <- left_join(counts_by_year, media_counts, by = "year") %>%
      pivot_longer(-year, names_to = "Type", values_to = "Count")
    
    ggplot(plot_df, aes(x = year, y = Count, color = Type)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = seq(start_year, end_year, by = 1)) +
      theme_minimal() +
      labs(x = "Year", y = "Count", color = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # -------------  
  # Mini Genre-Network  
  # -------------
  output$overview_networkPlot <- renderPlot({
    central_genre <- input$overview_trend_genre  # or reuse a separate select if you like
    if (is.null(central_genre) || central_genre == "") central_genre <- valid_genres[1]
    
    # build the same tiny network you already have
    genre_counts <- edges2 %>%
      filter(rel_type %in% influence_types) %>%
      semi_join(
        nodes_tbl %>% filter(`Node Type`=="Song", genre==central_genre) %>% select(id),
        by = c("target"="id")
      ) %>%
      left_join(nodes_tbl %>% select(id, genre), by = c("source"="id")) %>%
      filter(!is.na(genre)) %>%
      group_by(genre, rel_type) %>%
      summarise(n = n(), .groups="drop") %>%
      pivot_wider(names_from = rel_type, values_from = n, values_fill = 0) %>%
      mutate(total = rowSums(select(., all_of(influence_types))))
    
    rel_totals <- colSums(select(genre_counts, all_of(influence_types)))
    
    nodes <- bind_rows(
      tibble(id=central_genre, type="central", count=sum(genre_counts$total)),
      tibble(id=genre_counts$genre, type="genre",  count=genre_counts$total),
      tibble(id=influence_types, type="reltype", count=as.integer(rel_totals))
    )
    edges <- bind_rows(
      tibble(from=central_genre, to=genre_counts$genre, weight=genre_counts$total),
      genre_counts %>%
        pivot_longer(all_of(influence_types), names_to="to", values_to="weight") %>%
        filter(weight>0) %>%
        transmute(from=genre, to=to, weight=weight)
    )
    
    graph_plot <- tbl_graph(nodes, edges, directed=FALSE)
    
    # super-tiny manual layout
    ang_g <- seq(0, 2*pi, length.out=nrow(filter(nodes,type=="genre"))+1)[-1]
    ang_r <- seq(0, 2*pi, length.out=nrow(filter(nodes,type=="reltype"))+1)[-1]
    
    layout <- tibble(
      id = c(central_genre, filter(nodes,type=="genre")$id, filter(nodes,type=="reltype")$id),
      x  = c(0, cos(ang_g), 2*cos(ang_r)),
      y  = c(0, sin(ang_g), 2*sin(ang_r))
    )
    
    l <- create_layout(graph_plot, layout="manual", x=layout$x, y=layout$y)
    
    ggraph(l) +
      geom_edge_link(aes(width=weight), color="grey70", alpha=0.6) +
      geom_node_point(aes(size=count, fill=type), shape=21) +
      theme_void() +
      theme(legend.position="none")
  })
  

  

}

# Run the application 
shinyApp(ui = ui, server = server)

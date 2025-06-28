#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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
weighted_df <- read_rds("data/weighted_top_by_year_auto.rds")
collabs_tbl <- read_rds("data/sailor_collaborations_named.rds")
edges2 <- read_rds("data/edges2.rds")
nodes_tbl <- readRDS("data/nodes_tbl.rds")

influence_types <- c("InStyleOf", "CoverOf", "DirectlySamples", 
                     "InterpolatesFrom", "LyricalReferenceTo")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Sailor Shift Journey"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Top Influencers", tabName = "topinfluencers", icon = icon("star")),
      menuItem("Raw Song Data", tabName = "raw", icon = icon("table")),
      menuItem("Collaborators", tabName = "collaborators", icon = icon("users")),
      menuItem("Influence Graph", tabName = "graph", icon = icon("project-diagram")),
      menuItem("Top Artist Rankings", tabName = "topartists", icon = icon("chart-bar")),
      menuItem("Popularity Metrics", tabName = "popmetrics", icon = icon("chart-bar")),
      menuItem("Popularity Index Change", tabName = "popindex", icon = icon("exchange-alt")),
      menuItem("Predicted Future Stars", tabName = "futurestars", icon = icon("star")),
      menuItem("Genre Influence Network", tabName = "genrenetwork", icon = icon("project-diagram"))
      
      
      
      
      
      
    ),
    selectInput("edgeType", "Filter by Influence Type:",
                choices = c("All", unique(influences$`Edge Type`)), selected = "All"),
    sliderInput("year_range", "Year Range:",
                min = min(weighted_df$release_year),
                max = max(weighted_df$release_year),
                value = range(weighted_df$release_year), sep = "")
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              h3("Performer Influence Type Breakdown"),
              fluidRow(
                box(plotOutput("influencePlot"), width = 12),
                box(plotOutput("topByYearPlot"), width = 12)
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
              tags$img(src = "igraph.png", width = "100%", style = "max-height: 800px; object-fit: contain;")
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
      
      tabItem(tabName = "popmetrics",
              h3("Popularity Metrics Over Time by Artist"),
              fluidRow(
                box(
                  width = 12,
                  plotOutput("popularity_metrics_plot", height = "600px")
                )
              )
      ),
      
      tabItem(tabName = "popindex",
              h3("Popularity Index Change (2025 → 2039)"),
              fluidRow(
                box(width = 12,
                    plotOutput("pop_index_plot", height = "500px")
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
                    "Their high final scores reflect strong past engagement and influence potential, ",
                    "positioning them as likely future leaders in the genre."
                  )
                )
              )
      ),
      
      tabItem(tabName = "genrenetwork",
              fluidRow(
                box(
                  width = 3, status = "primary", solidHeader = TRUE, title = "Controls",
                  selectInput("selected_genre", "Select Central Genre:",
                              choices = sort(unique(nodes_tbl$genre)),
                              selected = sort(unique(nodes_tbl$genre))[1])
                ),
                box(
                  width = 9, status = "info", solidHeader = TRUE, title = "Interactive Genre Influence Network",
                  plotOutput("networkPlot", height = "700px")
                )
              )
      )
      
   )
 )
) 
  


  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Filtered data (influence type)
  filtered_influence <- reactive({
    if (input$edgeType == "All") {
      influences
    } else {
      influences %>% filter(`Edge Type` == input$edgeType)
    }
  })
  
top_influencers_named <- readRDS("data/top_influencers_named.rds")
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
      count(`Edge Type`) %>%
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
  
  
  output$top_influencers_plot <- renderPlotly({
    p <- ggplot(top_influencers_named, aes(x = n, y = reorder(display_name, n), text = paste("Influence Links:", n))) +
      geom_col(fill = "steelblue") +
      labs(title = "Top Influencers of All Time",
           x = "Number of Influence Links",
           y = "Influencer") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  
  output$song_table <- renderDataTable({
    # If using filter:
    # filtered_songs()
    
    # If no filter needed:
    sailor_songs_tbl
  })
  
  output$collab_table <- renderDataTable({
    collab_summary
  })
  
  
  output$influence_table <- renderDataTable({
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
  
  # hard-code the two‐point index values
  slope_data <- tibble(
    artist = c("Sailor Shift","Sailor Shift",
               "Kimberly Snyder","Kimberly Snyder",
               "Ping Tian",      "Ping Tian"),
    year   = c(2025, 2039,  2025, 2039,  2025, 2039),
    idx    = c(4.2, 7.9,    6.0, 6.8,    3.5, 3.9)
  ) %>%
    mutate(year = factor(year, levels = c(2025, 2039)))
  
  output$pop_index_plot <- renderPlot({
    ggplot(slope_data, aes(x = year, y = idx, color = artist, group = artist)) +
      geom_line(size = 1.5) +
      geom_point(size = 4) +
      # label each point with “Artist: value”
      geom_text(aes(label = paste0(artist, ": ", idx)),
                vjust = -1, hjust = 0.5, show.legend = FALSE) +
      labs(x = NULL, y = "Popularity Index") +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x       = element_text(face = "bold"),
        plot.title        = element_text(hjust = 0.5),
        legend.position   = "none"
      )
  })
  
  output$future_stars_table <- renderDataTable({
    top_predictions %>%
      # If you’d like to show a simple ID column:
      mutate(ID = row_number()) %>%
      select(ID, Artist = artist, Score = final_score)
  }, options = list(
    pageLength   = 10,
    searching    = FALSE,
    lengthChange = FALSE
  ))
  
  output$networkPlot <- renderPlot({
    req(input$selected_genre)
    
    central_genre <- input$selected_genre
    
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
    
    if (nrow(genre_counts) == 0) {
      showNotification("No data available for this genre.", type = "warning")
      return(NULL)
    }
    
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
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

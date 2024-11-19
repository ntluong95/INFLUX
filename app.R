pacman::p_load(
  rio,       
  here,        
  igraph, 
  tidygraph, 
  ggraph, 
  janitor,
  bslib,
  shiny,    
  tidyverse,
  viridis
)

result_df <- import(here("notebooks", "cosine_test_31 Oct.csv")) #import new data

ui <- page_sidebar(
  title = "Network Analysis Visualization",
  sidebar = sidebar(
    selectInput("centrality_method", "Centrality Measure:",
                choices = c("Degree" = "degree",
                          "Betweenness" = "betweenness",
                          "Closeness" = "closeness"),
                selected = "betweenness"),
    selectInput("community_method", "Community Detection:",
                choices = c("Cosine Similarity" = "cosine",
                          "Infomap" = "infomap",
                          "Label Propagation" = "label_prop",
                          "Leading Eigenvector" = "leading_eigen"),
                selected = "infomap"),
    selectInput("layout_type", "Graph Layout:",
                choices = c("Fruchterman-Reingold" = "fr",
                          "Kamada-Kawai" = "kk",
                          "Stress" = "stress",
                          "Circle" = "circle",
                          "Grid" = "grid")),
    numericInput("small_cluster_threshold", 
                 "Small Cluster Threshold:", 
                 value = 5, 
                 min = 1),
    hr(),
    helpText("Select different methods to analyze network centrality and community structure")
  ),
  
  card(
    card_header("Network Visualization"),
    plotOutput("network_plot", height = "800px")
  )
)

server <- function(input, output, session) {
  
  get_cluster_colors <- function(n) {
    viridis(n, option = "D")
  }
  
  network_data <- reactive({
    # First create nodes dataframe with cluster information
    nodes_df <- result_df %>%
      select(Cause_category, Cause_cluster) %>%
      rename(name = Cause_category, cluster = Cause_cluster) %>%
      bind_rows(
        result_df %>%
          select(Effect_category, Effect_cluster) %>%
          rename(name = Effect_category, cluster = Effect_cluster)
      ) %>%
      # Keep original name and cluster before transformation
      mutate(
        original_name = name,
        original_cluster = cluster,
        name = case_when(
          str_detect(name, "transmission|spread") ~ "disease transmission",
          TRUE ~ name
        )
      ) %>%
      # Group by the new name and keep all unique clusters
      group_by(name) %>%
      mutate(
        all_clusters = paste(sort(unique(original_cluster)), collapse = "_")
      ) %>%
      ungroup() %>%
      # Keep one row per unique name-cluster combination
      distinct(name, all_clusters, .keep_all = TRUE) %>%
      filter(!name %in% c("No content", "No context"))
    
    # Create edges dataframe
    edges_df <- result_df %>%
      select(Cause_category, Effect_category) %>%
      filter(!Cause_category %in% c("No content", "No context")) %>%
      mutate(across(c(Cause_category, Effect_category),
                   ~case_when(
                     str_detect(., "transmission|spread") ~ "disease transmission",
                     TRUE ~ .
                   )))
    
    # Create graph with node attributes
    network_recode1 <- tbl_graph(nodes = nodes_df, 
                                edges = edges_df, 
                                directed = TRUE)
    
    # Calculate centrality and community
    network_recode1 <- network_recode1 %>%
      mutate(
        centrality = case_when(
          input$centrality_method == "degree" ~ centrality_degree(),
          input$centrality_method == "betweenness" ~ centrality_betweenness(),
          input$centrality_method == "closeness" ~ centrality_closeness()
        ),
        community = case_when(
          input$community_method == "cosine" ~ as.factor(all_clusters),
          input$community_method == "infomap" ~ as.factor(group_infomap()),
          input$community_method == "label_prop" ~ as.factor(group_label_prop()),
          input$community_method == "leading_eigen" ~ as.factor(group_leading_eigen())
        )
      )
    
    # Only apply small cluster threshold for non-cosine methods
    if(input$community_method != "cosine") {
      network_recode1 <- network_recode1 %>%
        mutate(
          community = if_else(
            community %in% names(which(table(community) <= input$small_cluster_threshold)), 
            "Unclassified", 
            "Cluster"
          ),
          community = as.factor(community)
        )
    }
    
    network_recode1 %>%
      activate(edges) %>%
      mutate(edge_type = if_else(.N()$centrality[from] > 3, 
                                "link to endpoint", 
                                "link between drivers"))
  })
  
  cluster_colors <- reactive({
    g <- network_data()
    
    if(input$community_method == "cosine") {
      unique_communities <- sort(unique(as.character(V(g)$community)))
      n_communities <- length(unique_communities)
      colors <- get_cluster_colors(n_communities)
      names(colors) <- unique_communities
      colors
    } else {
      c("Unclassified" = "#1b9e77", "Cluster" = "#7570b3")
    }
  })
  
  output$network_plot <- renderPlot({
    g <- network_data()
    
    centrality_scaled <- scales::rescale(V(g)$centrality, to = c(0.3, 1))
    
    ggraph(g, layout = input$layout_type) + 
      geom_edge_link(aes(color = edge_type),
                    arrow = arrow(length = unit(4, 'mm'), 
                                type = "closed"), 
                    end_cap = circle(2, 'mm'), 
                    alpha = 0.7, 
                    edge_width = 1) +
      geom_node_point(aes(size = centrality, 
                         color = community), 
                     show.legend = TRUE) +
      geom_node_text(aes(label = name,
                        size = centrality,
                        alpha = centrality),
                    repel = TRUE) +
      scale_color_manual(values = cluster_colors(), 
                        name = "Cluster") +
      scale_edge_color_manual(values = c("link between drivers" = "gray", 
                                       "link to endpoint" = "#d95f02")) +
      scale_size(range = c(3, 10)) + 
      scale_alpha(range = c(0.3, 1)) +
      theme_graph() +
      labs(title = paste("Network Analysis using", 
                        tools::toTitleCase(input$centrality_method), 
                        "centrality and",
                        tools::toTitleCase(input$community_method), 
                        "community detection")) +
      guides(alpha = "none")
  })
}

shinyApp(ui, server)
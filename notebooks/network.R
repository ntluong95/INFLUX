pacman::p_load(
  rio,       
  here,     
  igraph,
  tidygraph, 
  ggraph, 
  janitor,    
  tidyverse     
)

result_df <- import(here("notebooks", "result_df_31 Oct.csv"))

# Test cosine similarity ---------------------------------------------------------------------

# Create a vector that contains all unique Cause_category and Effect_category
unique_drviers_categories <- unique(c(result_df$Cause_category, result_df$Effect_category))

# Get embeddings for each unique category using a transformer model
embeddings <- text::transformers_embedding(unique_drivers_categories, model = "sentence-transformers/all-mpnet-base-v2")

# Test normally ------------------------------------------------------------------------------

network <- result_df %>% 
  select(Cause_category, Effect_category) %>% 
  filter(!Cause_category %in% c("No content", "No context")) %>% #17 rows were removed
  as_tbl_graph(directed = TRUE)

# Plot the network
ggraph(network, layout = "dh") + 
  geom_edge_link(
    color = "grey55",
    arrow = arrow(length = unit(0.13, "inches"), angle = 10,ends = "last", type = "closed"),
    end_cap = circle(8, 'mm')) + 
  geom_node_point(size = 5, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph()


network_recode <- result_df %>% 
  select(Cause_category, Effect_category) %>% 
  filter(!Cause_category %in% c("No content", "No context")) %>% #17 rows were removed
  #Recode any strings containing "transmission" into "Disease transmission"
  mutate(Cause_category = case_when(
    str_detect(Cause_category, "transmission") ~ "disease transmission",
    TRUE ~ Cause_category)) %>% 
  mutate(Effect_category = case_when(
    str_detect(Effect_category, "transmission") ~ "disease transmission",
    TRUE ~ Effect_category)) %>%
  as_tbl_graph(directed = TRUE)

# Plot the network
ggraph(network_recode, layout = "dh") + 
  geom_edge_link(
    color = "grey55",
    arrow = arrow(length = unit(0.13, "inches"), angle = 10,ends = "last", type = "closed"),
    end_cap = circle(8, 'mm')) + 
  geom_node_point(size = 5, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph()



network_recode1 <- result_df %>% 
  select(Cause_category, Effect_category) %>% 
  filter(!Cause_category %in% c("No content", "No context")) %>% #17 rows were removed
  #Recode any strings containing "transmission" into "Disease transmission"
  mutate(Cause_category = case_when(
    str_detect(Cause_category, "transmission") ~ "disease transmission",
    str_detect(Cause_category, "spread") ~ "disease transmission",
    TRUE ~ Cause_category)) %>% 
  mutate(Effect_category = case_when(
    str_detect(Effect_category, "transmission") ~ "disease transmission",
    str_detect(Effect_category, "spread") ~ "disease transmission",
    TRUE ~ Effect_category)) %>%
  as_tbl_graph(directed = TRUE) %>%
  mutate(
    degree = centrality_degree(),
    community = as.factor(group_infomap())) %>% 
  mutate(community = if_else(community %in% names(which(table(community) <= 5)), "Unclassified", "Cluster")) %>%
  # mutate(community = if_else(is.na(community), "Unclassified", community)) %>%
  mutate(community = as.factor(community)) %>% 
  activate(edges) %>%
  mutate(edge_type = if_else(.N()$degree[from] > 3, "link to endpoint", "link between drivers")) # This checks if the degree of the starting node (from) of an edge is greater than 2.

# Define colors for clusters and edges
cluster_colors <- c("Unclassified" = "#1b9e77", "Cluster" = "#7570b3")

# Plot the network
ggraph(network_recode1, layout = "fr") + 
  geom_edge_link(aes(color = edge_type),
                 arrow = arrow(length = unit(4, 'mm'), type = "closed"), 
                 end_cap = circle(2, 'mm'), alpha = 0.7, edge_width = 1) +
  geom_node_point(aes(size = degree, color = community), show.legend = TRUE) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = cluster_colors, name = "Cluster") +
  scale_edge_color_manual(values = c("link between drivers" = "gray", "link to endpoint" = "#d95f02")) +
  scale_size(range = c(3, 10)) + 
  theme_graph()

# archived -----------------------------------------------------------------------------------


pacman::p_load(
  rio,       
  here,        
  igraph, 
  tidygraph, 
  ggraph, 
  janitor,
  bslib,
  shiny,    
  tidyverse     
)

result_df <- import(here("notebooks", "cosine_test_31 Oct.csv"))

ui <- page_sidebar(
  title = "Network Analysis Visualization",
  sidebar = sidebar(
    selectInput("centrality_method", "Centrality Measure:",
                choices = c("Degree" = "degree",
                          "Betweenness" = "betweenness",
                          "Closeness" = "closeness")),
    selectInput("community_method", "Community Detection:",
                choices = c("Infomap" = "infomap",
                          "Label Propagation" = "label_prop",
                          "Leading Eigenvector" = "leading_eigen")),
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
  
  # Define cluster colors
  cluster_colors <- c("Unclassified" = "#1b9e77", "Cluster" = "#7570b3")
  
  network_data <- reactive({
    network_recode1 <- result_df %>% 
      select(Cause_category, Effect_category) %>% 
      filter(!Cause_category %in% c("No content", "No context")) %>%
      mutate(across(c(Cause_category, Effect_category), 
                   ~case_when(
                     str_detect(., "transmission") ~ "disease transmission",
                     str_detect(., "spread") ~ "disease transmission",
                     TRUE ~ .
                   ))) %>%
      as_tbl_graph(directed = TRUE)
    
    # Calculate centrality and community
    network_recode1 <- network_recode1 %>%
      mutate(
        centrality = case_when(
          input$centrality_method == "degree" ~ centrality_degree(),
          input$centrality_method == "betweenness" ~ centrality_betweenness(),
          input$centrality_method == "closeness" ~ centrality_closeness()
        ),
        raw_community = case_when(
          input$community_method == "infomap" ~ as.factor(group_infomap()),
          input$community_method == "label_prop" ~ as.factor(group_label_prop()),
          input$community_method == "leading_eigen" ~ as.factor(group_leading_eigen())
        )
      ) %>%
      # Reclassify small communities
      mutate(
        community = if_else(
          raw_community %in% names(which(table(raw_community) <= input$small_cluster_threshold)), 
          "Unclassified", 
          "Cluster"
        ),
        community = as.factor(community)
      ) %>%
      activate(edges) %>%
      mutate(edge_type = if_else(.N()$centrality[from] > 3, 
                                "link to endpoint", 
                                "link between drivers"))
    
    network_recode1
  })
  
  output$network_plot <- renderPlot({
    g <- network_data()
    
    # Calculate text size scaling and alpha (transparency)
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
                        alpha = centrality), # Add alpha aesthetic
                    repel = TRUE) +
      scale_color_manual(values = cluster_colors, 
                        name = "Cluster") +
      scale_edge_color_manual(values = c("link between drivers" = "gray", 
                                       "link to endpoint" = "#d95f02")) +
      scale_size(range = c(3, 10)) + 
      scale_alpha(range = c(0.3, 1)) + # Add alpha scale
      theme_graph() +
      labs(title = paste("Network Analysis using", 
                        tools::toTitleCase(input$centrality_method), 
                        "centrality and",
                        tools::toTitleCase(input$community_method), 
                        "community detection")) +
      guides(alpha = "none") # Hide alpha legend
  })
}

shinyApp(ui, server)


# archived 2 ---------------------------------------------------------------------------------

pacman::p_load(
  rio,       
  here,        
  igraph, 
  tidygraph, 
  ggraph, 
  janitor,
  bslib,
  shiny,    
  tidyverse     
)

result_df <- import(here("notebooks", "cosine_test_31 Oct.csv"))

ui <- page_sidebar(
  title = "Network Analysis Visualization",
  sidebar = sidebar(
    selectInput("centrality_method", "Centrality Measure:",
                choices = c("Degree" = "degree",
                          "Betweenness" = "betweenness",
                          "Closeness" = "closeness")),
    selectInput("community_method", "Community Detection:",
                choices = c("Cosine Similarity" = "cosine",
                          "Infomap" = "infomap",
                          "Label Propagation" = "label_prop",
                          "Leading Eigenvector" = "leading_eigen")),
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
  
  # Define cluster colors
  cluster_colors <- c("Unclassified" = "#1b9e77", "Cluster" = "#7570b3")
  
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
      distinct(name, .keep_all = TRUE) %>%
      filter(!name %in% c("No content", "No context")) %>%
      mutate(name = case_when(
        str_detect(name, "transmission") ~ "disease transmission",
        str_detect(name, "spread") ~ "disease transmission",
        TRUE ~ name
      ))
    
    # Create edges dataframe
    edges_df <- result_df %>%
      select(Cause_category, Effect_category) %>%
      filter(!Cause_category %in% c("No content", "No context")) %>%
      mutate(across(c(Cause_category, Effect_category),
                   ~case_when(
                     str_detect(., "transmission") ~ "disease transmission",
                     str_detect(., "spread") ~ "disease transmission",
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
        raw_community = case_when(
          input$community_method == "cosine" ~ as.factor(cluster),
          input$community_method == "infomap" ~ as.factor(group_infomap()),
          input$community_method == "label_prop" ~ as.factor(group_label_prop()),
          input$community_method == "leading_eigen" ~ as.factor(group_leading_eigen())
        )
      ) %>%
      # Reclassify small communities
      mutate(
        community = if_else(
          raw_community %in% names(which(table(raw_community) <= input$small_cluster_threshold)), 
          "Unclassified", 
          "Cluster"
        ),
        community = as.factor(community)
      ) %>%
      activate(edges) %>%
      mutate(edge_type = if_else(.N()$centrality[from] > 3, 
                                "link near to the endpoint", 
                                "link between drivers"))
    
    network_recode1
  })
  
  output$network_plot <- renderPlot({
    g <- network_data()
    
    # Calculate text size scaling and alpha (transparency)
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
      scale_color_manual(values = cluster_colors, 
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

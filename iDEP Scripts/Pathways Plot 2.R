# Horizontal Bar Plot for Pathway Enrichment Data
# Custom parameters: sort by -log10(FDR), x-axis: fold enrichment, 
# size: genes, font: 12pt, aspect ratio: 1.5, colors: blue to red

# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)
# Interactive file selection
result <- load_data_and_plot()
# Function to create horizontal pathway bar plot
create_horizontal_pathway_plot <- function(data, 
                                           top_n = 20,
                                           fdr_threshold = 0.05,
                                           wrap_length = 50) {
  
  # Filter significant pathways and calculate -log10(FDR)
  data_filtered <- data %>%
    filter(FDR < fdr_threshold) %>%
    mutate(neg_log10_FDR = -log10(FDR)) %>%
    arrange(desc(neg_log10_FDR)) %>%  # Sort by -log10(FDR) descending
    slice_head(n = top_n)
  
  if (nrow(data_filtered) == 0) {
    stop("No significant pathways found with FDR < ", fdr_threshold)
  }
  
  # Wrap long pathway names
  data_filtered$Pathway_wrapped <- stringr::str_wrap(data_filtered$Pathway, width = wrap_length)
  
  # Reorder pathways by -log10(FDR) for plotting (most significant at top)
  data_filtered$Pathway_wrapped <- factor(data_filtered$Pathway_wrapped, 
                                          levels = data_filtered$Pathway_wrapped)
  
  # Create the horizontal bar plot
  p <- ggplot(data_filtered, aes(x = `Fold enriched`, y = Pathway_wrapped)) +
    geom_col(aes(fill = neg_log10_FDR, width = nGenes/max(nGenes)), 
             color = "white", size = 0.2) +
    scale_fill_gradient(low = "blue", high = "red", 
                        name = expression(-log[10](FDR))) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(
      text = element_text(size = 12, family = "serif"),  # marplot style font
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", size = 0.3),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.text.x = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      aspect.ratio = 1/1.5,  # Aspect ratio of 1.5 (width:height)
      plot.margin = margin(20, 20, 20, 20)
    ) +
    labs(
      x = "-log10(FDR)",
      caption = paste("Top", nrow(data_filtered), "pathways (FDR <", fdr_threshold, ")")
    )
  
  return(list(plot = p, data = data_filtered))
}

# Alternative version with bar width representing gene count more explicitly
create_horizontal_pathway_plot_v2 <- function(data, 
                                              top_n = 20,
                                              fdr_threshold = 0.05,
                                              wrap_length = 50) {
  
  # Filter and process data
  data_filtered <- data %>%
    filter(FDR < fdr_threshold) %>%
    mutate(neg_log10_FDR = -log10(FDR)) %>%
    arrange(desc(neg_log10_FDR)) %>%
    slice_head(n = top_n)
  
  # Wrap pathway names
  data_filtered$Pathway_wrapped <- stringr::str_wrap(data_filtered$Pathway, width = wrap_length)
  data_filtered$Pathway_wrapped <- factor(data_filtered$Pathway_wrapped, 
                                          levels = data_filtered$Pathway_wrapped)
  
  # Normalize gene count for bar width (0.3 to 0.9 range)
  data_filtered$bar_width <- 0.3 + (data_filtered$nGenes - min(data_filtered$nGenes)) / 
    (max(data_filtered$nGenes) - min(data_filtered$nGenes)) * 0.6
  
  # Create plot with variable bar widths
  p <- ggplot(data_filtered, aes(x = `Fold enriched`, y = Pathway_wrapped)) +
    geom_col(aes(fill = neg_log10_FDR, width = bar_width), 
             color = "white", size = 0.2) +
    scale_fill_gradient(low = "blue", high = "red", 
                        name = expression(-log[10](FDR))) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(
      text = element_text(size = 12, family = "serif"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", size = 0.3),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.text.x = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      aspect.ratio = 1/1.5,
      plot.margin = margin(20, 20, 20, 20)
    ) +
    labs(
      title = "Pathway Enrichment Analysis",
      x = "Fold Enrichment",
      y = "Pathway",
      caption = paste("Top", nrow(data_filtered), "pathways (FDR <", fdr_threshold, ")\nBar width represents gene count")
    )
  
  return(list(plot = p, data = data_filtered))
}

# Function to load data and create plot
load_data_and_plot <- function(csv_file = NULL) {
  
  # File selection if not provided
  if (is.null(csv_file)) {
    csv_file <- file.choose()
    cat("Selected file:", csv_file, "\n")
  }
  
  # Read the CSV file
  data <- read.csv(csv_file, stringsAsFactors = FALSE)
  cat("Loaded", nrow(data), "pathways\n")
  
  # Check for required columns
  required_cols <- c("FDR", "nGenes", "Fold enriched", "Pathway")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    # Try alternative column names
    if ("Fold.enriched" %in% colnames(data)) {
      names(data)[names(data) == "Fold.enriched"] <- "Fold enriched"
    }
    missing_cols <- setdiff(required_cols, colnames(data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
  }
  
  # Create plot
  result <- create_horizontal_pathway_plot_v2(data, top_n = 20, fdr_threshold = 0.05)
  
  # Display plot
  print(result$plot)
  
  return(result)
}

# Example usage:
# result <- load_data_and_plot()  # Interactive file selection
# result <- load_data_and_plot("your_pathway_data.csv")  # Direct file path

# To save the plot:
# ggsave("pathway_enrichment_plot.png", result$plot, width = 12, height = 8, dpi = 300)
# ggsave("pathway_enrichment_plot.pdf", result$plot, width = 12, height = 8)
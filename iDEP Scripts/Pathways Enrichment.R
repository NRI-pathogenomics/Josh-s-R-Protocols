# GO Pathway Enrichment Bar Plot Generator
# Replicates iDEA 2.0 style bar plots for DESeq2 + GO BP enrichment results

# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)
library(stringr)

# Function to create pathway enrichment bar plots
create_pathway_barplot <- function(csv_file = NULL, 
                                   top_n = 20,
                                   p_value_col = "p.adjust",
                                   pathway_col = "Description",
                                   gene_ratio_col = "GeneRatio",
                                   count_col = "Count",
                                   p_threshold = 0.05,
                                   color_by = "p.adjust",
                                   wrap_length = 50,
                                   title = "Enriched GO Biological Processes") {
  
  # File selection if not provided
  if (is.null(csv_file)) {
    csv_file <- file.choose()
    cat("Selected file:", csv_file, "\n")
  }
  
  # Read the CSV file
  tryCatch({
    data <- read.csv(csv_file, stringsAsFactors = FALSE)
    cat("Successfully loaded", nrow(data), "pathways from CSV file\n")
  }, error = function(e) {
    stop("Error reading CSV file: ", e$message)
  })
  
  # Check for required columns
  required_cols <- c(pathway_col, p_value_col)
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Filter significant pathways
  data_filtered <- data %>%
    filter(!!sym(p_value_col) < p_threshold) %>%
    arrange(!!sym(p_value_col))
  
  if (nrow(data_filtered) == 0) {
    stop("No significant pathways found with p-value < ", p_threshold)
  }
  
  # Select top N pathways
  if (nrow(data_filtered) > top_n) {
    data_filtered <- data_filtered[1:top_n, ]
  }
  
  # Process gene ratio if available
  if (gene_ratio_col %in% colnames(data_filtered)) {
    data_filtered$gene_ratio_numeric <- sapply(data_filtered[[gene_ratio_col]], function(x) {
      if (grepl("/", x)) {
        parts <- strsplit(as.character(x), "/")[[1]]
        return(as.numeric(parts[1]) / as.numeric(parts[2]))
      } else {
        return(as.numeric(x))
      }
    })
  } else {
    data_filtered$gene_ratio_numeric <- 1
  }
  
  # Use count column if available, otherwise use gene ratio
  if (count_col %in% colnames(data_filtered)) {
    data_filtered$plot_size <- data_filtered[[count_col]]
    size_label <- "Gene Count"
  } else {
    data_filtered$plot_size <- data_filtered$gene_ratio_numeric * 100
    size_label <- "Gene Ratio"
  }
  
  # Wrap long pathway names
  data_filtered$pathway_wrapped <- str_wrap(data_filtered[[pathway_col]], width = wrap_length)
  
  # Reorder pathways by significance (most significant at top)
  data_filtered$pathway_wrapped <- factor(data_filtered$pathway_wrapped, 
                                          levels = rev(data_filtered$pathway_wrapped))
  
  # Calculate -log10(p-value) for color scale
  data_filtered$neg_log10_p <- -log10(data_filtered[[p_value_col]])
  
  # Create the plot
  p <- ggplot(data_filtered, aes(x = pathway_wrapped, y = plot_size)) +
    geom_col(aes(fill = neg_log10_p), width = 0.7, color = "white", size = 0.2) +
    coord_flip() +
    scale_fill_gradient(low = "#FED976", high = "#E31A1C", 
                        name = expression(-log[10](italic(p)))) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10, color = "black"),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    labs(
      title = title,
      x = "GO Biological Processes",
      y = size_label,
      caption = paste("Top", nrow(data_filtered), "enriched pathways (p.adjust <", p_threshold, ")")
    )
  
  return(list(plot = p, data = data_filtered))
}

# Alternative function for dot plot style (iDEA 2.0 alternative visualization)
create_pathway_dotplot <- function(csv_file = NULL,
                                   top_n = 20,
                                   p_value_col = "p.adjust",
                                   pathway_col = "Description",
                                   gene_ratio_col = "GeneRatio",
                                   count_col = "Count",
                                   p_threshold = 0.05,
                                   wrap_length = 50,
                                   title = "Enriched GO Biological Processes") {
  
  # File selection if not provided
  if (is.null(csv_file)) {
    csv_file <- file.choose()
    cat("Selected file:", csv_file, "\n")
  }
  
  # Read and process data (similar to bar plot function)
  data <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  data_filtered <- data %>%
    filter(!!sym(p_value_col) < p_threshold) %>%
    arrange(!!sym(p_value_col))
  
  if (nrow(data_filtered) > top_n) {
    data_filtered <- data_filtered[1:top_n, ]
  }
  
  # Process gene ratio
  if (gene_ratio_col %in% colnames(data_filtered)) {
    data_filtered$gene_ratio_numeric <- sapply(data_filtered[[gene_ratio_col]], function(x) {
      if (grepl("/", x)) {
        parts <- strsplit(as.character(x), "/")[[1]]
        return(as.numeric(parts[1]) / as.numeric(parts[2]))
      } else {
        return(as.numeric(x))
      }
    })
  }
  
  # Use count for dot size
  if (count_col %in% colnames(data_filtered)) {
    data_filtered$dot_size <- data_filtered[[count_col]]
  } else {
    data_filtered$dot_size <- data_filtered$gene_ratio_numeric * 20
  }
  
  # Wrap pathway names
  data_filtered$pathway_wrapped <- str_wrap(data_filtered[[pathway_col]], width = wrap_length)
  data_filtered$pathway_wrapped <- factor(data_filtered$pathway_wrapped, 
                                          levels = data_filtered$pathway_wrapped)
  
  # Calculate -log10(p-value)
  data_filtered$neg_log10_p <- -log10(data_filtered[[p_value_col]])
  
  # Create dot plot
  p <- ggplot(data_filtered, aes(x = gene_ratio_numeric, y = pathway_wrapped)) +
    geom_point(aes(size = dot_size, color = neg_log10_p)) +
    scale_color_gradient(low = "#FED976", high = "#E31A1C", 
                         name = expression(-log[10](italic(p)))) +
    scale_size_continuous(name = "Gene Count", range = c(3, 10)) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_line(color = "grey90", size = 0.3),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 11, face = "bold")
    ) +
    labs(
      title = title,
      x = "Gene Ratio",
      y = "GO Biological Processes",
      caption = paste("Top", nrow(data_filtered), "enriched pathways (p.adjust <", p_threshold, ")")
    )
  
  return(list(plot = p, data = data_filtered))
}

# Example usage:
# Interactive file selection and plot generation
generate_pathway_plots <- function() {
  cat("Select your enriched pathways CSV file...\n")
  
  # Create bar plot
  result_bar <- create_pathway_barplot(
    top_n = 20,
    p_threshold = 0.05,
    wrap_length = 45,
    title = "Top Enriched GO Biological Processes"
  )
  
  # Display the plot
  print(result_bar$plot)
  
  # Optionally create dot plot with same data
  cat("\nWould you like to also create a dot plot? (y/n): ")
  response <- readline()
  
  if (tolower(response) %in% c("y", "yes")) {
    result_dot <- create_pathway_dotplot(
      csv_file = attr(result_bar, "file_path"),
      top_n = 20,
      p_threshold = 0.05,
      wrap_length = 45,
      title = "Top Enriched GO Biological Processes"
    )
    print(result_dot$plot)
  }
  
  return(result_bar)
}

# Simple one-liner to generate plots
# Uncomment the line below to run interactively:
# result <- generate_pathway_plots()

# Or specify file directly:
# result <- create_pathway_barplot("path/to/your/enriched_pathways.csv")
# print(result$plot)
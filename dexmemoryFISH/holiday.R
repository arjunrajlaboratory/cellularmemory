
library(ggplot2)
library(dplyr)
library(tidyr)

inputdirectory <- "/Users/jess/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/dexmemfish-altControls/holiday"
outputdirectory <- "/Users/jess/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/dexmemfish-altControls/holiday"


# Function to safely convert to numeric
convert_to_numeric <- function(df, cols_to_convert) {
  existing_cols <- intersect(cols_to_convert, names(df))
  df[existing_cols] <- lapply(df[existing_cols], function(x) as.numeric(as.character(x)))
  return(df)
}

# Function to extract identifier from filename
extract_identifier <- function(filename) {
  sub(".*-(.*)\\.csv$", "\\1", basename(filename))
}

  
  # Columns to convert
  cols_to_convert <- c("a594", "cy5")
  
  extract_identifier <- function(filename) {
    # Extracting everything after the last "-" and before the ".csv"
    sub(".*-(.*)\\.csv$", "\\1", basename(filename))
  }
  
  # Step 1: Import Data, Convert Types, and Add Identifier
  files <- list.files(path = inputdirectory, pattern = "*.csv", full.names = TRUE)
  data_list <- lapply(files, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df <- convert_to_numeric(df, cols_to_convert)
    df$file_identifier <- extract_identifier(f) # Adding the identifier
    return(df)
  })
  
  
  # Combining all data into one dataframe
  all_data <- bind_rows(data_list, .id = "file_id")
  
  # Reshape the data for plotting
  melted_data_filtered <- all_data %>%
    dplyr::select(a594, cy5, file_identifier) %>%
    gather(key = "variable", value = "value", -file_identifier) %>%
    dplyr::filter(variable %in% c("a594", "cy5"))
  
  # Convert the 'value' column to numeric and handle conversion issues
  melted_data_filtered$value <- as.numeric(as.character(melted_data_filtered$value))
  
  # Omit NA values if any exist after conversion
  melted_data_filtered <- na.omit(melted_data_filtered)
  
  
  #save plots
  rawcounts_plot <- ggplot(melted_data_filtered, aes(x = file_identifier, y = value, fill = variable)) +
    geom_boxplot(outliers = FALSE, aes(fill = variable), alpha = 0.5) +  # Exclude outliers
   # geom_jitter(aes(color = variable), width = 0.3, alpha = 0.7) + 
    facet_wrap(~ variable, scales = "free", ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = "Box Plot Comparison of cy3, a594, cy5, cy7 Across Files",
         x = "Variable",
         y = "Value")+
    scale_fill_brewer(palette = "Set3")
  
  rawcounts_plot
  
  
  # Save the plot as a PDF
  ggsave(filename = paste0(outputdirectory, "/countspernuclei.pdf"), plot = rawcounts_plot, width = 6, height = 12, device = "pdf")

  
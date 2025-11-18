library(ggplot2)
library(dplyr)
library(tidyr)


inputdirectories <- c("/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/dexmemfish-altControls/dex2dexjnk1/rep3", 
                      "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/dexmemfish-altControls/dex2dexjnk1/rep6"
)

outputdirectory <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/dexmemfish-altControls/dex2dexjnk1"



# Function to safely convert to numeric
convert_to_numeric <- function(df, cols_to_convert) {
  existing_cols <- intersect(cols_to_convert, names(df))
  df[existing_cols] <- lapply(df[existing_cols], function(x) as.numeric(as.character(x)))
  return(df)
}

# Function to process and plot data from multiple input directories
  
  # Columns to convert
  cols_to_convert <- c("cy3", "a594", "cy5", "cy7","cell.Blob.Blob.metrics...Area","cell.Blob.Blob.metrics...Perimeter","cell.Blob.Blob.metrics...Centroid...y","cell.Blob.Blob.metrics...Centroid...x")
  
  # Modified function to extract numeric identifier before the underscore
  extract_first_number_after_dash <- function(filename) {
    # Extracting the first number after the "-"
    sub(".*-([0-9]+).*\\.csv$", "\\1", basename(filename))
  }
  
  
  # Initialize an empty list to store data from all directories
  all_data_list <- list()
  
  # Adding a unique dataset identifier for each input directory
  dataset_id <- 1
  
  # Iterate over each input directory and append data
  for(inputdirectory in inputdirectories) {
    files <- list.files(path = inputdirectory, pattern = "*.csv", full.names = TRUE)
    data_list <- lapply(files, function(f) {
      df <- read.csv(f, stringsAsFactors = FALSE)
      
      # Check if the file is from rep7/all folder
      if (grepl("rep7", f)) {
        # Rename columns
        names(df)[names(df) == "a594"] <- "temp_col"
        names(df)[names(df) == "cy5"] <- "a594"
        names(df)[names(df) == "temp_col"] <- "cy5"
      }
      
      df <- convert_to_numeric(df, cols_to_convert)
      df$sample_identifier <- extract_first_number_after_dash(f)
      df$dataset_identifier <- paste("Dataset", dataset_id)
      return(df)
    })
    all_data_list <- c(all_data_list, data_list)
    dataset_id <- dataset_id + 1
  }
  
  
  # Combining all data into one dataframe
  all_data <- bind_rows(all_data_list, .id = "file_id")
  
  
  # Normalize cy3, a594, and cy5 by cy7
  all_data <- all_data %>%
    dplyr::mutate(cy3_norm = cy3 / cy7,
                  a594_norm = a594 / cy7,
                  cy5_norm = cy5 / cy7)
 
  #normalize by area
  all_data <- all_data %>%
    dplyr::mutate(micron2 = `cell.Blob.Blob.metrics...Area` * 0.0467) %>%
    dplyr::mutate(cy3_areanorm_RNApermicron2 = cy3 / `micron2`,
                  a594_areanorm_RNApermicron2 = a594 / `micron2`,
                  cy5_areanorm_RNApermicron2 = cy5 / `micron2`)
  
  
  
  
  # Calculate the mean for each area-normalized variable, sample, and dataset
  mean_areanorm_data <- all_data %>%
    group_by(dataset_identifier, sample_identifier) %>%
    dplyr::summarise(
      cy3_mean = mean(cy3_areanorm_RNApermicron2, na.rm = TRUE),
      cy3_se = sd(cy3_areanorm_RNApermicron2, na.rm = TRUE) / sqrt(n()),
      a594_mean = mean(a594_areanorm_RNApermicron2, na.rm = TRUE),
      a594_se = sd(a594_areanorm_RNApermicron2, na.rm = TRUE) / sqrt(n()),
      cy5_mean = mean(cy5_areanorm_RNApermicron2, na.rm = TRUE),
      cy5_se = sd(cy5_areanorm_RNApermicron2, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
      pivot_longer(
      cols = contains("_mean") | contains("_se"),
      names_to = c(".value", "variable"),
      names_sep = "_"
    )
  
  
  # Split the data into means and SEs
  mean_data <- mean_areanorm_data %>%
    filter(variable == "mean") %>%
    dplyr::select(-variable)
  
  se_data <- mean_areanorm_data %>%
    filter(variable == "se") %>%
    dplyr::select(-variable)
  
  # Join means and SEs
  analyte_data <- full_join(mean_data, se_data, by = c("dataset_identifier", "sample_identifier"), suffix = c("_mean", "_se"))
  
  # Pivot longer to have one row per analyte per dataset_identifier per sample_identifier
  long_data <- analyte_data %>%
    pivot_longer(cols = -c(dataset_identifier, sample_identifier), names_pattern = "(.*)_(mean|se)", names_to = c("analyte", ".value"))
  
  
processed_data <- long_data
  
overall_means <- processed_data %>%
    group_by(sample_identifier, analyte) %>%
    summarise(overall_mean = mean(mean, na.rm = TRUE), .groups = 'drop')

dodge_width = .7
  

  # Creating the plot with individual experiment bars and error bars
plot <- ggplot() +
  # Add individual experiment bars with dodging
  geom_bar(data = processed_data, aes(x = sample_identifier, y = mean, fill = dataset_identifier, group = interaction(sample_identifier, dataset_identifier, analyte)), 
           stat = "identity", position = position_dodge(width = dodge_width), width = 0.6) +
  # Add error bars with dodging, aligned with individual bars
  geom_errorbar(data = processed_data, aes(x = sample_identifier, ymin = mean - se, ymax = mean + se, group = interaction(sample_identifier, dataset_identifier, analyte)),
                position = position_dodge(width = dodge_width), width = 0.25) +
  # Overlay overall mean bars as translucent with a wider dodge to cover all individual bars
  geom_bar(data = overall_means, aes(x = sample_identifier, y = overall_mean, fill = analyte, group = sample_identifier), 
           stat = "identity", position = position_dodge(width = dodge_width * 1.2), width = dodge_width * 1.1, alpha = 0.5, color = "black", show.legend = FALSE) +
  facet_wrap(~analyte, scales = "free_y") +
  theme_minimal() +
  labs(title = "Mean Values with SE and Overall Mean for Each Condition", x = "Sample Identifier", y = "Mean Value") +
  scale_fill_brewer(palette = "Set3")

  print(plot)
  
 

  ggsave(filename = paste0(outputdirectory, "/jnkpre.pdf"), plot = plot, width = 12, height = 8, device = "pdf")
  
  
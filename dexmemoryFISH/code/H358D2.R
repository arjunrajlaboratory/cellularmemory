
library(ggplot2)
library(dplyr)
library(tidyr)

inputdirectory <- "/Users/jess/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/dexmemfish/H358D2"
outputdirectory <- "/Users/jess/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/dexmemfish/H358D2"


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
cols_to_convert <- c("cy3.children", "a594.children", "cy5.children", "cy7.children")

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
  dplyr::select(cy3.children, a594.children, cy5.children, cy7.children, file_identifier) %>%
  gather(key = "variable", value = "value", -file_identifier) %>%
  dplyr::filter(variable %in% c("cy3.children", "a594.children", "cy5.children", "cy7.children"))

# Convert the 'value' column to numeric and handle conversion issues
melted_data_filtered$value <- as.numeric(as.character(melted_data_filtered$value))

# Omit NA values if any exist after conversion
melted_data_filtered <- na.omit(melted_data_filtered)


#save plots
rawcounts_plot <- ggplot(melted_data_filtered, aes(x = file_identifier, y = value, fill = variable)) +
  geom_boxplot(outlier.shape = NA, aes(fill = variable), alpha = 0.5) +  # Exclude outliers
  # geom_jitter(aes(color = variable), width = 0.3, alpha = 0.7) + 
  facet_wrap(~ variable, scales = "free", ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Box Plot Comparison of cy3, a594, cy5, cy7 Across Files",
       x = "Variable",
       y = "Value")+
  scale_fill_brewer(palette = "Set3")
  #+ ylim(0,50)
  
  rawcounts_plot


# Save the plot as a PDF
ggsave(filename = paste0(outputdirectory, "/countspernuclei.pdf"), plot = rawcounts_plot, width = 6, height = 12, device = "pdf")




rawcounts_barplot <- ggplot(melted_data_filtered, 
                         aes(x = file_identifier, y = value, fill = variable)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", alpha = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.9), width = 0.2) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +  # Add individual points
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Mean Values with Individual Points - cy3, a594, cy5, cy7",
       x = "File",
       y = "Value") +
  scale_fill_brewer(palette = "Set3")

rawcounts_barplot

ggplot(melted_data_filtered, aes(x = file_identifier, y = value, fill = variable)) +
  geom_violin(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Distribution of Values by File",
       x = "File",
       y = "Value") +
  scale_fill_brewer(palette = "Set3")

rawcounts_plot <- ggplot(melted_data_filtered, 
                         aes(x = file_identifier, y = value, fill = variable)) +
  geom_boxplot(outlier.shape = NA, 
               coef = 0,  # This removes the whiskers
               alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Box Plot Comparison of cy3, a594, cy5, cy7 Across Files",
       x = "Variable",
       y = "Value") +
  scale_fill_brewer(palette = "Set3") +
  ylim(0,50)


rawcounts_plot


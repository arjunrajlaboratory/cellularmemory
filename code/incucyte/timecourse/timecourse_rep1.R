
# Load necessary libraries
library(tidyverse)
library(dplyr)

# Set the path to the folder containing .tif files
folder_path_1 <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/incucyte/trametinibtimecourse/rep1" # Replace with your folder path

# List all .tif files in the folder
file_names <- list.files(folder_path_1, pattern = "\\.tif$", full.names = TRUE)

# Extract only the file names, without the full path
file_names <- basename(file_names)

# Split file names at underscores
file_info_list <- strsplit(file_names, split = "_")

# Find the maximum number of parts in any file name
max_length <- max(sapply(file_info_list, length))

# Pad each list element with NA to ensure equal length
file_info_list <- lapply(file_info_list, function(x) {
  length(x) <- max_length
  x
})

# Convert the list to a data frame
file_info <- do.call(rbind, file_info_list)
colnames(file_info) <- paste0("Column", seq_len(max_length))

# Convert to tibble for a nicer printout (optional)
file_info <- data.frame(file_info)




# Assuming 'file_info' is your data frame and it's already loaded
# Convert Column2 to numeric if it's not already
file_info$Column2 <- as.numeric(as.character(file_info$Column2))


file_info <- file_info %>% 
  mutate(UniqueID = row_number())

ordered_names <- c("tlo",  "thi", "d1", "d3",  "d5",  "d7", "d12", "d14","d16","d21", "d28")

# Match UniqueID with the order in Column1
file_info$OrderedID <- factor(file_info$UniqueID, levels = file_info$UniqueID[order(match(file_info$Column1, ordered_names))])

file_info$Group <- ifelse(file_info$Column1 %in% c("tlo", "thi"), "control", "timecourse")

# Create the bar graph with faceting based on the new 'Group' column
timecourseplot <- ggplot(file_info, aes(x = OrderedID, y = Column2, fill = Column1)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Group) +
  theme_minimal() +
  labs(title = "trametinib lo-to-hi timecourse",
       x = "Ordered Sample Identifier",
       y = "colony count 2 weeks after high-dose challenge") +
  scale_fill_discrete(name = "day of high-dose challenge")

timecourseplot

pdf("/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/incucyte/timecourse/timecourserep1_colonycount.pdf", width=10, height=7)
plot(timecourseplot)
dev.off()






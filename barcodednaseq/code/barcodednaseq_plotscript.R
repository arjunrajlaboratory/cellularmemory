# Load necessary libraries
library(tidyverse)

# Set the path where your files are stored
file_path <- "/Users/jess/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/barcodednaseq/20230726"
output_directory <- "/Users/jess/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/barcodednaseq/20230726"


# List all text files in the directory
# Read and combine files with an identifier for each file
files <- list.files(file_path, pattern = "*.txt", full.names = TRUE)
data_list <- lapply(files, function(file) {
  read.delim(file) %>%
    mutate(Experiment = tools::file_path_sans_ext(basename(file))) # Add file name as an experiment identifier
})
data <- bind_rows(data_list)

 
filtered_data <- data %>%
  filter(Name %in% c("c_bc30", "b_bc30", "d_bc30")) %>%
  mutate(Name = factor(Name, levels = c("b_bc30", "c_bc30", "d_bc30")))

# Plot all in one with groups
# Define the plot with adjusted bar spacing
plot <- ggplot(filtered_data, aes(x = Name, y = Length, fill = Experiment)) +
  geom_bar(stat = "identity", position = position_dodge(width = .8), width = 0.7) +  # Adjust dodge width
  labs(title = "Length Comparison Across Experiments", x = "Category", y = "Length") +
  theme_minimal()

# Display the plot
plot

# Filename for the plot
filename <- "Grouped_Bar_Graph.pdf"

# Save the plot
ggsave(filename = file.path(output_directory, filename), plot = plot, width = 10, height = 6)


library(nlme)
library(emmeans)
library(car)


# Fit mixed-effects model with Experiment as random effect
model <- lme(Length ~ Name, random = ~ 1 | Experiment, data = filtered_data)

# Display model summary
summary(model)

# Check model assumptions
plot(model)  # Residual plots
qqnorm(residuals(model, type = "pearson"))
qqline(residuals(model, type = "pearson"))
# Check for normality of residuals
residuals <- residuals(model)
shapiro.test(residuals)

# Check for homogeneity of variances
leveneTest(Length ~ Name, data = filtered_data)

# Perform pairwise comparisons using emmeans
emmeans_results <- emmeans(model, pairwise ~ Name, adjust = "holm")
print(emmeans_results)



data_wide <- spread(filtered_data, Name, Length)
print(data_wide)

conditions <- unique(filtered_data$Name)
num_conditions <- length(conditions)

p_values <- matrix(NA, nrow = num_conditions, ncol = num_conditions)
rownames(p_values) <- colnames(p_values) <- conditions

for (i in 1:(num_conditions - 1)) {
  for (j in (i + 1):num_conditions) {
    cond_i <- conditions[i]
    cond_j <- conditions[j]
    
    data_i <- filtered_data$Length[filtered_data$Name == cond_i]
    data_j <- filtered_data$Length[filtered_data$Name == cond_j]
    
    test_result <- t.test(data_i, data_j, paired = TRUE)
    p_values[i, j] <- p_values[j, i] <- test_result$p.value
  }
}



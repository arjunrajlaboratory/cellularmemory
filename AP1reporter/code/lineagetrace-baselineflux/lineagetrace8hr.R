

library(viridis)
library(tidyverse)
library(data.table)
library(scales)
library(dplyr)
library(gridExtra)


# Load the data
data <- fread("/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/ap1reporter/baselineflux/6d_VID2072_A1_baselineflux.csv")

data <- data %>%
  mutate(
    Normalized_Red = `red intensity measurements / MedianIntensity` - 
      `red Annulus intensity measurements / MedianIntensity`,
    Normalized_Green = `green intensity measurements / MedianIntensity` - 
      `green Annulus intensity measurements / MedianIntensity`,
    parentID = `Parent and child / parentId`,
    childID = `Parent and child / childId`,
    annotationID = `Parent and child / annotationId`,
    redmean = `red intensity measurements / MeanIntensity`,
    greenmean = `green intensity measurements / MeanIntensity`
  )

dataOG <- data %>% dplyr::select(parentID, childID, annotationID)
dataNew <- data %>% dplyr::select(annotationID, parentID_1 = parentID)

dataNew <- dataNew[annotationID != parentID_1]


dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2 = parentID), on = .(parentID_1 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3 = parentID), on = .(parentID_2 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4 = parentID), on = .(parentID_3 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5 = parentID), on = .(parentID_4 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6 = parentID), on = .(parentID_5 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7 = parentID), on = .(parentID_6 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7, parentID_8 = parentID), 
                   on = .(parentID_7 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7, parentID_8, parentID_9 = parentID), 
                   on = .(parentID_8 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7, parentID_8, parentID_9, parentID_10 = parentID), 
                   on = .(parentID_9 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7, parentID_8, parentID_9, parentID_10, parentID_11 = parentID), 
                   on = .(parentID_10 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7, parentID_8, parentID_9, parentID_10, parentID_11, parentID_12 = parentID), 
                   on = .(parentID_11 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7, parentID_8, parentID_9, parentID_10, parentID_11, parentID_12, parentID_13 = parentID), 
                   on = .(parentID_12 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7, parentID_8, parentID_9, parentID_10, parentID_11, parentID_12, parentID_13, parentID_14 = parentID), 
                   on = .(parentID_13 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7, parentID_8, parentID_9, parentID_10, parentID_11, parentID_12, parentID_13, parentID_14, parentID_15 = parentID), 
                   on = .(parentID_14 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7,  parentID_8, parentID_9, parentID_10, parentID_11, parentID_12, parentID_13, parentID_14, parentID_15, parentID_16 = parentID), 
                   on = .(parentID_15 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7,  parentID_8, 
                             parentID_9, parentID_10, parentID_11, parentID_12, parentID_13, parentID_14, parentID_15, parentID_16, parentID_17 = parentID), 
                   on = .(parentID_16 = annotationID), nomatch = 0L, allow.cartesian=TRUE]
dataNew <- dataNew[dataOG, .(annotationID, parentID_1, parentID_2, parentID_3, parentID_4, parentID_5, parentID_6, parentID_7,  parentID_8, 
                             parentID_9, parentID_10, parentID_11, parentID_12, parentID_13, parentID_14, parentID_15, parentID_16, parentID_17, parentID_18 = parentID), 
                   on = .(parentID_17 = annotationID), nomatch = 0L, allow.cartesian=TRUE]



data_numeric <- data.frame(lapply(dataNew, function(x) as.numeric(x)))
lineagedata <- na.omit(data_numeric)
lineagedata <- data.table(lineagedata)
lineagedata[, lineage := .I]


# Number of columns
num_columns <- ncol(lineagedata)

# Generate new column names from 19 to 1
new_col_names <- c(as.character(19:(19 - num_columns + 2)), "lineage")

# Rename columns
setnames(lineagedata, old = colnames(lineagedata), new = new_col_names)

#gather data
lineagedata_long <- lineagedata %>%
  pivot_longer(
    cols = -lineage,  # Columns to make longer, except 'id'
    names_to = "generation",  # New column for the original column names
    values_to = "annotationID"  # New column for the values
  )

#merge data
filteredData <- data %>% dplyr::select(annotationID, Normalized_Green, Normalized_Red)
filteredData$annotationID <- as.numeric(filteredData$annotationID)

lineagedata_long$annotationID <- as.numeric(lineagedata_long$annotationID)
lineagedata_long <- left_join(lineagedata_long, filteredData, by = "annotationID")

lineagedata_long$lineage <- factor(lineagedata_long$lineage)
levels(lineagedata_long$generation) <- c( "1", "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9",  "10", "11", "12", "13", "14", "15", "16", "17", "18", "19")
lineagedata_long$hour <- as.numeric(lineagedata_long$generation)*1-1




#plotdata from one lineage
# Plot using ggplot2 with facets
# Plot using ggplot2
greencolors <- viridis(length(lineagedata$lineage), option = "D")
greenplot <- ggplot(lineagedata_long, aes(x = hour, y = Normalized_Green, group = lineage)) +
  geom_line(aes(color = lineage), alpha = 0.5) +  # Set transparency with alpha
  geom_point(aes(color = lineage), size = 1, alpha = 0.5) +
  scale_color_manual(values = greencolors) +
  labs(title = "Red vs. Generation for Each Lineage",
       x = "hour",
       y = "Red",
       color = "Lineage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

redcolors <- viridis(length(lineagedata$lineage), option = "B")
redplot <- ggplot(lineagedata_long, aes(x = hour, y = Normalized_Red, group = lineage)) +
  geom_line(aes(color = lineage), alpha = 0.5) +  # Set transparency with alpha
  geom_point(aes(color = lineage), size = 1, alpha = 0.5) +
  scale_color_manual(values = redcolors) +
  labs(title = "Red vs. Generation for Each Lineage",
       x = "Generation",
       y = "Red",
       color = "Lineage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

greenplot
redplot

output_path_green <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/ap1reporter/baselineflux/greenplot.pdf"
output_path_red <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/ap1reporter/baselineflux/redplot.pdf"
ggsave(output_path_red, plot = redplot, width = 12, height = 8, dpi = 300, useDingbats = F)
ggsave(output_path_green, plot = greenplot, width = 12, height = 8, dpi = 300, useDingbats = F)



library(pracma)
library(reshape2)

datacor <- lineagedata_long[,c(1,4:6)]
datacor <- melt(datacor, id.vars = c("lineage", "hour"), 
                  variable.name = "channel", value.name = "value")


# Ensure datacor is a data.table
setDT(datacor)

# View the melted data structure
print(datacor)

# Define the calculate_autocorrelation function
calculate_autocorrelation <- function(values) {
  acf_result <- acf(values, plot = FALSE)
  return(acf_result$acf)
}

# Calculate autocorrelation for each combination of lineage and channel
autocorrelation_results <- datacor[, .(autocorrelation = list(calculate_autocorrelation(value))), by = .(lineage, channel)]


calculate_crosscorrelation <- function(values1, values2) {
  ccf_result <- ccf(values1, values2, plot = FALSE)
  return(ccf_result$acf)
}

crosscorrelation_results <- datacor[channel == "Normalized_Red"][datacor[channel == "Normalized_Green"], on = .(lineage, hour)]
crosscorrelation_results <- crosscorrelation_results[, .(crosscorrelation = list(calculate_crosscorrelation(value, i.value))), by = lineage]

# Example of plotting autocorrelation for one cell in one channel
autocorr_example <- autocorrelation_results[lineage == 1 & channel == "Normalized_Red"]$autocorrelation[[1]]
ggplot(data.frame(lag = 1:length(autocorr_example), autocorr = autocorr_example), aes(x = lag, y = autocorr)) +
  geom_line() + ggtitle("Autocorrelation for Cell 1, Channel 1")

# Example of plotting cross-correlation for one cell
crosscorr_example <- crosscorrelation_results[lineage == 1]$crosscorrelation[[1]]
ggplot(data.frame(lag = 1:length(crosscorr_example), crosscorr = crosscorr_example), aes(x = lag, y = crosscorr)) +
  geom_line() + ggtitle("Cross-Correlation for Cell 1")


# Average autocorrelation
green_autocor_results <- dplyr::filter(autocorrelation_results, channel == "Normalized_Green")
red_autocor_results <- dplyr::filter(autocorrelation_results, channel == "Normalized_Red")

green_average_autocorrelation <- Reduce("+", green_autocor_results$autocorrelation) / length(green_autocor_results$autocorrelation)
red_average_autocorrelation <- Reduce("+", red_autocor_results$autocorrelation) / length(red_autocor_results$autocorrelation)

# Average cross-correlation
average_crosscorrelation <- Reduce("+", crosscorrelation_results$crosscorrelation) / length(crosscorrelation_results$crosscorrelation)


# Convert to data frames for plotting
green_autocorr_df <- data.frame(lag = 0:(length(green_average_autocorrelation) - 1), correlation = green_average_autocorrelation)
red_autocorr_df <- data.frame(lag = 0:(length(red_average_autocorrelation) - 1), correlation = red_average_autocorrelation)

# Plot overall autocorrelation
green_autocorr_plot <- ggplot(green_autocorr_df, aes(x = lag, y = correlation)) +
  geom_line() +
  labs(title = "overall green autocorrelation", x = "Lag", y = "Correlation") +
  theme_minimal()

green_autocorr_plot

red_autocorr_plot <- ggplot(red_autocorr_df, aes(x = lag, y = correlation)) +
  geom_line() +
  labs(title = "overall red autocorrelation", x = "Lag", y = "Correlation") +
  theme_minimal()

red_autocorr_plot

# Calculate the correct lag sequence for cross-correlation
lag_max <- (length(average_crosscorrelation) - 1) / 2
lags <- -lag_max:lag_max
crosscorr_df <- data.frame(lag = lags, correlation = average_crosscorrelation)

# Plot overall cross-correlation
crosscorplot <- ggplot(crosscorr_df, aes(x = lag, y = correlation)) +
  geom_line() +
  labs(title = "Overall Cross-Correlation", x = "Lag", y = "Correlation") +
  theme_minimal() +
  ylim(-0.2,1)
crosscorplot


output_path_greenavg <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/ap1reporter/baselineflux/green_autocorplot.pdf"
output_path_redavg <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/ap1reporter/baselineflux/red_autocorplot.pdf"
output_path_crosscor <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/ap1reporter/baselineflux/crosscorplot.pdf"

ggsave(output_path_redavg, plot = red_autocorr_plot, width = 12, height = 8, dpi = 300, useDingbats = F)
ggsave(output_path_greenavg, plot = green_autocorr_plot, width = 12, height = 8, dpi = 300, useDingbats = F)
ggsave(output_path_crosscor, plot = crosscorplot, width = 12, height = 8, dpi = 300, useDingbats = F)



find_characteristic_time <- function(correlation_values, threshold = exp(-1)) {
  which(correlation_values <= threshold)[1]
}

autocorrelation_times <- autocorrelation_results[, .(characteristic_time = find_characteristic_time(unlist(autocorrelation))), by = .(lineage, channel)]
crosscorrelation_times <- crosscorrelation_results[, .(characteristic_time = find_characteristic_time(unlist(crosscorrelation))), by = lineage]




# individual lineage plots
lineagetest <- dplyr::filter(datacor, lineage == 11)

ggplot(lineagetest, aes(x = hour, y = value, color = channel)) +
  geom_line() +
  labs(x = "X values", y = "Y values", title = "Two Line Plots on the Same Graph") +
  theme_minimal()



selected_datacor <- dplyr::filter(datacor, lineage %in% c(6,9,13,17,18,22,23,24,25,29,30,33))

selected_datacor_plot <- ggplot(selected_datacor, aes(x = hour, y = value, color = channel)) +
  geom_line() +
  facet_wrap(~lineage) +
  labs(x = "X values", y = "Y values", title = "Two Line Plots on the Same Graph") +
  theme_minimal()

selected_datacor_plot

output_path_datacor_plot <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/ap1reporter/baselineflux/datacorplot.pdf"
ggsave(output_path_datacor_plot, plot = selected_datacor_plot, width = 12, height = 8, dpi = 300, useDingbats = F)


#####
# Calculate average characteristic times

average_char_time_green <- mean(autocorrelation_times[channel == "Normalized_Green"]$characteristic_time, na.rm = TRUE)
average_char_time_red <- mean(autocorrelation_times[channel == "Normalized_Red"]$characteristic_time, na.rm = TRUE)

average_char_time_green_hours <- mean(autocorrelation_times[channel == "Normalized_Green"]$characteristic_time, na.rm = TRUE)*8
average_char_time_red_hours <- mean(autocorrelation_times[channel == "Normalized_Red"]$characteristic_time, na.rm = TRUE)*8

average_char_time_cross <- mean(crosscorrelation_times$characteristic_time, na.rm = TRUE)
average_char_time_cross_hours <- mean(crosscorrelation_times$characteristic_time, na.rm = TRUE)*8

#####

datacor <- lineagedata_long[,c(1,4:6)]
datacor <- data.table(datacor)

# Define function to calculate cross-correlation and find tau value
calculate_crosscorrelation_and_tau <- function(values1, values2) {
  ccf_result <- ccf(values1, values2, plot = FALSE, lag.max = 20)
  correlation <- ccf_result$acf
  lags <- ccf_result$lag
  threshold <- exp(-1)
  
  # Tau: Lag at maximum correlation
  tau_index <- which.max(correlation)
  tau_lag <- lags[tau_index]
  
  # Characteristic Time: First lag where correlation drops below threshold
  char_time_index <- which(correlation <= threshold)[1]
  char_time <- if (!is.na(char_time_index)) lags[char_time_index] else NA
  
  return(list(correlation = correlation, lags = lags, tau_lag = tau_lag, char_time = char_time))
}

# Apply the function to each lineage
crosscorrelation_results <- datacor[, {
  result <- calculate_crosscorrelation_and_tau(Normalized_Green, Normalized_Red)
  list(correlation = list(result$correlation), lags = list(result$lags), tau_lag = result$tau_lag)
}, by = lineage]

# Calculate the average cross-correlation
average_crosscorrelation <- rowMeans(do.call(cbind, crosscorrelation_results$correlation))
lags_ccf <- crosscorrelation_results$lags[[1]]

# Calculate the average tau value
average_tau_cross <- mean(crosscorrelation_results$tau_lag, na.rm = TRUE)

# Create data frame for plotting
crosscorr_df <- data.table(lag = lags_ccf, correlation = average_crosscorrelation)

# Plot average cross-correlation between Normalized_Green and Normalized_Red
ggplot(crosscorr_df, aes(x = lag, y = correlation)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = average_tau_cross, linetype = "dashed", color = "red") +
  geom_text(aes(x = average_tau_cross, y = min(correlation), label = paste("Tau =", round(average_tau_cross, 2))), vjust = -1, color = "red") +
  labs(title = "Average Cross-Correlation between Normalized_Green and Normalized_Red", x = "Lag", y = "Correlation") +
  theme_minimal()

# Visualize the Raw Signals with Shift
plot_shifted_signals <- function(datacor, tau_lag) {
  shifted_datacor <- datacor[, .(
    hour = hour,
    Normalized_Green = Normalized_Green,
    Normalized_Red_shifted = shift(Normalized_Red, n = -tau_lag, type = "lead")
  ), by = lineage]
  
  ggplot(shifted_datacor, aes(x = hour)) +
    geom_line(aes(y = Normalized_Green, color = "Normalized_Green")) +
    geom_line(aes(y = Normalized_Red_shifted, color = "Normalized_Red_shifted")) +
    facet_wrap(~ lineage) +
    labs(title = paste("Signals with Shift (Tau =", tau_lag, ")"), x = "Hour", y = "Value") +
    theme_minimal() +
    scale_color_manual(name = "Signal", values = c("Normalized_Green" = "green", "Normalized_Red_shifted" = "red"))
}

# Extract tau lag value (example for lineage 1)
tau_lag <- crosscorrelation_results[lineage == 1]$tau_lag
plot_shifted_signals(datacor, tau_lag)


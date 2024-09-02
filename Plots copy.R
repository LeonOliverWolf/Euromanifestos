library(stm)
library(ggplot2)
library(dplyr)
# Load results

result <- readRDS("~/Desktop/Results searchK/searchK_result3_50.rds")

# Define a function to extract and label metrics
extract_metrics <- function(result) {
  data.frame(
    K = as.integer(result$results$K),
    HeldOutLikelihood = unlist(result$results$heldout),
    Residuals = unlist(result$results$residual),
    SemanticCoherence = unlist(result$results$semcoh),
    LowerBound = unlist(result$results$bound),
    Exclusivity = unlist(result$results$exclus))  # Assuming exclusivity is part of the results
}
result<- extract_metrics(result)

combined_data<-result

#combined_data <- combined_data %>% filter(K %% 2 == 0)

# Plot Held-Out Likelihood
plot_heldout <- ggplot(combined_data, aes(x = K, y = HeldOutLikelihood)) +
  geom_line() +
  geom_point() +
  labs(title = "Held-Out Likelihood vs Number of Topics",
       x = "Number of Topics (K)",
       y = "Held-Out Likelihood") +
  theme_minimal()

# Plot Residuals
plot_residuals <- ggplot(combined_data, aes(x = K, y = Residuals)) +
  geom_line() +
  geom_point() +
  labs(title = "Residuals vs Number of Topics",
       x = "Number of Topics (K)",
       y = "Residuals") +
  theme_minimal()

# Plot Semantic Coherence
plot_semcoh <- ggplot(combined_data, aes(x = K, y = SemanticCoherence)) +
  geom_line() +
  geom_point() +
  labs(title = "Semantic Coherence vs Number of Topics",
       x = "Number of Topics (K)",
       y = "Semantic Coherence") +
  theme_minimal()

# Plot Lower Bound
plot_lowerbound <- ggplot(combined_data, aes(x = K, y = LowerBound)) +
  geom_line() +
  geom_point() +
  labs(title = "Lower Bound vs Number of Topics",
       x = "Number of Topics (K)",
       y = "Lower Bound") +
  theme_minimal()

# Display the plots
print(plot_heldout)
print(plot_residuals)
print(plot_semcoh)
print(plot_lowerbound)

library(ggrepel)
# Plotting the filtered data
plot_semcoh_exclus <- ggplot(combined_data, aes(x = SemanticCoherence, y = Exclusivity, label = K)) +
  geom_point() +
  geom_text_repel(max.overlaps = 50) +
  geom_segment(x = min(combined_data$SemanticCoherence), 
               y = min(combined_data$Exclusivity), 
               xend = max(combined_data$SemanticCoherence), 
               yend = max(combined_data$Exclusivity), 
               linetype = "dashed", color = "red") +  # Adding the line segment
  labs(title = "Semantic Coherence vs Exclusivity (Filtered K values)",
       x = "Semantic Coherence",
       y = "Exclusivity") +
  theme_minimal()

plot_semcoh_exclus

















?searchK

result <- readRDS("~/Desktop/Results searchK/searchK_result8_14.rds")

# Define a function to extract and label metrics
extract_metrics <- function(result) {
  data.frame(
    K = as.integer(result$results$K),
    HeldOutLikelihood = unlist(result$results$heldout),
    Residuals = unlist(result$results$residual),
    SemanticCoherence = unlist(result$results$semcoh),
    LowerBound = unlist(result$results$bound),
    Exclusivity = unlist(result$results$exclus))  # Assuming exclusivity is part of the results
}
result<- extract_metrics(result)

combined_data<-result

library(ggplot2)
# Plot Held-Out Likelihood
plot_heldout <- ggplot(combined_data, aes(x = K, y = HeldOutLikelihood)) +
  geom_line() +
  geom_point() +
  labs(title = "Held-Out Likelihood vs Number of Topics",
       x = "Number of Topics (K)",
       y = "Held-Out Likelihood") +
  theme_minimal()

# Plot Residuals
plot_residuals <- ggplot(combined_data, aes(x = K, y = Residuals)) +
  geom_line() +
  geom_point() +
  labs(title = "Residuals vs Number of Topics",
       x = "Number of Topics (K)",
       y = "Residuals") +
  theme_minimal()

# Plot Semantic Coherence
plot_semcoh <- ggplot(combined_data, aes(x = K, y = SemanticCoherence)) +
  geom_line() +
  geom_point() +
  labs(title = "Semantic Coherence vs Number of Topics",
       x = "Number of Topics (K)",
       y = "Semantic Coherence") +
  theme_minimal()

# Plot Lower Bound
plot_lowerbound <- ggplot(combined_data, aes(x = K, y = LowerBound)) +
  geom_line() +
  geom_point() +
  labs(title = "Lower Bound vs Number of Topics",
       x = "Number of Topics (K)",
       y = "Lower Bound") +
  theme_minimal()

# Display the plots
print(plot_heldout)
print(plot_residuals)
print(plot_semcoh)
print(plot_lowerbound)

# Plotting the filtered data
plot_semcoh_exclus <- ggplot(combined_data, aes(x = SemanticCoherence, y = Exclusivity, label = K)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  geom_segment(x = min(combined_data$SemanticCoherence), 
               y = min(combined_data$Exclusivity), 
               xend = max(combined_data$SemanticCoherence), 
               yend = max(combined_data$Exclusivity), 
               linetype = "dashed", color = "red") +  # Adding the line segment
  labs(title = "Semantic Coherence vs Exclusivity (Filtered K values)",
       x = "Semantic Coherence",
       y = "Exclusivity") +
  theme_minimal()

plot_semcoh_exclus































#Country distribution
country_distribution <- table(meta$country)

# Convert the table to a data frame
country_distribution_df <- as.data.frame(country_distribution)
names(country_distribution_df) <- c("Country", "Count")

# Plot the distribution of countries
ggplot(country_distribution_df, aes(x = reorder(Country, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Country Distribution",
       x = "Country",
       y = "Count") +
  theme_minimal() +
  coord_flip()













#other descriptives
table(meta$group)

# Define total EP seats for each election year
ep_seats_total_lookup <- data.frame(
  year = c(1979, 1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019, 2024),
  EPseats_total_year = c(410, 434, 518, 567, 626, 732, 736, 751, 705, 720)  # Example values; replace with actual
)
meta <- meta %>%
  left_join(ep_seats_total_lookup, by = "year")


meta$word_count <- sapply(strsplit(as.character(meta$Content), "\\s+"), length)

# Aggregate the word counts by group and year
word_count_summary <- meta %>%
  group_by(group, year) %>%
  summarise(total_word_count = sum(word_count), .groups = 'drop')

#count in million
word_count_summary$total_word_count_100k <- word_count_summary$total_word_count / 1e5

library(RColorBrewer)


# Assign specific colors to each group
group_colors <- c(
  "ALDE/Renew" = "#FFD700",  # Gold
  "EPP" = "#6A5ACD",         # Slate Blue
  "ECR" = "#1E90FF",         # Dodger Blue
  "Greens/EFA" = "#32CD32",  # Lime Green
  "GUE/NGL" = "#8B0000",     # Dark Red
  "ID" = "#87CEFA",          # Light Sky Blue
  "S&D" = "#FF4500",         # Orange Red
  "NI" = "#000000"           # Black
)

# Create the plot
plot <- ggplot(word_count_summary, aes(x = year, y = total_word_count_100k, color = group, group = group)) +
  geom_line(size = 1) +           # Adding size to lines for better visibility
  geom_point(size = 2) +          # Adding size to points for better visibility
  labs(title = "Total Number of Words in Documents Over Time by Group",
       x = "Year",
       y = "Total Number of Words in 100k",
       color = "Group") +
  scale_color_manual(values = group_colors) +  # Using the manual color assignment
  theme_minimal() +
  theme(legend.position = "bottom")  # Move legend to the bottom for better layout

plot(plot)
# Save the plot with higher resolution
ggsave("~/Desktop/word_count_plot.png", plot = plot, width = 10, height = 8, dpi = 300) #dpi is resulution


















library(stm)

stm<-readRDS("~/Desktop/stm_spline.rds")


par(mar = c(0.5, 0.5, 0.5, 0.5))  # Adjust these values as needed

# Plot the STM with adjusted margins
plot(stm2, type = "labels", n = 10)

plot(stm, type = "summary", n = 5)

plot(stm)
topic_labels <- labelTopics(stm, n = 10)
print(topic_labels)

plot(stm, n = 4)
  
  

  
topic_correlations <- topicCorr(stm)
topic_correlations
# Inspect the correlation matrix
cor_matrix <- topic_correlations$cor
print(cor_matrix)












# Assume 'stm' is your STM model object
# Assume 'documents' is a character vector of your original texts
# To find top 3 documents for topics 1, 2, and 3:

thoughts <- findThoughts(model = stm, 
                         texts = meta$translated, 
                         topics = 11, 
                         n = 3)

# To view the results:
thoughts





library(wordcloud)
cloud(stm, topic = 1, max.words = 50)










loaded_stm_model<-stm


theta_matrix <- loaded_stm_model$theta
# Get the number of documents and topics
num_documents <- nrow(theta_matrix)
num_topics <- ncol(theta_matrix)

# Create an empty list to store document-topic proportions
document_topic_proportions <- vector("list", length = num_documents)

# Loop through each document and extract topic proportions
for (i in 1:num_documents) {
  document_topic_proportions[[i]] <- data.frame(
    document_id = i,
    topic = 1:num_topics,
    proportion = theta_matrix[i, ]
  )
}

# Combine the list of data frames into a single data frame
document_topic_proportions <- do.call(rbind, document_topic_proportions)


# Print the document-topic proportions using the cat() function
cat("Document ID\tTopic\tProportion\n")  # Print column headers

# Iterate over each row and print the document ID, topic index, and proportion
for (i in 1:nrow(document_topic_proportions)) {
  cat(document_topic_proportions[i, "document_id"], "\t",
      document_topic_proportions[i, "topic"], "\t",
      document_topic_proportions[i, "proportion"], "\n")
}

View(document_topic_proportions)

top_documents_per_topic <- document_topic_proportions %>%
  group_by(topic) %>%
  arrange(desc(proportion)) %>%  # Sort by proportion within each topic
  slice_head(n = 5)  # Select top 5 documents

View(top_documents_per_topic)


meta$document_id <- as.character(meta$X)

# Merge top_documents_per_topic with meta to get the additional information
merged_data <- merge(top_documents_per_topic, meta, by = "document_id")

# Select the relevant columns (document_id, country, and partyname)
relevant_info <- merged_data[, c("document_id", "country", "partyname","topic","proportion", "group", "year")]

View(relevant_info)


table(meta$year)
library(stm)
#stm<-readRDS("~/Desktop/stm.rds")

stm2<-stm
combined_object<-read_rds("~/Desktop/Final.rds")
meta<-combined_object$meta
vocab<-combined_object$vocab
docs<-combined_object$docs

meta <- meta %>%
  mutate(year = case_when(
    year == 1995 ~ 1994,
    year == 1996 ~ 1994,
    year == 1987 ~ 1984,
    
    TRUE ~ year  # Keep other years unchanged
  ))

meta$group <- ifelse(meta$group == "ECR" & meta$year < 2009, "ID", meta$group)
table(meta$group)

meta$group <- ifelse(meta$emcs %in% emcs_group_70, "ID", meta$group)
table(meta$group, meta$year)


group_colors <- c(
  "ALDE/Renew" = "#FFD700",  # Gold
  "EPP" = "#6A5ACD",         # Slate Blue
  "ECR" = "#1E90FF",         # Dodger Blue
  "Greens/EFA" = "#32CD32",  # Lime Green
  "GUE/NGL" = "#8B0000",     # Dark Red
  "ID" = "#87CEFA",          # Light Sky Blue
  "S&D" = "#FF4500",         # Orange Red
  "NI" = "#000000"           # Black
)


meta$EPseats_redefined_factor1<-as.factor(meta$EPseats_redefined_factor1)
meta$group<-as.factor(meta$group)








# Estimate the effect of group, year, and the factor variable on the topics
stm1effect2 <- estimateEffect(formula =~group*year+EPseats_redefined_factor1, stmobj = stm, metadata = meta)

# Extract effect estimates manually for each group
effects <- lapply(names(group_colors), function(group) {
  plot.estimateEffect(
    stm1effect2,
    covariate = "year",
    model = stm,
    topics = 3,  # Topic of interest
    method = "continuous",
    moderator = "group",
    moderator.value = group, 
    add = FALSE,
    omit.plot = TRUE,
    # Skip default plotting
  )
})

# Set up the plot parameters
par(bty = "n", lwd = 2, xaxt = "n")  # Remove box around plot, set line width, and remove x-axis

# Initialize the plot with the first group
plot(effects[[1]]$x, effects[[1]]$means[[1]], type = "l", 
     xlab = "Election Year",
     ylab = "Expected Topic Proportions",
     ylim = c(-0.5, 1),  # Y-axis range
     xlim = c(1979, 2024),
     col = group_colors[names(group_colors)[1]])

# Add lines for the remaining groups
for (i in 2:length(effects)) {
  lines(effects[[i]]$x, effects[[i]]$means[[1]], 
        col = group_colors[names(group_colors)[i]])
}

# Add horizontal reference line
abline(h = 0, lty = 4, lwd = 1, col = "grey45")  # Dotted line at y = 0

# Customize x-axis with specific tick marks
par(xaxt = "s")
axis(1, at = c(1979, 1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019, 2024), 
     labels = c(1979, 1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019, 2024), las = 2)

# Restore default plotting parameters
par(bty = "o", lwd = 1, xaxt = "s")

# Add a custom legend
legend_x <- 2016
legend_y <- 1
legend_spacing <- 0.065
legend_line_width <- 5

# Add lines and text for the legend
for (i in seq_along(group_colors)) {
  # Draw the lines for the legend with the specified line width
  segments(x0 = legend_x - 0.3, y0 = legend_y - (i - 1) * legend_spacing,
           x1 = legend_x - 0.1, y1 = legend_y - (i - 1) * legend_spacing,
           col = group_colors[names(group_colors)[i]], lwd = legend_line_width)
  
  # Add text next to the color indicator
  text(x = legend_x + 0.1, y = legend_y - (i - 1) * legend_spacing,
       labels = names(group_colors)[i], pos = 4, cex = 1.2)
}


table(meta$group, meta$year)

plot_folder <- file.path(Sys.getenv("HOME"), "Desktop", "Plots")
if (!dir.exists(plot_folder)) {
  dir.create(plot_folder)
}
table(meta$group)
# Set resolution parameters
dpi <- 500  # Desired resolution in dots per inch
width_inch <- 8  # Width of the plot in inches
height_inch <- 6  # Height of the plot in inches
topics_of_interest<- 1:11



# Loop through each topic to generate and save plots
for (topic in topics_of_interest) {
  
  # Estimate the effect of group, year, and the factor variable on the topics
  stm1effect <- estimateEffect(
    formula =~group*s(year)+ EPseats_redefined_factor1, 
    stmobj = stm, 
    metadata = meta,
  )
  
  # Extract effect estimates manually for each group, considering the first document
  effects <- lapply(names(group_colors), function(group) {
    effect <- plot.estimateEffect(
      stm1effect,
      covariate = "year",
      model = stm,
      topics = topic,  
      method = "continuous",
      moderator = "group",
      moderator.value = group, 
      add = FALSE,
      omit.plot = TRUE,
      smoothing = "loess",
      span = 0.99
    )
    
    # Filter data to only include years after the first document in the meta for the group
    first_year <- min(meta$year[meta$group == group])
    filtered_effect <- list(
      x = effect$x[effect$x >= first_year],
      means = lapply(effect$means, function(y) y[effect$x >= first_year])
    )
    
    return(filtered_effect)
  })
  
  # Determine the min and max y-values across all groups
  ymin <- min(sapply(effects, function(e) min(e$means[[1]], na.rm = TRUE)), na.rm = TRUE)
  ymax <- max(sapply(effects, function(e) max(e$means[[1]], na.rm = TRUE)), na.rm = TRUE)
  
  # Extend ymin and ymax to the next best 5-value sequence
  ymin <- floor(ymin / 2) * 2
  ymax <- ceiling(ymax / 2) * 2
  
  # Set up the filename for the plot
  filename <- file.path(plot_folder, paste0("topic_", topic, "_smooth.png"))
  
  # Create the PNG file with high resolution
  png(filename, width = width_inch * dpi, height = height_inch * dpi, res = dpi)
  
  # Set up the plot parameters
  par(bty = "n", lwd = 2, xaxt = "n", yaxt = "n")  # Remove box around plot, set line width, and remove x and y-axis
  
  # Initialize the plot with the first group
  plot(effects[[1]]$x, effects[[1]]$means[[1]], type = "l", 
       xlab = "Election Year",
       ylab = "Expected Topic Proportions",
       ylim = c(ymin, ymax),  # Y-axis range based on calculated min and max
       xlim = c(1979, 2024),
       col = group_colors[names(group_colors)[1]])
  
  # Add lines for the remaining groups
  for (i in 2:length(effects)) {
    lines(effects[[i]]$x, effects[[i]]$means[[1]], 
          col = group_colors[names(group_colors)[i]])
  }
  
  # Add horizontal reference line
  abline(h = 0, lty = 4, lwd = 1, col = "grey45")  # Dotted line at y = 0
  
  # Restore y-axis settings **before** adding the axis
  par(yaxt = "s")
  
  # Customize x-axis with specific tick marks
  par(xaxt = "s")
  axis(1, at = c(1979, 1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019, 2024), 
       labels = c(1979, 1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019, 2024), las = 2)
  
  # Customize y-axis with specific tick marks and scaling
  axis(2, at = seq(ymin, ymax, by = 2), las = 2)  
  
  par(bty = "o", lwd = 1, xaxt = "s", yaxt = "s")
  
  # Close the PNG device to save the file
  dev.off()
}


# Step 1: Extract the effect for Topic 3
topic3_effect <- summary(stm1effect, topics = 3)
plot(stm1effect, 
     covariate = "year", 
     model = stm, 
     method = "continuous", 
     topics = 3, 
     moderator = "group", 
     moderator.value = "NI",
     xlab = "Year", 
     ylab = "Estimated Topic Proportion",
     main = "Topic 3 Proportions for S&D Group Over Time")

# Step 3: Extracting the underlying data (optional)
# This part involves accessing the data behind the plot to get the raw numbers
library(dplyr)

# If you want to look at the data used in the plot
plot_data <- plot.estimateEffect(
  stm1effect, 
  covariate = "year", 
  method = "continuous", 
  topics = 3, 
  moderator = "group", 
  moderator.value = "NI",
  printlegend = FALSE
)

# Extract and view the data points
topic3_sd_over_time <- data.frame(year = plot_data$x, 
                                  topic3_proportion = plot_data$means)

# Print the extracted data
print(topic3_sd_over_time)
View(topic3_sd_over_time)

table(meta$group, meta$year)
data_2019 <- meta %>% filter(meta$year == "2019")
data_2019 <- data_2019 %>% filter(data_2019$group == "GUE/NGL")
View(data_2019)

users_stm<-readRDS("~/Desktop/Final.rds")

library(dplyr)

library(stm)

meta<-users_stm$meta
docs<-users_stm$documents
vocab<-users_stm$vocab

meta <- meta %>%
  mutate(country = case_when(
    country == "czech republic" ~ "czechrepublic",
    country == "the netherlands" ~ "netherlands",
    TRUE ~ country
  ))


table(meta$country)

# Proceed with topic modeling
K_values <- c(3:50) 

result1 <- searchK(
  documents = docs, 
  vocab = vocab, 
  K = K_values, 
  init.type = "Spectral", 
  data = meta
)


# Plot the result
saveRDS(result1, file = "~/Desktop/searchK_result3_50.rds")

# Extracting the metrics
semantic_coherence <- result1$results$semcoh
exclusivity <- result1$results$exclus
K_values <- result1$results$K

# Create data frame for plotting
data <- data.frame(K_values, 
                   SemanticCoherence = unlist(semantic_coherence), 
                   Exclusivity = unlist(exclusivity))


data_3_30 <- data.frame(K_values = K_values, 
                        SemanticCoherence = unlist(data$SemanticCoherence), 
                        Exclusivity = unlist(data$Exclusivity))

x_min <- min(data_3_30$SemanticCoherence)
x_max <- max(data_3_30$SemanticCoherence)
y_min <- min(data_3_30$Exclusivity)
y_max <- max(data_3_30$Exclusivity)

library(ggplot2)
# Plot Semantic Coherence vs Exclusivity
plot_semcoh_exclus <- ggplot(data, aes(x = SemanticCoherence, y = Exclusivity, label = K_values)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  geom_segment(x = x_min, y = y_min, xend = x_max, yend = y_max, linetype = "dashed", color = "red") +  # Adding the line segment
  labs(title = "Semantic Coherence vs Exclusivity (K = 3 to 50)",
       x = "Semantic Coherence",
       y = "Exclusivity") +
  theme_minimal()

plot_semcoh_exclus





K_values <- c(8:14) 

result2 <- searchK(
  documents = docs, 
  vocab = vocab, 
  K = K_values, 
  init.type = "Spectral", 
  data = meta
)


# Plot the result
saveRDS(result2, file = "~/Desktop/searchK_result8_14.rds")


# Extracting the metrics for result2
semantic_coherence_2 <- result2$results$semcoh
exclusivity_2 <- result2$results$exclus
K_values_2 <- result2$results$K

# Create data frame for plotting
data_2 <- data.frame(K_values = K_values_2, 
                     SemanticCoherence = unlist(semantic_coherence_2), 
                     Exclusivity = unlist(exclusivity_2))

# Determine plot range
x_min_2 <- min(data_2$SemanticCoherence)
x_max_2 <- max(data_2$SemanticCoherence)
y_min_2 <- min(data_2$Exclusivity)
y_max_2 <- max(data_2$Exclusivity)
library(ggplot2)

# Plot Semantic Coherence vs Exclusivity for result2
plot_semcoh_exclus_2 <- ggplot(data_2, aes(x = SemanticCoherence, y = Exclusivity, label = K_values)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  geom_segment(x = x_min_2, y = y_min_2, xend = x_max_2, yend = y_max_2, linetype = "dashed", color = "red") +  # Adding the line segment
  labs(title = "Semantic Coherence vs Exclusivity (K = 8 to 14)",
       x = "Semantic Coherence",
       y = "Exclusivity") +
  theme_minimal()

# Display the plot
plot_semcoh_exclus_2








# Display the plot

stm <- stm(documents = docs,
           vocab = vocab,
           K = 11,
           max.em.its = 1000,
           init.type = "Spectral",
           data = meta,
           content = ~group,
           prevalence = ~ s(year) + meta$EPseats_redefined_factor1)



library(readr)

write_rds(stm, file = "~/Desktop/stm_spline.rds")

table(meta$EPseats_redefined_factor1)

?stm

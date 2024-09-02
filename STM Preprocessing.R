library(haven)
library(dplyr)
library(readxl)
library(stm)
library(openxlsx)
library(ggplot2)
library(stopwords)
library(stringi)
library(stringdist)
library(textclean)
library(textstem)
library(tm)
library(purrr)
library(cld3)
library(stringr)
library(udpipe)
library(spacyr)
library(data.table)
library(quanteda)
setwd("/Users/LeonO/Desktop/")


final_info<-read_excel("~/Desktop/final.xlsx")

final_info$Character_Count <- nchar(final_info$Cleaned_Content)
sum(final_info$Character_Count)

# Ensure final_info is a data frame
final_info <- as.data.frame(final_info)

# Function to remove URLs from text
remove_urls <- function(text) {
  # Define regex pattern to match URLs
  url_pattern <- "http(s)?://\\S+\\b|www\\.\\S+\\b"
  
  # Remove URLs from text
  cleaned_text <- gsub(url_pattern, "", text, ignore.case = TRUE)
  
  return(cleaned_text)
}

# Function to remove numbers from text
remove_numbers <- function(text) {
  # Define regex pattern to match numbers
  number_pattern <- "\\b\\d+\\b"
  
  # Remove numbers from text
  cleaned_text <- gsub(number_pattern, "", text)
  
  return(cleaned_text)
}

replace_hyphens <- function(text) {
  cleaned_text <- gsub("-", " ", text)
  return(cleaned_text)
}


# Create unique identifiers
final_info$unique_id <- seq(1, nrow(final_info))

final_info <- final_info %>%
  mutate(Cleaned_Content = sapply(Cleaned_Content, function(x) replace_hyphens(remove_numbers(remove_urls(x))))) %>%
  mutate(Cleaned_Content = str_trim(Cleaned_Content)) # Trim whitespace



# Combine the unique identifier with the cleaned_content
final_info$cleaned_content_with_id <- paste0(final_info$unique_id, " || ", final_info$Cleaned_Content)


# Function to split text into words but keep quoted sections intact
insert_commas <- function(text) {
  # Split the text by spaces except within quotes
  words <- unlist(strsplit(text, "(?<=\\S) (?=\\S)|(?<=\") (?=\")", perl = TRUE))
  return(paste(words, collapse = ","))  # Join the words with commas
}

# Function to apply insert_commas and print progress
process_row <- function(text, row_number) {
  message("Processing row: ", row_number)
  insert_commas(text)
}


# Apply the function to each row in the column with progress tracking
final_info <- final_info %>%
  mutate(cleaned_content_with_id = map2(cleaned_content_with_id, row_number(), process_row))









final<-read.csv("~/Desktop/STM.csv")
final<-read_csv(file.choose())



#important step
final <- final[!(final$EPseats == 0 & final$country != "europe"), ]

final <- final %>%
  mutate(group = as.character(group))


# Update the group values based on emcs_year
final <- final %>%
  mutate(group = case_when(
    emcs_year == "13954_94" ~ "50",
    emcs_year == "13955_94" ~ "50",
    emcs_year == "31952_94" ~ "100",
    emcs_year == "51903_94" ~ "100",
    TRUE ~ group  # Keep existing group value if emcs_year does not match
  ))

filtered_meta <- final[final$year == 1999, ]

View(filtered_meta)
# Define the mapping of old values to new values
# Define the group mapping based on the provided instructions
group_mapping <- c(
  "11" = "EPP",
  "10" = "EPP",
  "60" = "EPP",
  "80" = "EPP",
  "90" = "EPP",
  "91" = "EPP",
  "20" = "S&D",
  "30" = "Greens/EFA",
  "31" = "Greens/EFA",
  "32" = "Greens/EFA",
  "40" = "ALDE/Renew",
  "41" = "ALDE/Renew",
  "50" = "GUE/NGL",
  "51" = "GUE/NGL",
  "52" = "GUE/NGL",
  "53" = "GUE/NGL",
  "54" = "GUE/NGL",
  "70" = "ID",
  "71" = "ID",
  "72" = "ID",
  "100" = "ID",
  "110" = "EPP",
  "130" = "ECR",
  "140" = "ECR",
  "999" = "NI",
  "888" = "NI",
  "-777" = "NI",
  "-888" = "NI",
  "-999" = "NI"
)

# Convert the group column to character
final <- final %>%
  mutate(group = as.character(group))



# Update the group values based on the mapping
final <- final %>%
  mutate(group = case_when(
    group %in% names(group_mapping) ~ group_mapping[group],
    TRUE ~ group  # Keep existing group value if it does not match any key in group_mapping
  ))

table(final$group)

# Use mutate and case_when to add a new column with abbreviations
final <- final %>%
  mutate(group = case_when(
    group == "-999" ~ "Unavailable",
    group == "-888" ~ "Inapplicable",
    group == "-777" ~ "Unknown",
    group == "-666" ~ "Did not compete",
    group == "888" ~ "NI",
    group == "999" ~ "NI",
    group == "10" ~ "EPP",
    group == "11" ~ "EPP",
    group == "20" ~ "S&D",
    group == "30" ~ "Greens/EFA",
    group == "31" ~ "Greens/EFA",
    group == "32" ~ "Greens/EFA",
    group == "40" ~ "ALDE/Renew Europe",
    group == "41" ~ "ALDE/Renew Europe",
    group == "50" ~ "GUE/NGL",
    group == "51" ~ "GUE/NGL",
    group == "52" ~ "GUE/NGL",
    group == "53" ~ "GUE/NGL",
    group == "54" ~ "GUE/NGL",
    group == "60" ~ "EPP",
    group == "70" ~ "ID",
    group == "71" ~ "EFDD",
    group == "100" ~ "ECR",
    group == "130" ~ "ECR",
    group == "110" ~ "EPP",
    group == "90" ~ "EPP",
    group == "91" ~ "EPP",
    group == "80" ~ "EPP",
    group == "140" ~ "ID",
    TRUE ~ group  # Handle cases not specified in the mapping
  ))


final <- final %>%
  mutate(group = case_when(
    group %in% c("ALDE/Renew Europe", "ALDE/Renew") ~ "ALDE/Renew",
    TRUE ~ group))
# Assuming you have a 'group_mapped' column in your meta DataFrame

# Step 1: Combine "Renew Europe" with "ALDE/Renew"
final$group[final$group == "Renew Europe"] <- "ALDE/Renew"

# Step 2: Reassign "EFDD" to "ECR"
final$group[final$group == "EFDD"] <- "ECR"

table(final$group)

valid_groups <- c("NI", "EPP", "S&D", "Greens/EFA", 
                  "ALDE/Renew", "GUE/NGL", "ID", "ECR")



# Filter rows to keep only those with the specified group names
filtered_final <- final %>%
  filter(group %in% valid_groups)

final<-filtered_final




  
final$Character_Count <- nchar(final$translated)

sum(final$Character_Count, na.rm = TRUE)

table(final$group)




df <-final

write.csv(df, file = "~/Desktop/LASTONE.csv")

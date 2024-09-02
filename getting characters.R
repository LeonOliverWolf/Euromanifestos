library(data.table)
library(stringr)
library(dplyr)

setwd("~/Desktop/translated/")

# List of EU countries in lowercase
eu_countries <- c(
  "austria", "belgium", "bulgaria", "croatia", "cyprus", "czechrepublic", 
  "denmark", "estonia", "europe","finland", "france", "germany", "greece", "hungary", 
  "ireland", "italy", "latvia", "lithuania", "luxembourg", "malta", "netherlands", 
  "poland", "portugal", "romania", "slovakia", "slovenia", "spain", "sweden"
)

# Define the pattern for matching files
pattern <- paste0("output_(", paste(eu_countries, collapse = "|"), ")_\\d{1,4} en\\.txt")

# List all matching files
matching_files <- list.files(pattern = pattern, full.names = TRUE)

# Function to extract row id from the signifier
extract_row_id <- function(line) {
  match <- str_match(line, "^([0-9]+),\\|\\|")[, 2]
  return(as.integer(match))
}

# Initialize an empty list to store data tables
data_list <- list()

# Process each file
for (file in matching_files) {
  # Read the entire file content with UTF-8 encoding
  file_content <- readLines(file, encoding = "UTF-8", warn = FALSE)
  
  # Combine all lines into a single string for easier processing
  combined_content <- paste(file_content, collapse = "\n")
  
  # Find all row boundaries (start of each row block)
  row_starts <- str_locate_all(combined_content, "\\d+,\\|\\|")[[1]]
  
  # Add a final boundary for processing the last block
  row_starts <- rbind(row_starts, c(nchar(combined_content) + 1, nchar(combined_content) + 1))
  
  # Process each block between row boundaries
  for (i in 1:(nrow(row_starts) - 1)) {
    start_pos <- row_starts[i, 1]
    end_pos <- row_starts[i + 1, 1] - 1
    
    block <- substr(combined_content, start_pos, end_pos)
    
    # Extract row id from the first line of the block
    first_line <- strsplit(block, "\n")[[1]][1]
    if (grepl("^[0-9]+,\\|\\|", first_line)) {
      current_row_id <- extract_row_id(first_line)
      content_block <- strsplit(block, "\n")[[1]][-1]  # Remove the first line
      
      # Print message for current signifier
      cat(sprintf("Processing signifier: %d\n", current_row_id))
      
      # Extract content enclosed in quotes, handling cases where quotes might be missing
      quoted_contents <- str_extract_all(paste(content_block, collapse = " "), '"[^"]*[^"]*"|\'[^\'\']*[^\'\']*\'')
      
      # Flatten the list and remove extra quotes
      quoted_contents <- unlist(quoted_contents)
      quoted_contents <- gsub('^["\'"]|["\'"]$', '', quoted_contents)
      
      # Handle empty quotes: filter and combine non-empty contents
      combined_quoted_content <- paste(quoted_contents[quoted_contents != ""], collapse = " ")
      
      # Only add to data_list if combined_quoted_content is non-empty
      if (nchar(trimws(combined_quoted_content)) > 0) {
        data_list[[length(data_list) + 1]] <- data.table(row_id = current_row_id, translated = I(list(combined_quoted_content)))
      }
    }
  }
}

# Combine all data tables into a single data.table
translated_df <- rbindlist(data_list, use.names = TRUE, fill = TRUE)


print(translated_df$translated[1])

View(translated_df)






translate<-read.csv("/Users/LeonO/Desktop/processs.csv")


df_merged <- merge(final_info, translate, by.x = "unique_id", by.y = "row_id", all.x = TRUE)


df <- df_merged %>%
  mutate(translated = ifelse(country %in% c('ireland', 'unitedkingdom', 'europe'), 
                             cleaned_content_with_id, 
                             translated))


df$Character_Count <- nchar(df$translated)

sum(df$Character_Count, na.rm = TRUE)














# Filter rows with NA 'translated' and exclude specified countries
df_na_translated_filtered <- na_translated_df
# Set working directory
setwd("~/Desktop/special cases/")

# Initialize variables
max_chars <- 990000

# Function to write content to a file with UTF-8 encoding
write_to_file <- function(content, index, country) {
  filename <- paste0("na_output_", country, "_", index, ".txt")
  writeLines(content, filename, useBytes = TRUE)
  message("Written file: ", filename)
}

# Iterate over each country in the filtered DataFrame
countries <- unique(df_na_translated_filtered$country)

for (country in countries) {
  message("Processing country: ", country)
  
  # Extract rows corresponding to the current country
  country_rows <- df_na_translated_filtered %>% filter(country == !!country)
  
  # Initialize variables for accumulating content and file index
  current_content <- ""
  file_index <- 1
  
  for (i in 1:nrow(country_rows)) {
    row_content <- as.character(country_rows$cleaned_content_with_id[i])
    
    # Handle NA values by treating them as empty strings
    if (is.na(row_content)) {
      row_content <- ""
    }
    
    if (nchar(current_content) + nchar(row_content) + 1 > max_chars) {
      # Write the current content to a file
      write_to_file(current_content, file_index, country)
      
      # Increment the file index
      file_index <- file_index + 1
      
      # Reset the current content
      current_content <- row_content
    } else {
      # Accumulate the content
      if (nchar(current_content) > 0) {
        current_content <- paste(current_content, row_content, sep = " ")
      } else {
        current_content <- row_content
      }
    }
  }
  
  # Write any remaining content to a final file
  if (nchar(current_content) > 0) {
    write_to_file(current_content, file_index, country)
  }
}







# Load the necessary package
library(readr)

# Define the folder path
folder_path <- "~/Desktop/Cases"  # Change this to your folder path

# List all .txt files in the folder
txt_files <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

# Initialize a data frame to store results
result <- data.frame(Content = character(), FileName = character(), stringsAsFactors = FALSE)

# Loop through each file and read its content
for (file in txt_files) {
  # Read the content of the file
  file_content <- read_file(file)
  
  # Get the file name from the file path
  file_name <- basename(file)
  file_name <- sub("\\.txt$", "", basename(file))
  
  # Append the content and file name to the data frame
  result <- rbind(result, data.frame(translated = file_content, row_id = file_name, stringsAsFactors = FALSE))
}



result$row_id <-as.character(result$row_id)
df$unique_id <-as.character(df$unique_id)

df <- df %>%
  left_join(result, by = c("unique_id" = "row_id")) %>%
  mutate(translated = coalesce(translated.y, translated.x)) %>%
  select(-translated.x, -translated.y)


na_translated_df <- df %>% 
  filter(is.na(translated))

table(is.na(df$translated))

print(df$translated[184])

final<-df

write_csv(final, file = "~/STM.csv")

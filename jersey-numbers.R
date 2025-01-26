grab_jersey <- function(player_id) {
  
  swish_url <- paste0("https://www.swishanalytics.com/nba/players/player?id=", player_id)
  
  swish <- read_html(swish_url)
  
  result <- swish %>% 
    html_node(".mobile-hide") %>%
    html_text() %>% 
    # Extract out numeric
    parse_number()
  
  result
}


a1_ent <- epv_df |> pull(a1_ent) |> unique()
a2_ent <- epv_df |> pull(a2_ent) |> unique()

# Generate the variable names for a1 through a5 and h1 through h5
columns <- c(paste0("a", 1:5, "_ent"), paste0("h", 1:5, "_ent"))

# Iterate over each column name and dynamically assign the unique values to corresponding variables
for (col in columns) {
  assign(col, epv_df |> pull(all_of(col)) |> unique())
}

library(rvest)

# Generate the variable names for a1 through a5 and h1 through h5
columns <- c(paste0("a", 1:5), paste0("h", 1:5))

# Iterate over each column name and dynamically assign the results
for (col in columns) {
  ent_var <- paste0(col, "_ent")              # Create the entity variable name (e.g., a1_ent, h1_ent)
  jersey_var <- paste0(col, "_jersey_number") # Create the jersey variable name (e.g., a1_jersey_number, h1_jersey_number)
  
  # Dynamically evaluate and assign the jersey number variable
  assign(jersey_var, sapply(get(ent_var), grab_jersey))
}


# Generate the prefixes for a1 through a5 and h1 through h5
columns <- c(paste0("a", 1:5), paste0("h", 1:5))

# Loop through each prefix and dynamically create and mutate the jersey number columns
for (col in columns) {
  ent_var <- paste0(col, "_ent")              # Entity column name (e.g., a1_ent, h1_ent)
  jersey_var <- paste0(col, "_jersey_number") # Jersey column name (e.g., a1_jersey_number, h1_jersey_number)
  
  # Create a lookup table for each pair of ent and jersey number vectors
  lookup_table <- data.frame(ent = get(ent_var), jersey_number = get(jersey_var))
  
  # Mutate the new jersey number column into epv_df
  epv_df <- epv_df %>%
    mutate(!!jersey_var := match(!!sym(ent_var), lookup_table$ent) %>% lookup_table$jersey_number[.])
}


epv_df |> write_csv("data/epv_df.csv")



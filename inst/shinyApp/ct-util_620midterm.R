
# BIS 620 Midterm: Shiny Web App
# Emilia Liu & Tianyi Gao

library(dplyr)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)

# Create the connection
if (!exists("con")) {
  con = dbConnect(
    duckdb(
      file.path("ctrialsgovdb", "ctrialsgov.duckdb"), 
      read_only = TRUE
    )
  )
  if (length(dbListTables(con)) != 50) {
    stop("Problem reading from connection.")
  }
  studies = tbl(con, "studies")
  sponsors = tbl(con, "sponsors")
  conditions  = tbl(con, "conditions")
  countries = tbl(con, "countries")
  eligibility = tbl(con, "eligibilities")
}


### Combine Tables ###
# Create a summary of medical conditions for each trial
condition_summary = conditions |>
  select(nct_id, name) |>
  group_by(nct_id) |>
  summarise(condition = list(name))

# Generate a summary counting the occurrences of conditions for each trial
count_summary = conditions |>
  select(nct_id, name) |>
  group_by(nct_id) |>
  summarize(count_condition = n())

# Combine the condition summary and count summary by trial ID
combined_condition_data = left_join(
  condition_summary, 
  count_summary |> 
    select(nct_id, count_condition), by = "nct_id"
) 

# Link study information with the conditions and counts by trial ID
study_condition_data = left_join(
  studies, 
  combined_condition_data |> 
    select(nct_id, condition, count_condition), by = "nct_id"
) 

# Add sponsor information to the dataset by joining study and sponsor data
study_sponsor_data = left_join(
  study_condition_data, 
  sponsors |> 
    select(nct_id, sponsor_status = lead_or_collaborator), by = "nct_id"
) 

# Incorporate country information into the dataset by joining the merged data with country data
complete_data = left_join(
  study_sponsor_data, 
  countries |> 
    select(nct_id, name), by = "nct_id"
) 

# Combine eligibility information with the previously joined data by matching them using trial ID
final_data = left_join(
  complete_data, 
  eligibility |> 
    select(nct_id, gender), by = "nct_id"
) 




#' @title Query keywords from a database table.
#' @description Description goes here.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords 
#' (intersection) or any of the keywords (union)? (default FALSE; union).
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query)) 
}


# Create a histogram of the phases returned by a brief title keyword search
#' @param x the phase data.
#' @param brief_title_kw the brief title keywords to look for. This is optional.
plot_phase_histogram = function(x) {
  x$phase[is.na(x$phase)] = "NA"
  x = x |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n()) 


#####
### Problem 1 ###
  # Extract unique phases from the final data
  unique_phases = final_data |>
    select(phase) |>
    collect() |>
    unique()
  
  # Replace missing values with "NA" in the unique phases
  unique_phases[is.na(unique_phases)] = "NA"
  
  # Extract levels from the unique phases
  levels = unique_phases$phase
  
  # Plot the trials and their corresponding phase data and count for each phase
  ggplot(x, aes(x = phase, y = n)) +
    geom_col(fill = "skyblue") +
    theme_bw() +
    xlab("Phase") +
    ylab("Count") +
    scale_x_discrete("Phase", limits = levels, drop = FALSE)
  }
  

#####
### Problem 2 ###
#' Create a histogram of the conditions returned by a brief title keyword search
#' @param x the condition data.
plot_condition_histogram = function(x) {
  x$condition[is.na(x$condition)] = "NA"
  
  xx = conditions |>
  select(nct_id, name)
  
  table = left_join(
    x, 
    xx |> 
      select(nct_id, name), by = "nct_id", copy = TRUE
  ) 
  

  table = table |>
    collect() |>
    select(nct_id, name.y) |>
    group_by(name.y) |>
    summarize(n = n()) |>
    head(30) # Select the top 30 conditions for the histogram
  
  # Create a histogram plot with reordered condition names and counts
  print(ggplot(table, aes(x = reorder(name.y, n), y = n)) +
          geom_col(fill = "purple") +
          theme_bw() + 
          coord_flip() + 
          xlab("Condition") +
          ylab("Count")
  )
  }

#####

#####
### Problem 3 ###
# Find distinct sponsor types
sponsor_types = sponsors |>
  select(sponsor_type = lead_or_collaborator) |>
  collect() |>
  unique()

# Create an option to consider all sponsor types
selected_sponsor_type = append(sponsor_types$sponsor_type, "All")

# Define a function for filtering trials by sponsor type
#' @param sponsor_type the type of sponsor
#' @param studies_data the studies to subset according to sponsor type for.
get_trials_by_type = function(sponsor_type, studies_data) {
  if (sponsor_type != "All") {
    filtered_data = studies_data |>
      filter(lead_or_collaborator == sponsor_type)
  } else {
    filtered_data = studies_data
  }
}
#####

#####
### Problem 4 feature 1 ###
#' Create a histogram of the status returned by a brief title keyword search
#' @param data the status data.
plot_status_histogram = function(data) {
  # Group the data by overall status and count the occurrences
  data = data |>
    select(overall_status) |>
    group_by(overall_status) |>
    summarize(count = n()) 
  
  # Prepare the x-axis values for the histogram
  status_levels = final_data |>
    select(overall_status) |>
    collect() |>
    unique()
  status_levels[is.na(status_levels)] = "NA"
  status_categories = status_levels$overall_status
  
  # Plot the trials and their corresponding status data and count for each status
  ggplot(data, aes(x = overall_status, y = count)) +
    geom_col(fill = "lightblue") +
    theme_bw() + 
    xlab("Status") +
    ylab("Count") +
    scale_x_discrete("Status", limits = status_categories, drop = FALSE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Find all distinct trial status types
status_types = final_data |>
  select(overall_status) |>
  collect() |>
  unique()

# Create an option to consider all status types
status_types = append(status_types$overall_status, "All") 

# Define a function to subset trials based on status type
#' @param selected_type the selected type of status
#' @param data the studies that need to subset according to status type.
get_status_type = function(selected_type, data) {
  if (selected_type != "All") {
    filtered_data = data |>
      filter(overall_status == selected_type)
  } else {
    filtered_data = data 
  }
}
#####


#####
### Problem 4 feature 2 ###
# Function to subset trials based on a specified date range
#' @param fromDate: The starting date of the range
#' @param toDate: The ending date of the range
#' @param d: The studies to subset according to the date range
get_date_data = function(fromDate, toDate, data) {
  if (!is.null(fromDate) & !is.null(toDate)) {
    date_data = data |>
      filter(start_date >= fromDate, start_date <= toDate)
  } else {
    # For cases where no date range is specified, include all data
    date_data = data
  }
  return(date_data)
}
#####


#####
### Problem 4 feature 3 ###
# Find all unique gender values in the dataset
unique_genders = final_data |>
  select(gender) |>
  collect() |>
  unique()

# Extract gender types
gender_types = unique_genders$gender

# Function to subset trials based on the selected gender type
#' @param selected_gender the gender people selected
#' @param data the studies that need to subset according to status type.
get_gender_data = function(selected_gender, data) {
  if (selected_gender != "All") {
    filtered_data = data |>
      filter(selected_gender == gender)
  } else {
    filtered_data = data
  }
}
#####


#####
### Problem 4 feature 4 ###
# Find all unique countries in the dataset
countries_data = final_data |>
  select(name) |>
  collect() |>
  unique()

country_choices = append(countries_data$name, "All")

#' Get the country of studies that returned by a brief title keyword search
#' @param data the studies used to plot country data.
plot_country_histogram = function(data) {
  country_data = data |>
    select(name, study_first_posted_date) |>
    group_by(study_first_posted_date) |>
    summarize(count = n()) 
  
  ggplot(country_data, aes(x = study_first_posted_date, y = count)) +
    geom_point(alpha = .9) + 
    geom_rug(col = "steelblue", alpha = 0.1, size = 1.5) +
    theme_bw() + 
    xlab("Study Posted Date") +
    ylab("Count")
}

#' Define a function to subset the trials based on country
#' @param selected_country the selected country. 
#' @param data the studies that need to subset according to country selected.
get_country_data = function(selected_country, data) {
  data$selected_country[is.na(data$selected_country)] = "NA"
  if (selected_country != "All") {
    selected_data = data |>
      filter(name == selected_country)
  } else {
    selected_data = data
  }
}
#####


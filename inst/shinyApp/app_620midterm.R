
# BIS 620 Midterm: Shiny Web App
# Emilia Liu & Tianyi Gao

library(shiny)
library(shinythemes)
library(leaflet)

# connect to source file
source("ct-util_620midterm.R")

# define maximum number of studies we want to query at once
max_num_studies = 1000

# design the user interface
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Clinical Trials Query"), # title of the web app
  sidebarLayout( # create a sidebar allowing for feature filtering
    sidebarPanel(
      textInput("brief_title_kw", "Brief Title Keywords"), # main search bar for keywords
      selectInput("sponsor_type",
                  "Select Sponsor Type:", # problem 3: the drop-down list for sponsor types
                  choices = selected_sponsor_type,selected = "All"),
      selectInput("status_type",
                  "Select Trial Status:", # the drop-down list for trial progression status
                  choices = status_choices, selected = "All"),
      selectInput("country",
                  "Select Country:", # the drop-down list for countries
                  choices = country_choices, selected = "All"),
      dateInput("start_date",
                "Start Date From:",value = "1950-01-31", # pop-up calendar: lower limit
      ),
      dateInput("end_date",
                "Start Date To:", value = "2050-12-31" # pop-up calendar: upper limit
      ),
      radioButtons("gender", 
                   h5("Select Gender"), # radio buttons for choosing gender
                   choices = gender_types, selected = "All")
    ),
    
    # design the main panel to display histograms, dot plots and trial table
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Phase", plotOutput("phase_plot")),
        tabPanel("Condition", plotOutput("condition_plot")),
        tabPanel("Country", plotOutput("country_plot")),
        tabPanel("Status", plotOutput("status_plot")),
      ),
      dataTableOutput("trial_table") # trial table that lists detailed trail information
    )
  )
)

# design the server behind the UI
server <- function(input, output) {
  
  get_studies <- reactive({
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret = query_kwds(all, si, "brief_title", match_all = TRUE) # keyword query
      ret = get_trials_by_type(input$sponsor_type, ret)
      ret = get_status_type(input$status_type, ret)
      ret = get_country_data(input$country, ret)
      ret = get_gender_data(input$gender, ret)
      ret = get_date_data(input$start_date, input$end_date, ret)
    } else {
      ret = get_trials_by_type(input$sponsor_type, all) 
      ret = get_status_type(input$status_type, ret)
      ret = get_country_data(input$country, ret)
      ret = get_gender_data(input$gender, ret)
      ret = get_date_data(input$start_date, input$end_date, ret)
    }
    ret |>
      head(max_num_studies) |> # generate maximum number of 1000 studies at once
      collect()
  })
  
  # problem 1: phase histogram
  output$phase_plot = renderPlot({
    get_studies() |>
      plot_phase_histogram() 
  })
  
  # trial table below the main panel
  output$trial_table = renderDataTable({
    get_studies() |> 
      select(nct_id, brief_title, start_date,completion_date,condition,
             lead_or_collaborator,overall_status,gender,name) |>
      rename(`NCT ID` = nct_id, 
             `Brief Title` = brief_title,
             `Start Date` = start_date, 
             `Completion Date` = completion_date,
             `Condition` = condition, 
             `Sponsor type` = lead_or_collaborator, 
             `Status` = overall_status, 
             `Gender` = gender,
             `Country` = name
      )
  })
  
  # feature: trial progression status
  output$status_plot = renderPlot({
    get_studies() |>
      plot_status_histogram()
  })
  
  # problem 2: conditions histogram
  output$condition_plot = renderPlot({
    get_studies() |>
      plot_condition_histogram()
  })
  
  # feature: countries/locations of trials
  output$country_plot = renderPlot({
    get_studies() |>
      plot_country_histogram()
  })
  
  
}


# Run the web app
shinyApp(ui = ui, server = server)

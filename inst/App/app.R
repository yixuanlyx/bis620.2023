library(shiny)
library(dplyr)

source("ct-util.R")
max_num_studies <- 5000
# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("brief_title_kw", "Brief title keywords"),
      #Add a drop-down so that queries can be subsetted on sponsor type
      selectInput("source_class", ("Sponsor Type"),
                  choices = list("All",
                                 "Federal" = "FED",
                                 "Individual" = "INDIV",
                                 "Industry" = "INDUSTRY",
                                 "Network" = "NETWORK",
                                 "NIH" = "NIH",
                                 "Other" = "OTHER",
                                 "Other gov" = "OTHER_GOV",
                                 "Unknown" = "UNKNOWN")),

      #Yixuan-feature 1:
      #Add checkboxes so that queries can be subset on study type
      checkboxGroupInput("study_type", "Study Type",
                         c("Interventional",
                           "Observational",
                           "Observational [Patient Registry]",
                           "Expanded Access"),
                         selected = NULL),
      #Yixuan-feature 2:
      #Add checkboxes so that queries can be subset on study status
      checkboxGroupInput("overall_status", ("Study Status"),
                         c("Completed",
                           "Enrolling by invitation",
                           "Recruiting",
                           "Unknown status",
                           "Active, not recruiting",
                           "Approved for marketing",
                           "Withdrawn",
                           "Not yet recruiting",
                           "Terminated",
                           "Available",
                           "Temporarily not available",
                           "Suspended",
                           "No longer available",
                           "Withheld")),
      #Yuhan-feature 3:
      #Add a text input so that queries can be subset on sponsor name
      textInput("name.y", "Sponsor Name"),
      #Yuhan-feature 4:
      #Add checkboxes so that queries can be subset on lead or collaborator
      checkboxGroupInput("lead_or_collaborator", ("Lead or Collaborator"),
                         choices = list("Lead" = "lead",
                                        "Collaborator" = "collaborator"),
                         selected = NULL),
    ),

    #Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Phase", plotOutput("phase_plot")),
        tabPanel("Concurrent", plotOutput("concurrent_plot")),
        #Add a new tab that gives a histogram showing the conditions
        #that trials in a query are examining.
        tabPanel("Condition", plotOutput("condition_plot"))
      ),
      dataTableOutput("trial_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  get_studies <- reactive({
    si <- input$brief_title_kw |>
      strsplit(",") |>
      unlist() |>
      trimws()
    # input for the sponsor name
    ss <- input$name.y |>
      strsplit(",") |>
      unlist() |>
      trimws()
    # return studies
    #depending on user input of brief title keyword and sponsor name
    if (input$brief_title_kw == "" & input$name.y == "") {
      ret <- studies
    }
    else if (input$brief_title_kw != "" & input$name.y == "") {
      ret = query_kwds(studies, si, "brief_title", match_all = TRUE)}
      else if (input$brief_title_kw == "" & input$name.y != ""){
      ret = query_kwds(studies, ss, '"name.y"', match_all = TRUE)
    } else{
      ret = query_kwds(query_kwds(studies, si, "brief_title", match_all = TRUE),
                       ss, '"name.y"', match_all = TRUE)
    }
      if (input$source_class != "All"){
        ret = ret %>% filter(source_class %in% !!input$source_class)}
      if (!is.null(input$study_type)){
        ret = ret %>% filter(study_type %in% !!input$study_type)}
      if (!is.null(input$overall_status)){
        ret = ret %>% filter(overall_status %in% !!input$overall_status)}
      if (!is.null(input$lead_or_collaborator)){
        ret = ret %>%
          filter(lead_or_collaborator %in% !!input$lead_or_collaborator)}
    ret |>
      head(max_num_studies) |>
      collect()
  })
  #phase plot showing the phase distribution of studies we are examining
  output$phase_plot <- renderPlot({
    get_studies() |>
      plot_phase_histogram()
  })
  #concurrent plot showing the concurrent studies we are examining
  output$concurrent_plot <- renderPlot({
    get_studies() |>
      select(start_date, completion_date) |>
      get_concurrent_trials() |>
      ggplot(aes(x = date, y = count)) +
      geom_line() +
      xlab("Date") +
      ylab("Count") +
      theme_bw()
  })

  #histogram showing the conditions that trials in a query are examining.
  output$condition_plot <- renderPlot({
    get_studies()  %>%
      plot_condition_histogram()
  })

  #trial table showing the basic information of the trials result
  output$trial_table <- renderDataTable({
    get_studies() |>
      select(nct_id, brief_title, start_date,
             completion_date,study_type,overall_status, name.y) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date,
             `Study Type` = study_type,
             `Study Status` = overall_status, `Sponsor Name` = name.y)
  })

}

# Run the application
shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(dplyr)
library(glue)
library(shinyjs)
library(DT)

ui <- page_sidebar(
  title = "Gimme spectacle!",
  useShinyjs(),
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    checkboxGroupInput(
     inputId = "if_female",
     label = "Female director?",
     choices = c(TRUE, FALSE),
     selected = TRUE
   ),
   checkboxInput(
     inputId = "if_english",
     label = "Is english fine?",
     value = TRUE
   ),
   checkboxInput(
     inputId = "if_musical",
     label = "Is musical fine?",
     value = FALSE
   ),
   actionButton(
     inputId = "run_random",
     label = "Gimme!"
   )
  ),
  navset_pill(
    nav_panel(
      title = "one", 
      br(),
      verbatimTextOutput(outputId = "spectacle"),
      shinyjs::hidden(
        uiOutput("open_spec_page")
      )
    ),
    nav_panel(
      title = "all", 
      br(),
      DT::dataTableOutput(outputId = "all_spectacles")
    )
  )
  
)

server <- function(input, output) {
  
  # observe( if(input[["run_random"]] == 0) shinyjs::hide("open_page") )
  
  observe( if(input[["run_random"]] > 0) shinyjs::show("open_spec_page") )
  
  all_spectacles <- read.csv("./data/spektakle_online.tsv",
                             header = TRUE,
                             sep = "\t") %>%
    mutate(if_musical = as.numeric(if_musical),
           if_female = as.numeric(if_female),
           if_english = as.numeric(if_english))
  
  filtered <- reactive({
    
    all_spectacles %>%
      filter(if_musical == input[["if_musical"]],
             if_female %in% as.logical(input[["if_female"]]),
             if_english == input[["if_english"]])
    
  })
  
  n_opts <- reactive({ nrow(filtered()) })
  
  n_random <- reactive({
    
    input[["run_random"]]
    
    set.seed(Sys.time())
    
    sample(1:n_opts(), 1)
    
    })
  
  spectacle_random <- reactive({
    
    filtered()[n_random(), ]
    
    })
  
  output[["spectacle"]] <- renderText({
    
    if(input[["run_random"]] > 0) {
      
      glue::glue_data(spectacle_random(), 
                      "Spectacle: {title}
                      Directed by: {director}
                      Duration: {length}
                      Comments: {comments}
                      Famous names: {famous.names}
                      ")
      
    } else ""
    
  })
  

  
  output[["open_spec_page"]] <- renderUI({
    
    actionButton(
        inputId = "open_page",
        label = "Show more!",
        onclick = paste0("window.open('", spectacle_random()[["link"]] , "')" )
      )
    
    
  })
  
  output[["all_spectacles"]] <- DT::renderDataTable({
    
    filtered() %>%
      select(title, director, length, comments, famous.names, link) %>%
      arrange(title)
    
  })
}

shinyApp(ui = ui, server = server)

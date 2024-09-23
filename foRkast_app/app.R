library(shiny)
library(shinythemes)
library(shinyFiles)
library(gh)
library(stringr)

# UI code block
ui<-fluidPage(
  titlePanel("FoRkast"),
  sidebarLayout(
    sidebarPanel(
      selectInput("original_owner", "Original Owner", choices = "TuringPPL"),
      selectInput("repo", "Repo", choices = "Competition-Example-Mpox"),
      textInput("target_file", "Target File", value = "forecast.csv"),
      textInput("destination_folder", "Destination Folder", value = paste0(getwd(),"/Contest_Entries")),
      actionButton("run_button", "Run FoRkast Finder"),
      h3(textOutput("status"))
    ),
    mainPanel(
      h3("Welcome to FoRkast"),
      p("FoRkast is a tool that finds all forked repos taking part in forecasting
        competitions and checks each submission before cloning the repos locally."),
    )
  ),
  theme = shinytheme("flatly")
)


# Define the server logic for the Shiny app

server <- function(input, output,session) {
  
  
  observeEvent(input$run_button, {
    
    # Get the input values
    original_owner <- input$original_owner
    repo <- input$repo
    target_file <- input$target_file
    destination_folder <- input$destination_folder
    
    # Get the list of forks
    forks_data <- as.list(gh("GET /repos/:owner/:repo/forks", owner = original_owner, repo = repo))
    
    # Extract the clone URLs from the API response
    fork_clone_urls <- lapply(forks_data, function(x) x$clone_url)
    
    # Create folders for the cloned repos locally
    errors_folder <- paste0(destination_folder, "/Errors")
    
    # Create the destination & Error folder if it doesn't exist
    if (!dir.exists(destination_folder)) {
      dir.create(destination_folder)
    }
    
    if (!dir.exists(errors_folder)) {
      dir.create(errors_folder)
    }
    ################################################################################### 
    # RUN THROUGH CHECKS AND CLONES FOR EACH REPO  ####################################
    ###################################################################################
    
    for (fork_url in fork_clone_urls) {
      
      owner <- strsplit(fork_url, "/")[[1]][4]
      repo_name <- str_extract(fork_url, "[^/]+\\.git$")
      repo_name <- str_remove(repo_name, "\\.git$")
      
      # Create Error List
      errors <- list()
      
      # Find the contents of the submissions folder in the forked repository
      repo_contents <- gh("GET /repos/:owner/:repo/contents/submission", owner = owner, repo = repo_name)
      
      # Find the target file in the repository contents & if not there, add to errors
      file_info <- repo_contents[sapply(repo_contents, function(x) x$name == target_file)]
      
      if (length(file_info) == 0) {
        errors <- c(errors, "Forecast file not found")
        
      } else {
        ################################################################################### 
        # CHECK THE FORECAST FOR ERRORS USING SUBMISSION TESTS#############################
        ###################################################################################
        
        if(repo=="Competition-Example-Mpox"){
          source(file = "submission_tests/mpox_tests.R", local = TRUE)
        }
        
      }
      
      
      ################################################################################### 
      # CLONE REPOS AND PUT ERRORS IN FOLDER           ##################################
      ###################################################################################
      
      if(length(errors)==0){ # Check if there are no errors
        
        # Construct the full path for cloning
        clone_path <- file.path(destination_folder, repo_name)
        
        # Clone the repository using `sprintf` for clearer formatting
        system_command <- sprintf("git clone %s %s", fork_url, paste0("'",clone_path,"'"))
        system(system_command)
        
      }else{ # If there are errors
        # Construct the full path for cloning
        clone_path <- file.path(errors_folder, repo_name)
        
        # Clone the repository using `sprintf` for clearer formatting
        system_command <- sprintf("git clone %s %s", fork_url, paste0("'",clone_path,"'"))
        system(system_command)
        
        # Write the errors to a text file
        writeLines(unlist(errors), file.path(clone_path, "errors.txt"))
        
        
      }
    }
    
    output$status <- renderText("FoRkast Finder Complete")
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)

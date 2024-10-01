library(shiny)
library(bslib)
library(gh)
library(stringr)


sidebar_width<-4

sidebarPanel2 <- function (..., out = NULL, width = sidebar_width) {
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}


owner_choices<-c("TuringPPL")
contest_choices<-c("Competition-Example-Mpox")



# UI code block
ui<-fluidPage(
  theme = bs_theme(
    bootswatch = "minty",
    base_font = font_google("Roboto"),
    ),
  
  h1(id="big-heading", "FARA - Forecasting AggregatoR App"),
  tags$style(HTML("#big-heading{color: #78c2ad;}")),
  sidebarLayout(
    
    sidebarPanel2(

      radioButtons("contest_owner", "Owner", choices = owner_choices,inline = TRUE),
      radioButtons("repo", "Repo", choices = contest_choices,inline = TRUE),
      
      textInput("target_file", "Target File To Check", value = "forecast.csv"),
      textInput("destination_folder", "Destination Folder", value = paste0(getwd(),"/Contest_Entries")),
      
      actionButton(inputId = "run_button", label = "Run FARA",icon = icon("play"),class = "btn-lg btn-success"),
      tags$br(),
      textOutput("status"),
      out = img(src='FAR.png',width = 250),
      
      width = sidebar_width
    ),
    
    mainPanel(
      h2("Welcome to FARA"),
      p("FARA (Forecasting AggregatoR App) is a tool that finds all forked repos taking part in forecasting
        competitions and checks each submission before cloning the repos locally."),
      
      h3("Instructions"),
      p("1. Select the owner of the competition"),
      p("2. Select the competition repo"),
      p("3. Enter the name of the file to check (this is usually a forecast in a csv format)"),
      p("4. Enter the destination folder where the repos will be cloned (this will autofill with the FoRkast app directory)"),
      p("5. Click the 'Run FARA' button to start the process"),
      
      h3("Outputs"),
      p("All submissions that pass the checks will be cloned to the destination folder (each within their own folder named owner_repo"),
      p("If there are any errors, the repos with issues will be named in the folder called 'Errors'. Each repo will have a text file with the errors listed."),
      p(tags$i("If the cloned repo folders already exist within the destination folder, FoRkast will not overwrite them. To clone any updated repos, please delete the older versions")),
      
      h3("Customising FARA"),
      p("This app can be editted to find forks of any repo and check any file and the checks can also be customised to suit the competition requirements. 
        Head to the ",tags$a(href="https://github.com/TuringPPL/Forecast-AggregatoR", "TuringPPL Forecast-AggregatoR")," to find the code and make changes. This app can also be used in conjunction 
        with our contest template repo to run your own forecasting contests: ",
        tags$a(href="https://github.com/TuringPPL/forecasting-contest-template", "TuringPPL Forecasting Contest Template"),
        "."),
      
    )
    
  )
)


# Define the server logic for the Shiny app

server <- function(input, output,session) {
  
  success<-0
  
  observeEvent(input$run_button, {
    
    # Get the input values
    contest_owner <- input$contest_owner
    repo <- input$repo
    target_file <- input$target_file
    destination_folder <- input$destination_folder
    
    # Get the list of forks
    forks_data <- as.list(gh("GET /repos/:owner/:repo/forks", owner = contest_owner, repo = repo))
    
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
      
      forked_owner <- strsplit(fork_url, "/")[[1]][4]
      forked_repo_name <- str_extract(fork_url, "[^/]+\\.git$")
      forked_repo_name <- str_remove(forked_repo_name, "\\.git$")
      
      # Create Error List
      errors <- list()

      
      # Find the contents of the submissions folder in the forked repository
      repo_contents <- gh("GET /repos/:owner/:repo/contents/submission", owner = forked_owner, repo = forked_repo_name)
      
      # Find the target file in the repository contents & if not there, add to errors
      file_info <- repo_contents[sapply(repo_contents, function(x) x$name == target_file)]
      
      if (length(file_info) == 0) {
        errors <- c(errors, "Forecast file not found")
        
      } else {
        
        
        ################################################################################### 
        # CHECK THE FORECAST FOR ERRORS USING SUBMISSION TESTS ############################
        ###################################################################################
        
        if(repo=="Competition-Example-Mpox"){
          # Fetch the file content (assuming it's a text file)
          file_content_encoded <- gh("GET /repos/:owner/:repo/contents/submission/:path",
                                     owner = forked_owner, repo = forked_repo_name, path = target_file)$content
          
          # Decode the content from base64
          file_content <- rawToChar(base64enc::base64decode(file_content_encoded))
          
          # Split the text into lines
          lines <- strsplit(file_content, "\n")[[1]]
          
          # Extract column names from the first line
          col_names <- strsplit(lines[1], ",")[[1]]
          
          # Split each remaining line into columns and create a list of rows
          data_rows <- lapply(lines[-1], function(line) strsplit(line, ",")[[1]])
          
          # Create the data.frame
          df <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
          colnames(df) <- col_names
          rm(lines, col_names, data_rows,file_content_encoded,file_content)
          
          # Check if the data.frame has the correct columns
          if(!all(c("who_region", "month_start","Estimate") %in% colnames(df))){
            errors <- c(errors, "Incorrect column names")
          }
          
          # Check if the forecast column is numeric
          if(!all(str_detect(df$Estimate, "^-?\\d+(\\.\\d+)?$"))){
            errors <- c(errors, "Forecast column is not numeric")
          }
          
          # Check if the forecast column contains -9999
          if(-9999 %in% df$Estimate){
            errors <- c(errors, "Forecast column contains -9999")
          } 
          
          
        }else{
          errors <- c(errors, "No tests available for this competition")
        }
        
      }
      
      
      ################################################################################### 
      # CLONE PASSING REPOS AND PUT ERRORS IN NAMED FOLDER           ####################
      ###################################################################################
      
      if(length(errors)==0){ # Check if there are no errors
        
        # Construct the full path for cloning
        clone_path <- file.path(destination_folder, paste0(forked_owner,"_",forked_repo_name))
        
        # Clone the repository using `sprintf` for clearer formatting
        system_command <- sprintf("git clone %s %s", fork_url, paste0("'",clone_path,"'"))
        system(system_command)
        
        # Add 1 to the success counter
        success<-success+1
        
      }else{ # If there are errors
        # Construct the full path for cloning
        clone_path <- file.path(errors_folder, paste0(forked_owner,"_",forked_repo_name))
        
        if (!dir.exists(clone_path)) {
          dir.create(clone_path)
        }
        
        # Clone the repository using `sprintf` for clearer formatting
        system_command <- sprintf("git clone %s %s", fork_url, paste0("'",clone_path,"'"))
        # system(system_command)
        
        # Write the errors to a text file
        writeLines(unlist(errors), file.path(clone_path, "errors.txt"))
        
        
      }
    }
    
    output$status <- renderText(paste0("FoRkast Complete - ",success," repo(s) cloned successfully from ",length(fork_clone_urls)," forked repo(s)"))
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)

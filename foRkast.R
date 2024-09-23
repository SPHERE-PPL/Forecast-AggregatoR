library(gh)
library(stringr)


################################################################################### 
# SET CONTEST PARAMETERS.                   #######################################
###################################################################################
# Competition Repo Details
original_owner <- "TuringPPL"
repo <- "Competition-Example-Mpox"

# Specify the target file name
target_file <- "forecast.csv" 

# Specify the destination folder
destination_folder <- paste0(getwd(),"/Contest_1_Entries")

################################################################################### 
# GET THE LIST OF FORKS                     #######################################
###################################################################################

# Get the list of forks
forks_data <- as.list(gh("GET /repos/:owner/:repo/forks", owner = original_owner, repo = repo))

# Extract the clone URLs from the API response
fork_clone_urls <- lapply(forks_data, function(x) x$clone_url)

# Create folders for the cloned repos locally

errors_folder<-paste0(destination_folder,"/Errors")

# Create the destination & Error folder if it doesn't exist
if (!dir.exists(destination_folder)) {
  dir.create(destination_folder)
}

if (!dir.exists(errors_folder)) {
  dir.create(errors_folder)
}

################################################################################### 
# PERFORM CHECK AND DOWNLOAD LOOP FOR EACH REPO ###################################
###################################################################################


for (fork_url in fork_clone_urls) {
  
  owner <- strsplit(fork_url, "/")[[1]][4]
  repo_name <- str_extract(fork_url, "[^/]+\\.git$")
  repo_name <- str_remove(repo_name, "\\.git$") 
  
  # Create Error List
  errors <- list()
  
  ################################################################################### 
  # EXTRACT THE CONTENTS OF THE FORECAST FILE #######################################
  ###################################################################################
  
  # Find the contents of the submissions folder in the forked repository
  repo_contents <- gh("GET /repos/:owner/:repo/contents/submission", owner = owner, repo = repo_name)
  
  # Find the target file in the repository contents & if not there, add to errors
  file_info <- repo_contents[sapply(repo_contents, function(x) x$name == target_file)]

  if(length(file_info) == 0){
    errors <- c(errors, "Forecast file not found")
 
  }else{
  
  # Fetch the file content (assuming it's a text file)
  file_content_encoded <- gh("GET /repos/:owner/:repo/contents/submission/:path",
                             owner = owner, repo = repo_name, path = target_file)$content

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
  }
  
  ################################################################################### 
  # CHECK THE FORECAST FOR ERRORS             #######################################
  ###################################################################################
  if(length(errors)==0){
    
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



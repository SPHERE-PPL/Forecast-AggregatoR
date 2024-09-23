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

 # Check if the data.frame has the correct columns
if(!all(c("who_region", "month_star","Estimate") %in% colnames(df))){
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
  
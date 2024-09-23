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
  
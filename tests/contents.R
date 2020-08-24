# Contents Test
# 

contents.test<-function(df,column,threshold){
  
  possibilities <- names(table(df[[column]], useNA = "ifany"))
  possibilities[is.na(possibilities)]<- "NA"
  possibilities[is.nan(possibilities)] <- "NaN"
  
  missing<- setdiff(as.character(threshold),possibilities)
  extra<- possibilities[!possibilities %in% as.character(threshold)]
  
  missing<-paste(unlist(missing), collapse = " ")
  extra  <-paste(unlist(extra), collapse = " ")
  
  both <- paste(missing,extra)
  
  if (both == " "){
    result <- paste("Pass.",column, "contents are as specified in threshold.") 
  } else if(both != " "){
    if (nchar(extra) == 0){
      result<- paste("Fail.",column,"column is missing:", missing,".")
    } else if (nchar(missing) == 0){
      result<- paste("Fail.",column, "column has unexpected value(s):",extra,".")
    } else {
      result<- paste("Fail.",column, "column is missing:",missing," and has unexpected value(s):",extra,".")
    }
    
  }
  return(result)   
}


              
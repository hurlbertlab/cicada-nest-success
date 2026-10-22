# Function to create and initialize a trend table, based off a list of the names of columns
make_trend_table <- function(cols_list, rows_list = c("NA")) {
  #make trend table
  trend_table <- as.data.frame(matrix(ncol = length(cols_list), nrow = 0))
  #name the columns
  colnames(trend_table) <- cols_list
  #initiate trend table with first row, fill with NAs
  trend_table[1,] <- NA
  
  #if first column (rows_list) is not NA, fill it in
  if(rows_list[1] == "NA") {
    #do nothing
  } else {
    #add in the list that makes up the data in the first column
    for(s in 1:length(rows_list)) {
      
      trend_table[s,1] <- rows_list[s]
      
    }
  }
  
  return(trend_table)
}


#hm. I want summary plots like I have for the nestwatch data.
make_n_labels = function(df = summary_data, 
                         filter_species = "Eastern Bluebird", 
                         labs = "before/after" #options = before/after or cicada/non
) {
  
  df <- df |>
    filter(Species.Name == filter_species)
  if(labs == "before/after") {
    lab = paste0("n = ",
                 df$n[df$cicada_year == -1], 
                 ", ",
                 df$n[df$cicada_year == -0], 
                 ", ",
                 df$n[df$cicada_year == 1]
    )
  } else if(labs == "cicada/non")
    lab = paste0("n = ",
                 df$n[df$cicada_year_binary == 0], 
                 ", ",
                 df$n[df$cicada_year_binary == 1] #when cicadas have emerged
    )
}

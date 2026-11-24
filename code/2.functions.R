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


#function to add segments
add_segments <- function(df,
                         y,
                         x_displacement = 0.2,
                         y0, #string
                         y1, #string
                         signif, #string
                         color,
                         lwd = 3) {
  
  points(x = df$sp_id + x_displacement,
         y = y, 
         pch = ifelse(signif == TRUE, 16, 1),
         col = color,
         cex = 2)
  
  segments(y0 = y0, 
           y1 = y1,
           x0 = df$sp_id + x_displacement,
           x1 = df$sp_id + x_displacement,
           lwd = 4,
           lty = ifelse(signif == TRUE, "solid", "dotted"),
           col = color)
}

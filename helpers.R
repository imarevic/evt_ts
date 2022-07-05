# loads raw data from specific dir and
# merges all data to one df
load_data <- function(subdir) {
  all_files = list.files(path=subdir, pattern="*.csv")
  for (i in 1:length(all_files)) {
    all_files[[i]] <- paste0(subdir, "/",all_files[[i]])
  }
  
  files_list = lapply(all_files, read.csv)
  col_names <- c("match_id", "year", "match_num", "player1", "player2", "PointServer", "Speed_KMH", "P1DoubleFault", "P2DoubleFault")
  df_final <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(df_final) <- col_names
  
  for (i in 1:length(files_list)) {
    if ((i) %% 2 != 0) {
      df_m = files_list[i]
      df_p = files_list[i+1]
      merged_df <- merge(x=df_p,y=df_m,by="match_id",all.x=TRUE)
      merged_df <- merged_df[col_names]
      df_final <- rbind(df_final, merged_df)
    }
  }
  return(df_final)
}

# preprocesses data, so that double faults 
# and game starts are filtered and a flag for men vs. women is created
preprocess_data <- function(df) {
  # create competition flag
  df$competition <- with(df, ifelse(substr(match_num,1,1)=="1", "men", "women"))
  # remove double faults and game starts
  df <- df[ which(df$P1DoubleFault==0 & df$P2DoubleFault==0), ]
  df <- df[ which(df$Speed_KMH!=0), ]
  # return result
  cols_to_keep <- c("match_id", "year", "match_num", "competition", "player1", "player2", "PointServer", "Speed_KMH")
  df <- df[cols_to_keep]
  return(df)
}
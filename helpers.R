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
  df <- df %>% 
    mutate(
      competition = case_when(
        substr(match_num,1,1)=="1" ~ "men",
        substr(match_num,1,2)=="MS" ~ "men",
        substr(match_num,1,1)=="2" ~ "women",
        substr(match_num,1,2)=="WS" ~ "women",
        .default = "undefined"
      )
    )

  # remove double faults and game starts
  df <- df[ which(df$P1DoubleFault==0 & df$P2DoubleFault==0), ]
  df <- df[ which(df$Speed_KMH!=0), ]
  # flag server
  df <- df %>% 
    mutate(
      server = case_when(
        PointServer == 1 ~ player1,
        PointServer == 2 ~ player2
      )
    )
  # rename cols
  df <- df %>% 
    rename(
      serve_speed_kmh = Speed_KMH,
    )
  # return result
  cols_to_keep <- c("match_id", "year", "match_num", "competition", "server", "serve_speed_kmh")
  df <- df[cols_to_keep]
  return(df)
}

# harmonize misspellings
remove_duplicate_players <- function(df) {
  df$server[df$server=="NA _ NA"]<-"unknown player"  
  df$server[df$server=="Auger _ Aliassime"]<-"Auger-Aliassime" 
  df$server[df$server=="Auger-Aliassime _ NA"]<-"Auger-Aliassime"
  df$server[df$server=="Carreno _ Busta"]<-"Carreno-Busta"  
  df$server[df$server=="Carreno-Busta _ NA"]<-"Carreno-Busta"  
  df$server[df$server=="Cervantes _ Huegun"]<-"Cervantes"  
  df$server[df$server=="Cervantes _ NA"]<-"Cervantes"  
  df$server[df$server=="DiMitrov _ NA"]<-"Dimitrov"  
  df$server[df$server=="Dimitrov _ NA"]<-"Dimitrov"  
  df$server[df$server=="Dutra _ Da"]<-"Dutra-Silva"  
  df$server[df$server=="Dutra _ Silva"]<-"Dutra-Silva"  
  df$server[df$server=="Garcia _ Lopez"]<-"Garcia-Lopez"  
  df$server[df$server=="Garcia-Lopez _ NA"]<-"Garcia-Lopez"  
  df$server[df$server=="Gimeno _ Traver"]<-"Gimeno-Traver"  
  df$server[df$server=="Gimeno-Traver _ NA"]<-"Gimeno-Traver"  
  df$server[df$server=="Haider _ Maurer"]<-"Haider-Maurer"  
  df$server[df$server=="Haider-Maurer _ NA"]<-"Haider-Maurer"  
  df$server[df$server=="Martin _ Del"]<-"delMartin"  
  df$server[df$server=="Martin _ del"]<-"delMartin"  
  df$server[df$server=="McDonald _ NA"]<-"McDonald"  
  df$server[df$server=="Mcdonald _ NA"]<-"McDonald"  
  df$server[df$server=="Ramos _ Vinolas"]<-"Ramos-Vinolas"  
  df$server[df$server=="Ramos-Vinolas _"]<-"Ramos-Vinolas"  
  df$server[df$server=="Roger _ Vasselin"]<-"Roger-Vasselin"  
  df$server[df$server=="Roger-Vasselin _ NA"]<-"Roger-Vasselin"  
  df$server[df$server=="Arruabarrena _ NA"]<-"Arruabarrena"  
  df$server[df$server=="Arruabarrena-Vecino _ NA"]<-"Arruabarrena"  
  df$server[df$server=="Duque-Marino _ NA"]<-"Duque-Marino"  
  df$server[df$server=="Duque _ Marino"]<-"Duque-Marino"  
  df$server[df$server=="Lucic-Baroni _ NA"]<-"Lucic-Baroni"  
  df$server[df$server=="Lucic _ Baroni"]<-"Lucic-Baroni"  
  df$server[df$server=="Mattek _ Sands"]<-"Mattek-Sands"  
  df$server[df$server=="Mattek-Sands _ NA"]<-"Mattek-Sands"  
  df$server[df$server=="Mchale _ NA"]<-"McHale"  
  df$server[df$server=="McHale _ NA"]<-"McHale"  
  df$server[df$server=="Soler _ Espinosa"]<-"Soler-Espinosa"  
  df$server[df$server=="Soler-Espinosa _ NA"]<-"Soler-Espinosa" 
  df$server[df$server=="Suárez _ Navarro"]<-"Suarez Navarro"
  df$server[df$server=="Suarez _ Navarro"]<-"Suarez Navarro"
  df$server[df$server=="Allertova "]<-"Allertova"
  df$server[df$server=="Basilashvili "]<-"Basilashvili"
  df$server[df$server=="Berankis "]<-"Berankis"
  df$server[df$server=="Bublik "]<-"Bublik"
  df$server[df$server=="Copil "]<-"Copil"
  df$server[df$server=="de _ Bakker"]<-"deBaker"
  df$server[df$server=="De _ Bakker"]<-"deBaker"
  df$server[df$server=="de _ Minaur"]<-"deMinaur"
  df$server[df$server=="De _ Minaur"]<-"deMinaur"
  df$server[df$server=="Del _ Potro"]<-"delPotro"
  df$server[df$server=="del Potro"]<-"delPotro"
  df$server[df$server=="Djere "]<-"Djere"
  df$server[df$server=="Djere"]<-"Djere"
  df$server[df$server=="Federer "]<-"Federer"
  df$server[df$server=="Fish "]<-"Fish"
  df$server[df$server=="Williams"]<-"Williams S." 
  df$server[df$server=="Williams _ NA"]<-"Williams S." 
  
  
  df$server <- sapply(strsplit(df$server,"_ NA"), `[`, 1)
  df$server <- str_trim(df$server)
  return(df)
}

# adds verified records manually
add_verified_records <- function(df){
  # add records for men
  men_rec_df<-data.frame(server = c("Isner","Karlovic", "Raonic", "Roddick", "Lopez", "Copil", "Otte",
                                    "Dent", "del Potro", "Tursunov", "Tsonga", "Opelka", "Zapata Miralles",
                                    "Fritz", "Shelton", "Gonzalez", "Zverev", "Monfils", "Vemic", "Berettini",
                                    "Ljubicic", "Berankis", "Wawrinka", "Muray", "Jarry", "Verdasco",
                                    "Fish", "Soederling", "Vesely", "Basilashvili", "Kyrgios", "Bublik",
                                    "Federer", "Djere", "Rosenkranz"),
                         serve_speed_kmh = c(253, 251, 249.9, 249.4, 244.6, 244, 243,
                                             241, 240, 237, 237, 237, 237, 
                                             236.6, 236.6, 236, 236.6, 235, 235, 235,
                                             234, 234, 234, 233.4, 233, 232,
                                             231.7, 230.1, 230.1, 230.1, 230.1, 230.1,
                                             230, 230, 230)
                        )
  men_rec_df$competition <- "men"
  
  # add records for women
  women_rec_df<-data.frame(server = c("Lisicki","Wiliams V.", "Parks", "Tomljanovic", "Williams S.",
                                       "Gauff", "Goerges", "Schultz-McCarthy", "Kichenok", "Hradecka",
                                       "Osaka", "Groenefeld", "Ivanovic", "Allertova", "Pera", "Samsonova", "Mladenovic"),
                         serve_speed_kmh = c(210.8, 207.6, 207.6, 207.6, 207, 
                                             206, 203, 202.7, 202, 201.2, 
                                             201.2, 201.1, 201, 201, 201, 201, 200)
                        )
  women_rec_df$competition <- "women"
  
  comb_df <- rbind(df, men_rec_df, women_rec_df)
  comb_df$serve_speed_kmh <- as.numeric(comb_df$serve_speed_kmh)
  final_df = comb_df %>%
    group_by(across(all_of(c("competition", "server")))) %>% summarise(serve_speed_kmh = max(serve_speed_kmh))
  return(final_df)
}

# calculates distinct serve speeds
# top obtain ordered Xnn for speeds
calc_ordered_serve_speeds <- function(df, sub_const) {
  
  df_grpd <- df %>% 
    group_by(serve_speed_kmh) %>% 
    mutate(count = n())
  
  df_ordering <- df_grpd %>% 
    group_by(serve_speed_kmh) %>% 
    mutate(ordering = 1:n())
  
  df_ordering$serve_speed_final <- (df_ordering$serve_speed_kmh - sub_const) + 0.01*((2*df_ordering$ordering-1)/2*df_ordering$count)
  df_res <- df_ordering[c("competition", "server", "serve_speed_final")]
  return(df_res)
}

# ------------------------------------ # 
# ----- evt serve speed analysis ----- #
# ------------------------------------ # 


# ----- setup ----- #
rm(list=ls())
if (!require(tidyverse)) install.packages('tidyverse')
source('helpers.R')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # asuming rstudio is used
print(getwd())

# ----- load and preprocess data ----- #
subdir <- "data"
df_raw = load_data(subdir)
df_base = preprocess_data(df_raw)

# ----- calc max serve speed per player ----- #


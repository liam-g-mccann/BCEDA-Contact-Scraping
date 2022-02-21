# Load packages
library(readxl)
library(writexl)
library(stringr)
library(dplyr)

# Import spreadsheet with municipality/FN data from scrape
All_BC_Communities <- read_excel("All BC Communities.xlsx", 
                                 col_types = c("text", "text", "text", 
                                               "text", "skip", "skip", "skip", "skip", 
                                               "text", "text", "text", "text", "text", 
                                               "skip", "skip"))

# Import spreadsheet with CivicInfo data
CivicInfo <- read_excel("CivicInfoComplete.xlsx")

# Create a function to format names consistently
clean_name <- function(name){
  name <- as.character(name)
  type <- str_remove_all(str_extract(name, "\\([[:alpha:] ]+\\)"), "[\\(\\)]")
  pure_name <- str_remove_all(name, " \\([:alpha:]+\\)")
  new_name <- paste(type, "of", pure_name)
  if(is.na(type) | type == ""){
    return(name)
  }
  else{
    return(new_name)
  }
}

# Apply created function to CivicInfo data
CivicInfo$Community <- as.character(lapply(CivicInfo$Community, clean_name))

# Select relevant columns from CivicInfo and derive Address variables to join with other spreadsheet
RelevantCols <- CivicInfo %>% 
  select(Address, City, `Contact Name`, Title, Email) %>% 
  mutate(Postal = str_extract(Address, "[:upper:][:digit:][:upper:] *[:digit:][:upper:][:digit:]"),
         Box = str_extract(Address, "Box [:digit:]*|BOX [:digit:]*")) %>% 
  mutate(JoinCol = paste(Postal, " ", str_extract(Box, "[:digit:]+")))


# Join the 2 data sets on newly derived variable
Joined <- All_BC_Communities %>% 
  mutate(Postal = str_extract(Address, "[:upper:][:digit:][:upper:] *[:digit:][:upper:][:digit:]"),
  Box = str_extract(Address, "Box [:digit:]*|BOX [:digit:]*")) %>% 
  select(Region, `Regional District`, Postal, Box, Community, Member) %>% 
  mutate(JoinCol = ifelse(is.na(Postal), NA, paste(Postal, " ", str_extract(Box, "[:digit:]+")))) %>% 
  left_join(RelevantCols, by = "JoinCol") %>% 
  select(colnames(All_BC_Communities))

# Write joined data to excel file
write_xlsx(Joined, "/Users/liammccann/Desktop/Root/Work/BCEDA/BC_Communities.xlsx")



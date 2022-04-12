#Load the required packages
library(tidyverse)
library(dplyr)
library(reshape)

#Set the working directory 
setwd('C:/Users/mcgr323/projects/whondrs_microbes/')

#Load WHONDRS S19S data (presence/absence)
data <- read.csv('Processed_Clean_S19S_Water_Field_sediments_9-29_Data.csv')

#Load WHONDRS S19S molarity data 
mol <- read.csv('Processed_Clean_S19S_Water_Field_sediments_9-29_Mol.csv')

#Change column header 
names(data)[1] <- "Mass"

#Create ID column
data <- mutate(data, ID = row_number())
mol <- mutate(mol, ID = row_number())

#Subset mol to get df of just Mass and MolForm
mol_subset <- mol %>% select(ID, Mass, MolForm)

#Merge data and mol
df <- merge(data, mol_subset, by= "ID")

#Select wanted columns and remove NAs and 0 counts
df_subset <- df %>% 
              select(-one_of('ID', 'Mass.y')) %>% 
              select('Mass.x', 'MolForm', everything()) %>% 
              pivot_longer(!c(Mass.x, MolForm), names_to = "Site_ID", 
                           values_to = "Count") %>%
              filter(Count != 0)%>%
              na.omit()

#Create metadata columns 
rf <- rowwise(df_subset, Site_ID)

#Extract the site ID 
df <- rf %>% mutate(ID = strsplit(Site_ID, "_")[[1]][2]) %>%
  mutate(Position = case_when(
  #Create column for up, mid and downstream postion
  grepl(pattern = "ICR.U", x = Site_ID) ~ "Up",
  grepl(pattern = "ICR.M", x = Site_ID) ~ "Mid",
  grepl(pattern = "ICR.U", x = Site_ID) ~ "Up",
  grepl(pattern = "ICR.M", x = Site_ID) ~ "Mid",
  grepl(pattern = "ICR.D", x = Site_ID) ~ "Down",
  grepl(pattern = "ICR.1", x = Site_ID) ~ "Up",
  grepl(pattern = "ICR.2", x = Site_ID) ~ "Mid",
  grepl(pattern = "ICR.3", x = Site_ID) ~ "Down"
))

#Extract the sample type (water or sediment)
df$Type <- ifelse(grepl("Sed", df$Site_ID), "Sediment", "Water")

#Write df to csv file
write.csv(df,file = "S19S_mol_form_data_with_metadata.csv")



#load the needed libraries
library(tidyverse)
library(dplyr)

#Set the working directory 
setwd('C:/Users/mcgr323/projects/whondrs_microbes/')

#Load WHONDRS S19S data (presence/absence)
data <- read.csv('Processed_Clean_S19S_Water_Field_sediments_9-29_Data.csv')

#Load WHONDRS S19S molarity data 
mol <- read.csv('Processed_Clean_S19S_Water_Field_sediments_9-29_Mol.csv')

#Change column header 
names(data)[1] <- "Mass"

#Create ID column
data <- dplyr::mutate(data, ID = row_number())
mol <- dplyr::mutate(mol, ID = row_number())

#Subset mol to get df of just Mass and MolForm
mol_subset <- mol %>% select(ID, Mass, MolForm)

#Merge data and mol
df <- merge(data, mol_subset, by= "ID")

#Select upstream sediment samples 
sed.upper.data = as.data.frame(cbind(df$Mass.x, df$MolForm, df[grep("Sed_Field_ICR.U",
                                                                    colnames(df))]))

#Select midstream sediment samples 
sed.mid.data = as.data.frame(cbind(df$Mass.x, df$MolForm, df[grep("Sed_Field_ICR.M",
                                                                  colnames(df))]))

#Select downstream sediment samples 
sed.lower.data = as.data.frame(cbind(df$Mass.x, df$MolForm, df[grep("Sed_Field_ICR.D",
                                                                    colnames(df))]))

#Change column names
names(sed.upper.data)[1] <- 'Mass'
names(sed.mid.data)[1] <- 'Mass'
names(sed.lower.data)[1] <- 'Mass'
names(sed.upper.data)[2] <- 'MolForm'
names(sed.mid.data)[2] <- 'MolForm'
names(sed.lower.data)[2] <- 'MolForm'

#Select upstream water samples 
water.upper.data = as.data.frame(cbind(df$Mass.x, df$MolForm, df[grep("ICR.1",
                                                                      colnames(df))]))

#Select midstream water samples 
water.mid.data = as.data.frame(cbind(df$Mass.x, df$MolForm, df[grep("ICR.2",
                                                                    colnames(df))]))

#Select downstream water samples 
water.lower.data = as.data.frame(cbind(df$Mass.x, df$MolForm, df[grep("ICR.3",
                                                                      colnames(df))]))

#Change column names
names(water.upper.data)[1] <- 'Mass'
names(water.mid.data)[1] <- 'Mass'
names(water.lower.data)[1] <- 'Mass'
names(water.upper.data)[2] <- 'MolForm'
names(water.mid.data)[2] <- 'MolForm'
names(water.lower.data)[2] <- 'MolForm'

#Select for variables of interest
var_of_interest <- c('S19S_0069', 
                     'S19S_0070',
                     'S19S_0078', 
                     'S19S_0079',
                     'S19S_0065',
                     'S19S_0033',
                     'S19S_0007', 
                     'S19S_0008', 
                     'S19S_0032',
                     'S19S_0013',
                     'S19S_0014',
                     'S19S_0064',
                     'S19S_0054',
                     'S19S_0037',
                     'S19S_0052',
                     'S19S_0053_DS',
                     'S19S_0012',
                     'S19S_0072',
                     'S19S_0073',
                     'S19S_0084')
## SEDIMENT UPPER
#Find matches in data 
matches_sed_up <- cbind(sed.upper.data$Mass, sed.upper.data$MolForm,sed.upper.data
                 [unique(grep(paste(var_of_interest,collapse="|"), colnames
                              (sed.upper.data), value=TRUE))])
names(matches_sed_up)[1] <- 'Mass'
names(matches_sed_up)[2] <- 'MolForm'

#For each site subset, eliminate rows with 0 and format headers
for (i in colnames(matches_sed_up[3:length(matches_sed_up)])) {
  site.data = as.data.frame(cbind(matches_sed_up$Mass, matches_sed_up$MolForm,
                                  matches_sed_up[[i]]))
  site.subset = site.data[!(site.data[3]==0),]
  names(site.subset)[1] <- 'Mass'
  names(site.subset)[2] <- 'MolForm'
  names(site.subset)[3] <- 'Count'
  write.csv(site.subset,file=paste0(i,".csv"),row.names = FALSE)
}

## SEDIMENT MID
#Find matches in data 
matches_sed_mid <- cbind(sed.mid.data$Mass, sed.mid.data$MolForm,sed.mid.data
                 [unique(grep(paste(var_of_interest,collapse="|"), colnames
                              (sed.mid.data), value=TRUE))])
names(matches_sed_mid)[1] <- 'Mass'
names(matches_sed_mid)[2] <- 'MolForm'

#For each site subset, eliminate rows with 0 and format headers
for (i in colnames(matches_sed_mid[3:length(matches_sed_mid)])) {
  site.data = as.data.frame(cbind(matches_sed_mid$Mass, matches_sed_mid$MolForm, 
                                  matches_sed_mid[[i]]))
  site.subset = site.data[!(site.data[3]==0),]
  names(site.subset)[1] <- 'Mass'
  names(site.subset)[2] <- 'MolForm'
  names(site.subset)[3] <- 'Count'
  write.csv(site.subset,file=paste0(i,".csv"),row.names = FALSE)
}

## SEDIMENT LOW
#Find matches in data 
matches_sed_low <- cbind(sed.lower.data$Mass, sed.lower.data$MolForm,sed.lower.data
                         [unique(grep(paste(var_of_interest,collapse="|"), colnames
                                      (sed.lower.data), value=TRUE))])
names(matches_sed_low)[1] <- 'Mass'
names(matches_sed_low)[2] <- 'MolForm'

#For each site subset, eliminate rows with 0 and format headers
for (i in colnames(matches_sed_low[3:length(matches_sed_low)])) {
  site.data = as.data.frame(cbind(matches_sed_low$Mass, matches_sed_low$MolForm, 
                                  matches_sed_low[[i]]))
  site.subset = site.data[!(site.data[3]==0),]
  names(site.subset)[1] <- 'Mass'
  names(site.subset)[2] <- 'MolForm'
  names(site.subset)[3] <- 'Count'
  write.csv(site.subset,file=paste0(i,".csv"),row.names = FALSE)
}

## WATER UPPER
#Find matches in data 
matches_wat_up <- cbind(water.upper.data$Mass, water.upper.data$MolForm,
                        water.upper.data[unique(grep(paste(var_of_interest,collapse="|"), colnames
                                     (water.upper.data), value=TRUE))])
names(matches_wat_up)[1] <- 'Mass'
names(matches_wat_up)[2] <- 'MolForm'

#For each site subset, eliminate rows with 0 and format headers
for (i in colnames(matches_wat_up[3:length(matches_wat_up)])) {
  site.data = as.data.frame(cbind(matches_wat_up$Mass, matches_wat_up$MolForm,
                                  matches_wat_up[[i]]))
  site.subset = site.data[!(site.data[3]==0),]
  names(site.subset)[1] <- 'Mass'
  names(site.subset)[2] <- 'MolForm'
  names(site.subset)[3] <- 'Count'
  write.csv(site.subset,file=paste0(i,".csv"),row.names = FALSE)
}

## WATER MID
#Find matches in data 
matches_wat_mid <- cbind(water.mid.data$Mass, water.mid.data$MolForm,
                        water.mid.data[unique(grep(paste(var_of_interest,collapse="|"), colnames
                                                     (water.mid.data), value=TRUE))])
names(matches_wat_mid)[1] <- 'Mass'
names(matches_wat_mid)[2] <- 'MolForm'

#For each site subset, eliminate rows with 0 and format headers
for (i in colnames(matches_wat_mid[3:length(matches_wat_mid)])) {
  site.data = as.data.frame(cbind(matches_wat_mid$Mass, matches_wat_mid$MolForm,
                                  matches_wat_mid[[i]]))
  site.subset = site.data[!(site.data[3]==0),]
  names(site.subset)[1] <- 'Mass'
  names(site.subset)[2] <- 'MolForm'
  names(site.subset)[3] <- 'Count'
  write.csv(site.subset,file=paste0(i,".csv"),row.names = FALSE)
}


## WATER LOW
#Find matches in data 
matches_wat_low <- cbind(water.lower.data$Mass, water.lower.data$MolForm,
                         water.lower.data[unique(grep(paste(var_of_interest,collapse="|"), colnames
                                                    (water.lower.data), value=TRUE))])
names(matches_wat_low)[1] <- 'Mass'
names(matches_wat_low)[2] <- 'MolForm'

#For each site subset, eliminate rows with 0 and format headers
for (i in colnames(matches_wat_low[3:length(matches_wat_low)])) {
  site.data = as.data.frame(cbind(matches_wat_low$Mass, matches_wat_low$MolForm,
                                  matches_wat_low[[i]]))
  site.subset = site.data[!(site.data[3]==0),]
  names(site.subset)[1] <- 'Mass'
  names(site.subset)[2] <- 'MolForm'
  names(site.subset)[3] <- 'Count'
  write.csv(site.subset,file=paste0(i,".csv"),row.names = FALSE)
}
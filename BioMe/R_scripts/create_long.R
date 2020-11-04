# Script that organizes longitudinal data within the BioMe BioBank program.
# Steps:
# 1. read the .txt tables (i.e., Social_History, Encounter_Diagnosis,
# Vitals, Medications)
# 2. include all terms adding a column with a term description

# Required libraries
require(data.table)
require(dplyr)
require(tidyr)
require(stringr)

# Data path
data_path <- ""
# File names
subj_list_file <- "masterrgnidlist.csv"
social_history_file<- "Social_History.txt"
diagn_file <- "Encounter_Diagnosis.txt"
vitals_file <- "Vitals.txt"
med_file <- "Medications.txt"

# Import data
subj_df <- fread(file.path(data_path, subj_list_file), sep = "|",
                 stringsAsFactors = FALSE, header = TRUE)
sh_df <- fread(file.path(data_path, social_history_file),
               sep = "|", stringsAsFactors = FALSE, header = TRUE)
diag_df <- fread(file.path(data_path, diagn_file),
               sep = "|", stringsAsFactors = FALSE, header = TRUE)
vitals_df <- fread(file.path(data_path, vitals_file),
               sep = "|", stringsAsFactors = FALSE, header = TRUE)
med_df <- fread(file.path(data_path, med_file),
               sep = "|", stringsAsFactors = FALSE, header = TRUE)


#############################################
# Create longitudal dataset: long orientation
#############################################

# Order tables wrt subject id and encounter date
setorder(vitals_df, rgnid, encounter_date)
setorder(diag_df, rgnid, encounter_date)
setorder(med_df, rgnid, encounter_date)
setorder(sh_df, rgnid, encounter_date)

# Select columns and row-bind datasets

# VITALS: create new column ("vs_code") pasting vitals and unit of measure
# Select: vs_code, vital_sign_description
# Add label column
vitals_df$vs_code <- paste(vitals_df$vital_sign_measurement, 
                           vitals_df$vital_sign_unit_of_measure, sep="::")
vitals_rid <- vitals_df[, .(rgnid, encounter_date, 
                            code=vs_code,
                            description=vital_sign_description)]
vitals_rid$label <- rep("vitals", nrow(vitals_rid))

# DIAGNOSIS
diag_df$dx_code <- paste(diag_df$dx_code1, 
                         diag_df$dx_code_type, sep = "::")
diagn_rid <- diag_df[, .(rgnid, encounter_date,
                          code = dx_code,
                          description = dx_name)]
diagn_rid$label <- rep("diagnosis", nrow(diagn_rid))

# SOCIAL HISTORY
sh_2 <- data.table::melt(sh_df[,.(rgnid, encounter_date, 
                           smoking=TOBACCO_USER, 
                           alcohol=IS_ALCOHOL_USER, 
                           drugs=IS_ILL_DRUG_USER, 
                           sex=SEXUALLY_ACTIVE)],
                           id.vars = c('rgnid','encounter_date'),
                           variable.name = 'description',
                           value.name = 'code')

sh_rid<-sh_2[, .(rgnid, encounter_date, code, description)]

sh_rid$label <- rep('sh', nrow(sh_rid))

# MEDICATIONS
med_rid <- med_df[, .(rgnid, encounter_date, 
                            code=DESCRIPTION,
                            description=SIG)]
med_rid$label <- rep("meds", nrow(med_rid))

# Merge all
df_long<-rbind(diagn_rid, vitals_rid, sh_rid, med_rid)

setorder(df_long, rgnid, encounter_date)
nrow(df_long) #30248776

what_na <- df_long[is.na(code),]

#drop 177479 NA's in sh
df_long_c<- df_long[!is.na(code),]

# Save complete 
fwrite(df_long_c, sep="|", 
       file= file.path(data_path,"longitudinal_assessments.txt"))
# Save EHRs for patients with at least two encounters and that have been 
# in the EHR system for at least a year

long_df_co <- df_long_c[rgnid %in% subj_df$rgnid,]

setorder(long_df_co, rgnid, encounter_date)
long_df_cohort<-long_df_co[!duplicated(long_df_co), ]

fwrite(long_df_cohort, sep="|", 
       file= file.path(data_path,"longitudinal_assessments.txt"))



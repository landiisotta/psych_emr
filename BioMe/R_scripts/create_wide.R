# Script that organizes data collected once within the BioMe BioBank program.
# Steps:
# 1. read the .txt tables (i.e., Questionnaire, Demographics)
# 2. drop columns that have more than 50% information missing
# 3. selected columns 
# 4. concatenate the different data by column into a unique dataframe

# Required libraries
require(data.table)
require(dplyr)
require(tidyr)
require(stringr)

# Data paths
data_path <- ""
# Files
subj_list_file <- "masterrgnidlist.csv"
quest_file <- "Questionnaire.txt"
demographics_file <- "Demographics.txt"

#Import data
# List of subjects that have been in EHR for at 
# least 1 year and has at least 2 visits (or encounters? Is it different?).
subj_df <- fread(file.path(data_path, subj_list_file), sep="|",
            stringsAsFactors=FALSE, header=TRUE)
# Questionnaire
quest_df <- fread(file.path(data_path, quest_file), sep="|",
                  stringsAsFactors=FALSE, header=TRUE)
# Social history
sochis_df <- fread(social_history_path, sep="|",
                   stringsAsFactors=FALSE, header=TRUE)
# Demographics
demo_df <- fread(demographics_path, sep="|",
                 stringsAsFactors=FALSE, header=TRUE)

#########################################
# Wide dataset (variables collected once)
#########################################
# Drop duplicated rows
quest_df <- quest_df[!duplicated(quest_df), ]
#length(which(duplicated(quest_df))) == 0

#Number of repeated subjects (id column "rgnid")
count_id<-quest_df[,.N,rgnid][order(-N)]
table(count_id$N) 
#    1      2     3
#  31551   294    11

# Why subjects have N>2 questionnaire entries? 
# Was a new version of the questionnaire administered? (i.e., after 2012?)
# We should probably consider the most complete entries instead of baseline data.
# Select the entry that maximizes the available information.
repeated_id <- count_id[N>=2]$rgnid
quest_repeated <- quest_df[rgnid %in% repeated_id] 
setorder(quest_repeated, rgnid, SPECIMEN_COLLECTION_DATE_TIME)
repeated_id$na_counts <- rowSums(is.na(quest_repeated))
repeated_id <- repeated_id[repeated_id[, .I(which.min(count_na)), rgnid]$V1]

# Drop subjects with repeated questionnaires and add the selected ones
quest_df <- quest_df[!rgnid %in% repeated_id]
quest_df <- rbind(quest_df, repeated_id[,!"count_na"])
setorder(quest_df, rgnid, ENROLLMENT_DATE, 
         SPECIMEN_COLLECTION_DATE_TIME) 
nrow(quest_u) #1856, yes this= uniqueN(quest$rgnid) 

# Add demographic information (drop duplicates and check if repeated info exist)
# and drop columns with more than 50% of
# data missing
demo_df <- demo_df[!duplicated(demo_df)]
#length(which(duplicated(demo_df)))

# Check of repeated observation exist (e.g., marital status can change)
table(demo_dfp[,.N,rgnid][order(-N)])

# if not...
dt_all = merge(quest_df, demo_df, by="rgnid", all=TRUE) 
#nrow(dt_all): 32300

#keep only columns with <50% missing data
id_col <- names(dt_all)[which(colSums(is.na(dt_all))<(nrow(dt_all)/2))]
dt_all <- dt_all[, ..id_col]
#Check which columns satisfy the condition
# names(dt_all)

# Save complete dataset with non-repeated measures
fwrite(dt_all, sep="|", 
       file= file.path(data_path,"single_assessments.txt"))
# Save dataset selecting only subjects with at least 2 visits 
# and that have been in EHR at least a year.
dt_rid <- dt_all[rgnid %in% subj_list_file$rgnid]
nrow(dt_rid) # 27837
fwrite(dt_rid, sep="|", 
       file= file.path(data_path, "single_assessments_cohort.txt"))

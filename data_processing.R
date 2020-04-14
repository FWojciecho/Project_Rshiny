lab_data <- read.table(file='Data\\Random_LabValuesInfo_2020.tsv', header = T, stringsAsFactors = F)
pat_data <- read.table(file='Data\\Random_PatientLevelInfo_2020.tsv', header = T, stringsAsFactors = F)


# replacing broken data
pat_data <- pat_data  %>% mutate(SEX = ifelse(SEX %in% c("F", "M"), SEX, "No data"))

# replacing F with Female and M with Male
pat_data <- pat_data %>% mutate(SEX = ifelse(SEX == "F", "Female", 
                                             ifelse(SEX == "M", "Male", SEX)))


# creating needed vectors
unique_age <- unique(pat_data$AGE)
unique_treatment <- unique(pat_data$ACTARM)
unique_race <- unique(pat_data$RACE)
unique_sex <- unique(pat_data$SEX)
unique_LBTEST <- unique(lab_data$LBTEST)
unique_pat <- unique(lab_data$USUBJID)


# removing unecessery (for analysis) observation
lab_data <- lab_data %>% filter(AVISIT %in% c("WEEK 5 DAY 36", "BASELINE")) 

# creating table
dt_temp <- data.frame(matrix(data= 1, nrow = length(unique_pat)*length(unique_LBTEST) ,ncol = length(unique_LBTEST)))

# filling table with relative diffrences between baseline LBTEST and latest test
colnames(dt_temp) <- c("pat_id", "LBTEST", "diffrence")                      
vec_temp_2 <- length(unique_pat)

for(i in 1:vec_temp_2){
  
  dt_temp$pat_id[i] <- unique_pat[i]
  dt_temp$LBTEST[i] <- unique_LBTEST[1]
  dt_temp$diffrence[i] <- lab_data %>% filter(USUBJID == unique_pat[i]) %>% 
    filter(LBTEST == unique_LBTEST[1]) %>% dplyr::arrange(AVISIT) %>%  mutate(DIFFRENCE = (last(AVAL)-first(AVAL))/first(AVAL)*100) %>% 
    select(DIFFRENCE) %>% filter(row_number()==1) %>% c()  
  
  dt_temp$pat_id[i + vec_temp_2] <- unique_pat[i]
  dt_temp$LBTEST[i + vec_temp_2] <- unique_LBTEST[2]
  dt_temp$diffrence[i + vec_temp_2]<- lab_data %>% filter(USUBJID == unique_pat[i]) %>% 
    filter(LBTEST == unique_LBTEST[2]) %>% dplyr::arrange(AVISIT) %>%  mutate(DIFFRENCE = (last(AVAL)-first(AVAL))/first(AVAL)*100) %>% 
    select(DIFFRENCE) %>% filter(row_number()==1) %>% c()  
  
  dt_temp$pat_id[i + 2*vec_temp_2] <- unique_pat[i]
  dt_temp$LBTEST[i + 2*vec_temp_2] <- unique_LBTEST[3]
  dt_temp$diffrence[i + 2*vec_temp_2]<- lab_data %>% filter(USUBJID == unique_pat[i]) %>% 
    filter(LBTEST == unique_LBTEST[3]) %>% dplyr::arrange(AVISIT) %>%  mutate(DIFFRENCE = (last(AVAL)-first(AVAL))/first(AVAL)*100) %>% 
    select(DIFFRENCE) %>% filter(row_number()==1) %>% c()  
  
}

dt_temp$diffrence <- unlist(dt_temp$diffrence) 

###########################
## WHO MM Final Analysis 
## Analyst: Kristin Andrejko
## kristin_andrejko@berkeley.edu
## UC Berkeley School of Public Health
## Updated: 9/ 05/ 2020 
###########################

###################
###Load libraries##
###################

library(tidyverse)
library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(Hmisc)
library(gridExtra)
library(table1)
library(formattable)
library(reshape)
library(reshape2)
library(kable)
library(kableExtra)
library(ggsci)

##################
###Load data#####
##################

dta <- read_excel("/Users/kristinandrejko/Box/01-Research/01-WHO-MM/WHO-Models/data/MM_Data_R_090320_final3.xlsx") 
totaldta <- read_excel("/Users/kristinandrejko/Box/01-Research/01-WHO-MM/WHO-Models/data/MM_TotalGuidelines.xlsx") 

####################
##### TABLE 1 ######
####################

#1. For recommendations -------------------
length(which(dta$grade_etd >0)) #EtD Framework (45 Recs) 
length(which(dta$grade_eprofile == "Y")) #GRADE Evidence Profile/ Table Framework (12 Recs)
length(which(dta$grade_eprofile == "Y*")) #GRADE Summary of Evidence Section (23 Recs)
length(which(dta$grade_eprofile == "N" & dta$grade_etd == 0)) #GRADE Supplemental Sectoin In Text (21 recs )

#2. For guidelines  -------------------
# 2.1. Guidelines with GRADE Evidence-to-Decision Framework
tb1.1 <- dta %>% 
  filter(grade_etd >0)
length(unique(tb1.1$grc)) #27

# 2.2. Guidelines with GRADE Evidence Profile/ Table 
tb1.2 <- dta %>% 
  filter(dta$grade_eprofile == "Y")
length(unique(tb1.2$grc)) #5

# 2.3. Guidelines with GRADE Summary of Evidence Section
tb1.3 <- dta %>% 
  filter(dta$grade_eprofile == "Y*")
length(unique(tb1.3$grc)) #13

# 2.4. Guidelines with Supplemental Section  
tb1.4 <- dta %>% 
  filter(dta$grade_eprofile == "N" & dta$grade_etd == 0)
length(unique(tb1.4$grc)) #8


#################################################################
##### Results #####
#################################################################

###Paragraph 1###

##1. Count the total number of guidelines and recommendations- regardless of MM status##

#1a. Total number of recommendations published- filtered by "Y" in totaldta df
totaldta_yonly <- totaldta %>%
  filter(include_guideline == "y")

total_recs <- sum(as.numeric(totaldta_yonly$rec_count), na.rm= TRUE) #1619- total recommendations published from ALL guidelines

#1b. Find total number of guidelines published between 2008 - 2019 
total_guidelines <- length(which(totaldta$include_guideline=="y")) 

#2. Count total number of unique guidelines and recommendations which include MM##
guidelines_mm <- length(unique(dta$grc)) #46- GRC approved guidelines that included a MM 
rec_mm <- length(unique(dta$rec_id)) #101- guideline recommendations that included MM

#3. Find the avg number of mathematical models per recommendation
mean(dta$mm_studies_row) #1.6
length(which(dta$mm_studies_row >= 2))#28 recs that are cited by 2 or more MM

#3.1 NEW: add median and range number of modeling studies per recommendation 
median(dta$mm_studies_row) #1
range(dta$mm_studies_row) #1 -> 9 

#4. Calculate prevalence
mm_guideline_prev <- (guidelines_mm /total_guidelines) * 100 #29.9% prev of MM in guidelines
mm_rec_prev <- (rec_mm / total_recs) * 100 # 6.24% prev of MM for total recs 

# FOR INFO ABOUT WHERE MODELS WERE CITED, SEE SECTION ON TABLE 1 # 
unique_grc <- distinct(dta, grc, .keep_all = TRUE) 
library(xlsx)
write.csv(unique_grc, file = "/Users/kristinandrejko/Box/01-Research/01-WHO-MM/WHO-Models/output/unique_grc.csv") 


#5. Find the most common decision criteria 

#5.1: First we need to get a count of the number of studies who reported decision criteria for the denominator 

#decision.criteria.count <- count(dta$grade_criteria)

decision.criteria.count <- dta %>%
  group_by(grade_criteria) %>%
  dplyr::summarise(n()) 
decision.criteria.count

#5.2: Next we need to look at the number of NR or not clear for grade_criteria  
df_decision.criteria.count <- data.frame(decision.criteria.count)
no_no.decision.criteria <- 52 #manually added at number of of NR

#5.3 Calculate the prev of rec that included modeling evidence in EtD Framework
prev_resource.implication <- (36/etd_rec_total) * 100
prev_benefit.harms <- (11/etd_rec_total) * 100
prev_feasibility <- (1/etd_rec_total) * 100
prev_testaccuracy <- (1/etd_rec_total) * 100

###Paragraph 2###

#1.0 First create Table 2- evidence quality by strength 

###################
######Table 2######
###################

#1.1. Data Cleaning (Combine very low to low into very low, and repleace weak w/ conditional)
dta[dta$evidence_quality == "Very low", "evidence_quality"] <- "Very Low" #replace "Very low" with Very Low
dta[dta$evidence_quality == "Very low to low", "evidence_quality"] <- "Very Low" #replace "Very low" with Very Low
dta[dta$evidence_strength == "Weak", "evidence_strength"] <- "Conditional" #replace "Weak" with "Conditional"

#1.2 Re-organize the order of the covariates that appear in the table
dta$evidence_quality_2 <- factor(dta$evidence_quality, 
                                 levels = c("Very Low", "Low", "Moderate",
                                            "High"))

dta$evidence_strength_2 <- factor(dta$evidence_strength, 
                                  levels = c("Conditional", "Strong")) 

#1.3 Create a label for the X column (quality of evidence)
label(dta$evidence_quality_2) <- "Quality of evidence (overall)"
label(dta$evidence_strength_2) <- "Strength of Recommendation"

#1.4 Use Table1 Package to create table using data from uniquerec
quality_v_strength <- table1(~evidence_quality_2 | evidence_strength_2, data=dta)
quality_v_strength

#1.5  update to stratify model-informed recommendation by topic & count for abstract # of ID vs non-ID

#First create new topic variable infectious, HIV/TB, noninfectious
dta$topic_cl <- ifelse(dta$topic == "HIV" | dta$topic == "TB", "HIV/TB", 
                       ifelse(dta$topic == "Hepatitis B or C" | dta$topic == "Malaria" | dta$topic == "Neglected tropical diseases" | 
                                dta$topic == "Reproductive health" | dta$topic == "Other infectious diseases and IPC" | dta$topic == "Infection Prevention and Control", 
                              "Infectious Disease", "Non-infectious Disease"))
table(dta$topic, dta$topic_cl)

# For ABSTRACT: Find the total number of guidelines and recommendations addressing topics in infectious disease 
unique_grc$topic_cl <- ifelse(unique_grc$topic == "HIV" | unique_grc$topic == "TB", "HIV/TB", 
                              ifelse(unique_grc$topic == "Hepatitis B or C" | unique_grc$topic == "Malaria" | unique_grc$topic == "Neglected tropical diseases" | 
                                       unique_grc$topic == "Reproductive health" | unique_grc$topic == "Other infectious diseases and IPC" | unique_grc$topic == "Infection Prevention and Control", 
                                     "Infectious Disease", "Non-infectious Disease"))
table(unique_grc$topic, unique_grc$topic_cl)

length(which(unique_grc$topic_cl == "Infectious Disease" |unique_grc$topic_cl == "HIV/TB")) #number of GL with topics in ID: 38
length(which(dta$topic_cl == "Infectious Disease" |dta$topic_cl == "HIV/TB")) #number of recs with topics in ID: 81 


quality_v_strength_topic <- table1(~evidence_quality_2 | evidence_strength_2*topic_cl, data=dta)
quality_v_strength_topic

quality_v_strength_topic <- table1(~evidence_quality_2 | topic_cl*evidence_strength_2, data=dta)
quality_v_strength_topic

#If they want "other evidnece"
quality_v_strength_otherevid <- table1(~evidence_quality_2 | other_evidence*evidence_strength_2, data=dta)
quality_v_strength_otherevid

#2.0 Next, calculate n and % for different quality of evidence ratings

#2a. Low to very low evidence
target <- c("Low", "Very Low", "Very low to low")
ltvl_dta_df <- dta %>%
  filter(evidence_quality %in% target)
low_verylow_num= length(ltvl_dta_df$evidence_quality)
prev_low_verylow = (low_verylow_num/ rec_mm) * 100

#2b. Moderate quality of evidence
moderate_dta_df <- dta %>%
  filter(evidence_quality == "Moderate")
moderate_num = length(moderate_dta_df$evidence_quality)
prev_mod= (moderate_num/ rec_mm) * 100

#2c. High quality of evidence
high_dta_df <- dta %>%
  filter(evidence_quality == "High")
high_num = length(high_dta_df$evidence_quality)
prev_high= (high_num/ rec_mm) * 100

#3. Calculate n and % for different Strength of Rec

#3.1 Conditional 
conditional_dta_df <- dta %>%
  filter(evidence_strength == "Conditional")
conditional_num = length(conditional_dta_df$evidence_strength)
prev_cond= (conditional_num/ rec_mm) * 100

#3.2 Strong 
strong_dta_df <- dta %>%
  filter(evidence_strength == "Strong")
strong_num = length(strong_dta_df$evidence_strength)
prev_strong= (strong_num/ rec_mm) * 100

#3.3 Low and Very Low Evidence in Strong Recs 
ltvl_strong <- c("Low", "Very Low")
ltvl_dta_df_strong <- dta %>%
  filter(evidence_quality %in% ltvl_strong) %>%
  filter(evidence_strength == "Strong")
low_vl_strong_no= length(ltvl_dta_df_strong$evidence_strength)
prev_low_vl_strong = (low_vl_strong_no/ strong_num) * 100

###Paragraph 3###

#1. N and % of studies that reported model assumptions 
prev_assumption <- (mean(dta$model_assumptions)) * 100 

#2. N and % of studies that reported sensitivity  
prev_sensitivity <- (mean(dta$model_sensitivity)) * 100 

#3. N and % of studies that reported model external validation  
prev_validation <- (mean(dta$model_validation)) * 100 

###Paragraph 4###

#Part 1- Infectious Disease#
#1. Create a new column indicating whether topic is ID or Chronic
#1a. Create a vector for the infectious diseases to be re-labeled as "1"

dta$topic[dta$topic=="TB"] <- "Tuberculosis"

inf_disease <- c("Malaria", "Hepatitis B or C", "HIV", 
                 "Neglected Tropical diseases", "Other infectious diseases and IPC", 
                 "Infection Prevention and Control", "Tuberculosis", "Neglected tropical diseases", 
                 "Reproductive Health")

#1b. Use mutate and ifelse to create new variable where infectious disease are labeled as 1
dta <- dta %>%
  mutate(topic_id= ifelse(topic %in% inf_disease, 1, 0))

#1c. Check that relabels worked for topic_id ("topic_infectious disease"=1)
topic_id_topic<- dta %>%
  select(topic_id, topic)
View(topic_id_topic)

#2. Count # and % of infectious disease recommendations 
no_id_yes <- length(which(dta$topic_id== 1))
prev_id_yes <- (no_id_yes / rec_mm) * 100

#3. Look at descriptive count of topics, to see HIV and TB have the highest prev
topic.count2 <- dta %>%
  group_by(topic) %>%
  dplyr::summarise(n()) 
topic.count2

#repeat at the GL level
unique_dta <- unique_dta %>%
  mutate(topic_id= ifelse(topic %in% inf_disease, 1, 0))

#1c. Check that relabels worked for topic_id ("topic_infectious disease"=1)
topic_id_topic<- unique_dta %>%
  select(topic_id, topic)
View(topic_id_topic)

#2. Count # and % of infectious disease guidelines 
no_id_yes_grc <- length(which(unique_dta$topic_id== 1))
prev_id_yes_grc <- (no_id_yes_grc / nrow(unique_dta)) * 100


#3. Look at descriptive count of topics, to see HIV and TB have the highest prev
topic.count <- dta %>%
  group_by(topic) %>%
  summarise(n()) 
topic.count

#3a. Create a vector for HIV and TB from topic cell 
HIV_TB <- c("HIV", "TB")

#3b. Filer to create a new variable called HIV_TB in datafram hiv_tb_df 
#where there is a 1 in column "hiv_tb_only" if it meets restrictions
hiv_tb_df <- dta %>%
  mutate(hiv_tb_only= ifelse(topic %in% HIV_TB, 1, 0))

#3c. Find the length of the hiv_tb dataset 
hiv_tb_length <- length(which(hiv_tb_df$hiv_tb_only== 1))

#3d. Calculate the prevalence of hiv_tb out of total ID papers!
prev_hiv_tb <- (hiv_tb_length / no_id_yes ) * 100

#4. Calculate total HIV and total TB guideline recs published 2008 to 2019
totaldta_hiv <- totaldta %>%
  filter(include_guideline == "y") %>% 
  filter(topic == "HIV")

totaldta_tb <- totaldta %>%
  filter(include_guideline == "y") %>% 
  filter(topic == "TB")

#4.1 Sum the rec count in each guideline
sum(as.numeric(totaldta_hiv$rec_count))
sum(as.numeric(totaldta_tb$rec_count))

#4.2 Filter total HIV and total TB guideline recs w/ MM
dta_hiv <- dta %>%
  filter(topic == "HIV")

dta_tb <- dta %>%
  filter(topic == "Tuberculosis")

#4.3 calculate prevalences 
prev_hiv <- (nrow(dta_hiv) / sum(as.numeric(totaldta_hiv$rec_count))) * 100
prev_tb <- (nrow(dta_tb) / sum(as.numeric(totaldta_tb$rec_count))) * 100 


#Part 2- Non-communicable disease#

#1a. Count the number of topics that were NOT considered ID 
no_id_no <- length(which(dta$topic_id== 0))
prev_chronic <- (no_id_no/ rec_mm) * 100

#Part 3- Questions Answered#

#1. Create a table that summarizes counts for key questions
#keyquestion.count <- count(dta$key_question)
key_question.count <- dta %>%
  group_by(key_question) %>%
  dplyr::summarise(n())
keyquestion.df <- data.frame(key_question.count)
names(keyquestion.df)[2] <- "freq"

#2. Calculate the n and % of the intervention effects 
#combine "treatment intervention effect" and "prevention or screening intervention effect"
intervention_effect_count <- 37 + 31
prev_intervention_effect <- (intervention_effect_count/ rec_mm) * 100 

#3. Calculate n and % of diagnostic test accuracy or threshold 
diagnostic_test_accuracy_count <- 17
prev_diagnostic_test_accuracy <- (diagnostic_test_accuracy_count/ rec_mm) * 100

#4. Create bar chart on key_question addressed

order <- c("Treatment intervention effect", "Prevention or screening intervention effect", 
           "Diagnostic test accuracy or threshold", "Prognosis, risk assessment", 
           "Disease transmission", "Economic")

fig_keyquestion_bar <- ggplot(data=keyquestion.df, aes(x= key_question, y=freq))+
  geom_bar(position= "dodge", stat="identity")+
  ggtitle("WHO Guideline Recommendations with Mathematical Modeling by Key Question")+
  ylab("Number of Guideline Recommendations with Mathematical Models")+
  xlab("")+
  scale_x_discrete(limits = order)+ #re-order from low -> high 
  coord_flip()+
  theme_minimal(base_size = 15) 
fig_keyquestion_bar

###Paragraph 5###

#0. Data cleaning
library(plyr)
dta$model_origin <- revalue(dta$model_origin, c("De Novo"="De novo MM"))
dta$model_origin <- revalue(dta$model_origin, 
                            c("Both existing and de novo referenced for same topic"
                              ="De novo MM and Existing"))


#1. Summarise the type of model and whether it was de novo or existing 

existing_mm_count <- length(which(dta$model_origin== "Existing MM"))
(existing_mm_count/ rec_mm) * 100
denovo_mm_count <- length(which(dta$model_origin== "De novo MM"))
(denovo_mm_count/ rec_mm) * 100
denovo_existing_mm_count <- length(which(dta$model_origin== "De novo MM and Existing")) 
(denovo_existing_mm_count/ rec_mm ) * 100

dta_denovo <- dta %>% 
  filter(model_origin == "De novo MM")

dta_existing <- dta %>% 
  filter(model_origin == "Existing MM")

mean(dta_denovo$model_sensitivity)
mean(dta_denovo$model_assumptions)
mean(dta_denovo$model_validation)

mean(dta_existing$model_sensitivity)
mean(dta_existing$model_assumptions)
mean(dta_existing$model_validation)

#find number with any report of model sensitivity 
length(which((dta_denovo$model_sensitivity>=0.1))) #43/ 46 (commisioned)
length(which((dta_existing$model_sensitivity>=0.1))) #20/48 (existing)
proptest_sensitivity <- prop.test(x = c(43, 20), n = c(46, 48))

#find number with any report of model assumptions out of total recommendations
length(which((dta$model_assumptions>=0.1)))
length(which((dta$model_sensitivity>=0.1)))
length(which((dta$model_validation>=0.1)))

#find number with any report of model assumptions 
length(which((dta_denovo$model_assumptions>=0.1))) #45/ 46
length(which((dta_existing$model_assumptions>=0.1))) #41/48
proptest_assumptions <- prop.test(x = c(45, 41), n = c(46, 48))

#find number with any report of validation 
length(which((dta_denovo$model_validation>=0.01)))
length(which((dta_existing$model_validation>=0.01)))

proptest_validation <- prop.test(x = c(1, 0), n = c(52, 50))

#################################################################
##### Figures #####
#################################################################

##########
#Figure 2#
##########

#Create a data frame with pub_year, num_grc, total_rec, mm_total_rec, mm_total_grc, total_mm

#1. Create a data frame with info at the GRC level (for guideline counts)
unique_grc <- distinct(dta, grc, .keep_all = TRUE) 

#1.1 Create a matrix 
matrix_countdta <- matrix(nrow=12, ncol=5)

#1.2 Create vector for pub_years
pub_years <- (2008:2019)

#2. Create for loop 
for (i in 2008:2019){
  unique_filtered_data <- unique_grc %>%
    filter(pub_date == i) 
  
  #matrix_countdta[i-2007, 1] <- length(unique(unique_grc$gl_id)) #unique guidelines per year
  #matrix_countdta[i-2007, 2] <- length(unique(unique_grc$rec_id)) #unique recommendations per year
  matrix_countdta[i-2007, 1] <- sum(unique_filtered_data$mm_studies_total) #unique models 
  
  filtered_dta <-  dta %>%
    filter(pub_date == i)
  
  matrix_countdta[i-2007, 2] <- length(unique(filtered_dta$gl_id)) #Guidelines w/ MM per year
  matrix_countdta[i-2007, 3] <- length(unique(filtered_dta$rec_id)) #Recommendations w/ MM per year
  
  totaldta_yonly <- totaldta %>%
    filter(include_guideline == "y") %>%
    filter(pub_year == i)
  
  matrix_countdta[i-2007, 4] <- length(unique(totaldta_yonly$grc)) #Total Guidelines per Year 
  matrix_countdta[i-2007, 5] <- sum(as.numeric(totaldta_yonly$rec_count)) #Total Recommendations per year 
}

#3. Convert to data frame
df_countdta <- data.frame(matrix_countdta)

#3.1 Merge with vector of years 
df_countdta <- cbind(df_countdta, pub_years)

#3.2 Rename columns
names(df_countdta)[1] <- "num_models"
names(df_countdta)[2] <- "mm_guidelines"
names(df_countdta)[3] <- "mm_rec"
names(df_countdta)[4] <- "num_guidelines"
names(df_countdta)[5] <- "num_rec"

#4. Create Figure 2, Panel A

#4.1 Create subset of data with just year, mm, mm_rec, mm_grc
df_countdta_fig2_pa <- df_countdta %>%
  select(pub_years, num_models, mm_rec, mm_guidelines)

#4.2 Melt data 
df_countdta_fig2_pa_melt <- melt(data = df_countdta_fig2_pa, 
                                 id.vars= c("pub_years"), 
                                 measure.vars= c("num_models", "mm_rec", "mm_guidelines"), 
                                 variable.name = "type", 
                                 value.name = "value")
#4.3 Plot
fig2_panela <- 
  ggplot(data=df_countdta_fig2_pa_melt, aes(x= pub_years, y= value, fill= type)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  #theme_minimal(base_size = 15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size=1.5), 
        text = element_text(size=20))+ 
  labs(y = "Number", x= "Year")+
  theme(legend.position= c(0.2, 0.9))+
  scale_x_continuous(breaks=c(2008:2019))+ 
  ylim(0,30)+
  scale_fill_lancet(name = "", labels= c("Mathematical Models", "Recommendations with Mathematical Models", "Guidelines with Mathematical Models"))+
  ggtitle("Absolute Number of Guidelines, Recommendations and Mathematical Models by Year of Publication")
#theme(plot.title = element_text(face= "bold"))+
fig2_panela

#4.4 Create fig2_panela without 2008 and 2019 
df_countdta_fig2_pav2 <- df_countdta_fig2_pa[-c(1,12),]

#4.5 Melt data 
df_countdta_fig2_pav2_melt <- melt(data = df_countdta_fig2_pav2, 
                                   id.vars= c("pub_years"), 
                                   measure.vars= c("mm_guidelines", "mm_rec", "num_models"), 
                                   variable.name = "type", 
                                   value.name = "value")
#4.6 Plot
fig2_panela_v2 <- 
  ggplot(data=df_countdta_fig2_pav2_melt, aes(x= pub_years, y= value, fill= type)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  #theme_minimal(base_size = 15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size=1.5), 
        text = element_text(size=20))+ 
  labs(y = "Number", x= "Year")+
  theme(legend.position= c(0.2, 0.9))+
  scale_x_continuous(breaks=c(2008:2019))+ 
  ylim(0,30)+
  scale_fill_brewer(palette = "Set2", name = "", labels= c("Guidelines with Mathematical Models", "Recommendations with Mathematical Models", "Mathematical Models"))+
  ggtitle("Absolute Number of Guidelines, Recommendations and Mathematical Models by Publication Year")
#theme(plot.title = element_text(face= "bold"))+
fig2_panela_v2

#5. Create Figure 2, Panel B

#5.1 Create new variables for prevalence 
df_countdta_fig2_pb <- df_countdta %>%
  mutate(prev_mm_grc= (mm_guidelines/num_guidelines)*100)  %>%
  mutate(prev_mm_rec= (mm_rec/num_rec)*100) 

#5.2 Create subset of data 
df_countdta_fig2_pb_select <- df_countdta_fig2_pb %>%
  select(pub_years, prev_mm_grc, prev_mm_rec)

#5.2.1 replace NaN for row 1 columns 2 and 3
df_countdta_fig2_pb_select[1,2] <- 0
df_countdta_fig2_pb_select[1,3] <- 0

#5.3 Melt data 
df_countdta_fig2_pb_melt <- melt(data = df_countdta_fig2_pb_select, 
                                 id.vars= c("pub_years"), 
                                 measure.vars= c("prev_mm_grc", "prev_mm_rec"), 
                                 variable.name = "type", 
                                 value.name = "value")

#5.4 Plot 
fig2_panelb <- 
  ggplot(data=df_countdta_fig2_pb_melt, aes(x= pub_years, y= value, fill= type)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  #theme_minimal(base_size = 15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y = "Proportion (%)", x= "Year")+
  theme(legend.position= "bottom")+
  scale_x_continuous(breaks=c(2008:2019))+ 
  ylim(0,100)+
  scale_fill_brewer(palette = "Accent", name = "", labels= c("Proportion of Guidelines with Mathematical Modelling", "Proportion of Recommendations with Mathematical Modelling" ))
#ggtitle("B")+
#theme(plot.title = element_text(face= "bold"))
fig2_panelb

#5.5 Create new variables for prevalence- including proportion of MM 
df_countdta_fig2_pb_v2 <- df_countdta %>%
  mutate(prev_mm_grc= (mm_guidelines/num_guidelines)*100)  %>%
  mutate(prev_mm_rec= (mm_rec/num_rec)*100) %>%
  mutate(prev_mm = num_models/ sum(num_models) * 100)

#5.6 Create subset of data 
df_countdta_fig2_pb_select_v2 <- df_countdta_fig2_pb_v2 %>%
  select(pub_years, prev_mm_grc, prev_mm_rec, prev_mm)

#5.7.1 Remove 2008 and 2019 from data set 
df_countdta_fig2_pb_select_v2 <- df_countdta_fig2_pb_select_v2[-c(1,12),] 

#5.8 Melt data (without prev_mm)
df_countdta_fig2_pb_melt_v2 <- melt(data = df_countdta_fig2_pb_select_v2, 
                                    id.vars= c("pub_years"), 
                                    measure.vars= c("prev_mm_grc", "prev_mm_rec"), 
                                    variable.name = "type", 
                                    value.name = "value")

library(RColorBrewer)
#5.9 Plot 
fig2_panelb_v2 <- 
  ggplot(data=df_countdta_fig2_pb_melt_v2, aes(x= pub_years, y= value, fill= type)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  #theme_minimal(base_size = 15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1.5), 
        text = element_text(size=20))+
  labs(y = "Proportion (%)", x= "Year")+
  theme(legend.position= c(0.25, 0.9))+
  scale_x_continuous(breaks=c(2008:2019))+ 
  ylim(0,100)+
  scale_fill_brewer(palette = "Set2", name = "", labels= c("Proportion of Guidelines with Mathematical Modelling", "Proportion of Recommendations with Mathematical Modelling"))+
  ggtitle("Proportion of Guidelines, Recommendations and Mathematical Models by Publication Year")
#theme(plot.title = element_text(face= "bold"))
fig2_panelb_v2


#6. Combine plots
fig2_combined <- ggarrange(fig2_panela, fig2_panelb, labels = c("A", "B"),
                           common.legend = FALSE, legend = "bottom", nrow = 2) 

# THIS IS FINAL PlOT FOR FIGURE 2: 
library(ggpubr)
fig2_combined_v2 <- ggarrange(fig2_panela_v2, fig2_panelb_v2, labels = c("A", "B"),
                              common.legend = FALSE, nrow = 2) 
fig2_combined_v2

ggsave(plot = fig2_combined_v2, 
       filename = "/Users/kristinandrejko/Box/01-Research/01-WHO-MM/WHO-Models/plots/fig2.pdf", 
       device = "pdf", 
       width= 16,
       height= 9, 
       units= "in")

##########
#Figure 3#
##########

#Create Figure 3, Panel A 

#1. Create data frame with unique GRC's only 
unique_grc <- distinct(dta, grc, .keep_all = TRUE) 

#2. Create a summary of topics by GRC level 
topic_count_grc <- unique_grc %>%
  group_by(topic) %>%
  dplyr::summarise(n()) 
topic_count_grc

#2.1 Rename "n()" to "freq"
names(topic_count_grc)[2] <- "freq"

#3. Check that sum of freq in topic_count_grc is 47
sum(topic_count_grc$freq)

#4. Rearrange by increasing frequency
topic_count_grc_2 <- topic_count_grc %>%
  arrange(freq)

#5.Create vector for type of disease 
type_disease_gl <- c("Chronic", "Chronic","Chronic","Infectious", "Chronic", "Infectious", 
                     "Infectious", "Chronic", "Chronic", "Infectious", "Infectious", "Infectious", 
                     "Infectious", "Infectious")

#6. Bind type of disease column 
topic_count_grc_2 <- cbind(topic_count_grc_2, type_disease_gl)

topic_count_grc_2$topic <- ifelse(topic_count_grc_2$topic == "TB", "Tuberculosis", topic_count_grc_2$topic)

#7. Force the order of topics 
order_topic_gl <- c("Diabetes", "Disabilities", "Environmental risk management", 
                    "Malaria", "Mental health and substance abuse", "Neglected tropical diseases",
                    "Reproductive health", "Cancer", "Nutrition", "Other infectious diseases and IPC",
                    "Infection Prevention and Control", "Hepatitis B or C", 
                    "Tuberculosis", "HIV")

#8. Create GG plot

fig3_panela <- ggplot(data=topic_count_grc_2, aes(x=topic, y=freq, fill= type_disease_gl))+
  geom_bar(position= "dodge", stat="identity")+
  #ggtitle("A")+
  #theme(plot.title = element_text(face= "bold"))+
  ylab("Number of Guidelines informed by mathematical modeling")+
  xlab("")+
  coord_flip()+
  labs(fill = "Type of Disease")+ 
  scale_x_discrete(limits = order_topic_gl)+ #re-order from low -> high 
  scale_y_continuous(expand = c(0,0), breaks = c(0, 5, 10, 14))+ 
  theme_minimal(base_size = 20) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1.5), 
        legend.position = c(0.9, 0.7)) +  
  #legend.position = "none")+ 
  scale_fill_brewer(palette = "Set2") 
fig3_panela

#Create Figure 3, Panel B 

dta$topic[dta$topic=="TB"] <- "Tuberculosis"

#1. Count Frequency of different health topics and create data frame
topic.count <- dta %>%
  group_by(topic) %>%
  dplyr::summarise(n()) 
topic.count

topic.count.df <- data.frame(topic.count)

#1.1 Rename "n()" to "freq"
names(topic.count.df)[2] <- "freq"

#2. Arrange data frame by in ascending order
topic.count.df.2 <- topic.count.df %>%
  arrange(freq)

#3. Hard code whether disease types are chronic or infectious in correct order
type_disease <- c("Chronic","Chronic","Chronic", "Chronic", "Infectious","Chronic", 
                  "Infectious", "Infectious", "Infectious", "Infectious", "Infectious", 
                  "Chronic", "Infectious", "Infectious")

#3. Merge type_disease with topic_count arranged by order 
topic.count.df.2<- cbind(topic.count.df.2, type_disease)

#count number of recommendations that were infectious 
sum(which(topic.count.df.2$type_disease == "Infectious"))

#4. Hard code the order you want the bars to appear in the graph 
order_topic <- c("Diabetes", "Mental health and substance abuse", 
                 "Disabilities", "Environmental risk management", 
                 "Malaria", "Nutrition", "Reproductive health", 
                 "Infection Prevention and Control", "Neglected tropical diseases", 
                 "Other infectious diseases and IPC", "Hepatitis B or C", "Cancer",
                 "HIV", "Tuberculosis")

#5. Create GG Plot 
fig3_panelb <- ggplot(data=topic.count.df.2, aes(x=topic, y=freq, fill= type_disease))+
  geom_bar(position= "dodge", stat="identity")+
  #ggtitle("B")+
  #theme(plot.title = element_text(face= "bold"))+
  ylab("Number of Recommendations informed by mathematical modeling")+
  xlab("")+
  scale_y_continuous(expand = c(0,0), breaks = c(0, 5, 10, 15, 20, 25, 28))+ 
  labs(fill = "Type of Disease")+ 
  coord_flip()+
  scale_x_discrete(limits = order_topic)+ #re-order from low -> high 
  theme_minimal(base_size = 20) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1.5))+ 
  theme(legend.position = 'none') + 
  scale_fill_brewer(palette = "Set2") 
fig3_panelb

#6. Arrange plots together 
fig3_combined <- ggarrange(fig3_panela, fig3_panelb, labels = c("A", "B"),
                           common.legend = FALSE, nrow = 2) 
fig3_combined

ggsave(plot = fig3_combined, 
       filename = "/Users/kristinandrejko/Box/01-Research/01-WHO-MM/WHO-Models/plots/fig3.pdf", 
       device = "pdf", 
       width= 15,
       height= 10, 
       units= "in")

############
##Appendix##
############

#1. Figure S1 

#1.2 Figure S1. Panel A: Number of Guidelines with MM vs. Total Number of Guidelines 

#1.2.1 Create subset of data with just year, mm, mm_rec, mm_grc
df_countdta_figs1_pa <- df_countdta %>%
  select(pub_years, num_guidelines, mm_guidelines)

#Remove 2008 from graphs 
df_countdta_figs1_pa_no2008 <- df_countdta_figs1_pa[-c(1),]

#1.2.2 Melt data 
df_countdta_figs1_pa_melt <- melt(data = df_countdta_figs1_pa_no2008, 
                                  id.vars= c("pub_years"), 
                                  measure.vars= c("num_guidelines", "mm_guidelines"), 
                                  variable.name = "type", 
                                  value.name = "value")

#1.2.3 
figs1_panela <- 
  ggplot(data=df_countdta_figs1_pa_melt, aes(x= pub_years, y= value, fill= type)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  #theme_minimal(base_size = 15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1.5), 
        text = element_text(size=20))+
  labs(y = "Number", x= "Year")+
  theme(legend.position=c(0.17, 0.9))+
  scale_x_continuous(breaks=c(2009:2019))+ 
  ylim(0,30)+
  scale_fill_brewer(palette = "Set2", name = "", labels= c("Guidelines", "Guidelines with Mathematical Models"))+
  ggtitle("Absolute Number of Guidelines and Guidelines with Models by Publication Year")
#theme(plot.title = element_text(face= "bold"))
figs1_panela

#1.3 Figure S1. Panel B: Number of Recommendations with MM vs. Total Number of Recommendations 

#1.3.1 Create subset of data with just year, mm, mm_rec, mm_grc
df_countdta_figs1_pb <- df_countdta %>%
  select(pub_years, num_rec, mm_rec)

#remove data from 2008 
df_countdta_figs1_pb_no2008 <- df_countdta_figs1_pb[-c(1),]

#1.3.2 Melt data 
df_countdta_figs1_pb_melt <- melt(data = df_countdta_figs1_pb_no2008, 
                                  id.vars= c("pub_years"), 
                                  measure.vars= c("num_rec", "mm_rec"), 
                                  variable.name = "type", 
                                  value.name = "value")

#1.3.3 
figs1_panelb <- 
  ggplot(data=df_countdta_figs1_pb_melt, aes(x= pub_years, y= value, fill= type)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  #theme_minimal(base_size = 15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1.5), 
        text = element_text(size=20))+
  labs(y = "Number", x= "Year")+
  theme(legend.position= c(0.20, 0.9))+
  scale_x_continuous(breaks = c(2009:2019))+ 
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 264))+
  #ylim(0,264)+
  scale_fill_brewer(palette = "Set2",name = "", labels= c("Recommendations", "Recommendations with Mathematical Models" ))+
  ggtitle("Absolute Number of Recommendations and Recommendations with Models by Publication Year")
#theme(plot.title = element_text(face= "bold"))
figs1_panelb

#1.4 Bind the two panels together 
figs1_combined <- ggarrange(figs1_panela, figs1_panelb, labels = c("A", "B"),
                            common.legend = FALSE, nrow = 2) 
figs1_combined

ggsave(plot = figs1_combined, 
       filename = "/Users/kristinandrejko/Box/01-Research/01-WHO-MM/WHO-Models/plots/figS1.pdf", 
       device = "pdf", 
       width= 15,
       height= 11, 
       units= "in")

#2. Figure S2: Strength of Guideline Recs & Quality of Evidence over time 

#2.1 Create Matrix with Strength of Guidelines Recs w/ MM over time 

years <- (2008:2018)
strength_by_year <- matrix(nrow=11, ncol=2)

for (i in 2008:2018){
  dta_strong <- dta %>%
    filter(pub_date == i) %>%
    filter(evidence_strength == "Strong") 
  
  dta_strong <- length(dta_strong$grc)
  
  strength_by_year[i-2007, 2] <- dta_strong
  
  dta_conditional <- dta %>%
    filter(pub_date == i) %>%
    filter(evidence_strength == "Conditional") 
  
  dta_conditional <- length(dta_conditional$grc)
  
  strength_by_year[i-2007, 1] <- dta_conditional
}

df_strength_by_year <- data.frame(strength_by_year)
colnames(df_strength_by_year)<- c( "Conditional", "Strong") 
rownames(df_strength_by_year)<- years

#2.2 Create Matrix with Quality of Guideline Recs w/ MM over time 

years <- (2008:2018)
quality_by_year <- matrix(nrow=11, ncol=4)

for (i in 2008:2018){
  dta_vlow <- dta %>%
    filter(pub_date == i) %>%
    filter(evidence_quality_2 == "Very Low") 
  
  dta_vlow <- length(dta_vlow$grc)
  
  quality_by_year[i-2007, 1] <- dta_vlow
  
  dta_low <- dta %>%
    filter(pub_date == i) %>%
    filter(evidence_quality_2 == "Low") 
  
  dta_low <- length(dta_low$grc)
  
  quality_by_year[i-2007, 2] <- dta_low
  
  dta_mod <- dta %>%
    filter(pub_date == i) %>%
    filter(evidence_quality_2 == "Moderate") 
  
  dta_mod <- length(dta_mod$grc)
  
  quality_by_year[i-2007, 3] <- dta_mod
  
  dta_high <- dta %>%
    filter(pub_date == i) %>%
    filter(evidence_quality_2 == "High") 
  
  dta_high <- length(dta_high$grc)
  
  quality_by_year[i-2007, 4] <- dta_high
}

df_quality_by_year <- data.frame(quality_by_year)
colnames(df_quality_by_year)<- c( "Very Low", "Low", "Moderate", "High") 
rownames(df_quality_by_year)<- years

#2.3 Melt and plot data frames

df_strength_by_year <- df_strength_by_year %>%
  mutate(year = years)

df_quality_by_year <- df_quality_by_year %>%
  mutate(year = years)

df_strength_by_year_no2008 <- df_strength_by_year[-c(1),]  
df_quality_by_year_no2008 <- df_quality_by_year[-c(1),]  

df_strength_by_year_melt <- melt(data = df_strength_by_year_no2008, 
                                 id.vars = "year", 
                                 measure.vars = c("Conditional", "Strong"), 
                                 variable.name = "Strength_Rec", 
                                 value.name = "Num_Rec")


df_quality_by_year_melt <- melt(data = df_quality_by_year_no2008, 
                                id.vars = "year", 
                                measure.vars = c( "Very Low", "Low", "Moderate", "High") , 
                                variable.name = "Quality_Rec", 
                                value.name = "Num_Rec")            

figs2_pa <- 
  ggplot(data=df_strength_by_year_melt, aes(x=year, y=Num_Rec, fill= Strength_Rec)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size =1.5), 
        text = element_text(size=20))+
  #theme_minimal(base_size = 10) +
  labs(y = "Number of Recommendations", x= "Year")+
  theme(legend.position= c(0.15, 0.9))+
  scale_x_continuous(breaks=c(2009:2018))+ 
  ylim(0,20)+
  guides(fill=guide_legend(""))+
  scale_fill_brewer(palette = "Set2", labels=c("Conditional", "Strong")) 
#ggtitle("Panel A: Strength of Guideline Recommendations incorporating evidence from mathematical modeling over time")
figs2_pa

figs2_pb <- 
  ggplot(data=df_quality_by_year_melt, aes(x=year, y=Num_Rec, fill= Quality_Rec)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1.5), 
        text = element_text(size=20))+
  #theme_minimal(base_size = 10) +
  labs(y = "Number of Recommendations", x= "Year")+
  theme(legend.position= c(0.15, 0.9))+
  scale_x_continuous(breaks=c(2009:2018))+ 
  ylim(0,20)+
  guides(fill=guide_legend(""))+
  scale_fill_brewer(palette = "Set2",labels=c("Very Low", "Low", "Moderate", "High"))
#ggtitle("Panel B: Quality of Guideline Recommendations incorporating evidence from mathematical modeling over time")
figs2_pb

#2.4 Bind strength and quality data frames together
df_strength_quality <- merge(df_quality_by_year, df_strength_by_year, by= "year") 

#2.5 Bind the df_strength_quality data frame with a subset of df_countdta

df_countdta_figs2_pc <- df_countdta %>%
  select(pub_years, mm_rec) 

#2.5.1 Rename Columns and merge

names(df_countdta_figs2_pc)[1] <- "year"
df_strength_quality_2 <- merge(df_strength_quality, df_countdta_figs2_pc, by = "year")
names(df_strength_quality_2)[2] <- "Very_Low"

#2.6 Create prev values 

df_strength_quality_2 <- df_strength_quality_2 %>%
  mutate(conditional_prev = (Conditional / mm_rec) * 100) %>%
  mutate(strong_prev = (Strong / mm_rec) * 100) %>%
  mutate(vlow_prev = (Very_Low / mm_rec) * 100) %>%
  mutate(low_prev = (Low / mm_rec) * 100) %>%
  mutate(mod_prev = (Moderate / mm_rec) * 100) %>%
  mutate(high_prev = (High / mm_rec) * 100)

#2.6.1 Deal with NaN values for 2008 
df_strength_quality_2[1, 9:14] <- 0

#2.7 Select only conditional prev, melt, and create panel c

df_strength_quality_figs2_pc <- df_strength_quality_2 %>%
  select(year, conditional_prev)

df_strength_quality_figs2_pc_melt <- melt(data = df_strength_quality_figs2_pc, 
                                          id.vars = "year", 
                                          measure.vars = c( "conditional_prev") , 
                                          variable.name = "type", 
                                          value.name = "value")     

#Remove 2008 
df_strength_quality_figs2_pc_melt_no2008 <- df_strength_quality_figs2_pc_melt[-c(1),]

figs2_pc <- 
  ggplot(data=df_strength_quality_figs2_pc_melt_no2008, aes(x=year, y=value, fill= type)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  #theme_minimal(base_size = 10) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1.5), 
        text= element_text(size = 20))+
  labs(y = "Proportion of Recommendations", x= "Year")+
  theme(legend.position= c(0.15, 0.9))+
  scale_x_continuous(breaks=c(2008:2018))+ 
  ylim(0,100)+
  guides(fill=guide_legend(""))+
  scale_fill_brewer(palette = "Set2", labels=c("Conditional"))  
#ggtitle("Panel C: Prevalence of Strength of Guideline Recommendationsincorporating evidence from mathematical modeling over time
#(out of total guideline recommendations including MM each year)")
figs2_pc

#2.8 Select only verylow, low, mod, high prev, melt, and create panel d

df_strength_quality_figs2_pd <- df_strength_quality_2 %>%
  select(year, vlow_prev, low_prev, mod_prev, high_prev)

#remove 2008 data 
df_strength_quality_figs2_pd_no2008 <- df_strength_quality_figs2_pd[-c(1),]

df_strength_quality_figs2_pd_melt <- melt(data = df_strength_quality_figs2_pd_no2008, 
                                          id.vars = "year", 
                                          measure.vars = c("vlow_prev", "low_prev", "mod_prev", "high_prev") , 
                                          variable.name = "type", 
                                          value.name = "value")         

figs2_pd <- 
  ggplot(data=df_strength_quality_figs2_pd_melt, aes(x=year, y=value, fill= type)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  #theme_minimal(base_size = 10) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1.5), 
        text = element_text(size = 20))+
  labs(y = "Proportion of Recommendations", x= "Year")+
  theme(legend.position=c(0.15, 0.9))+
  scale_x_continuous(breaks=c(2009:2018))+ 
  ylim(0,100)+
  guides(fill=guide_legend(""))+
  scale_fill_brewer(palette = "Set2", labels=c("Very Low", "Low", "Moderate", "High"))  
figs2_pd

#2.9 Pull all the panels together

figs2_combined <- ggarrange(figs2_pa, figs2_pb, figs2_pc, figs2_pd, labels = c("A", "B", "C", "D"),
                            common.legend = FALSE,nrow = 2, ncol=2) 
figs2_combined

#2.10 ggsave
ggsave(plot = figs2_combined, 
       filename = "/Users/kristinandrejko/Box/01-Research/01-WHO-MM/WHO-Models/plots/figS2.pdf", 
       device = "pdf", 
       width= 18,
       height= 12, 
       units= "in")

#3. Figure S3: Proportion of Guidelines by Health Topic

#3.1 Figure S3, Panel A 

totaldta_yonly$topic[totaldta_yonly$topic=="TB"] <- "Tuberculosis"

#3.1.1 Summarize total guidelines included by topic 
totaldta_yonly_topiccount <- totaldta_yonly %>%
  group_by(topic) %>%
  dplyr::summarise(n()) 
totaldta_yonly_topiccount

#3.1.2 Rename column n() to freq for total 
names(totaldta_yonly_topiccount)[2] <- "freq_total"

#3.1.3 rename column n() to freq for MM
names(topic_count_grc)[2] <- "freq_mm"

topic_count_grc$topic <- ifelse(topic_count_grc$topic == "TB", "Tuberculosis", topic_count_grc$topic)

#3.1.4 merge two tibbles by topic 
merge_topic_gl <- merge(totaldta_yonly_topiccount, topic_count_grc, by= "topic")

#3.1.5 Mutate new variable 
merge_topic_gl <- merge_topic_gl %>%
  mutate(perc = (freq_mm / freq_total) * 100) 

#3.1.6 Create ggplot with percentage 
fig_s3_pa <- ggplot(data=merge_topic_gl, aes(x=topic, y=perc))+
  geom_bar(position= "dodge", stat="identity")+
  ylab("Percentage of guidelines informed by mathematical modeling out of total guidelines published per topic")+
  xlab("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size =1.5), 
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "in"), text = element_text(size=20))+
  coord_flip()+
  scale_x_discrete(limits = order_topic_gl)+ #re-order from low -> high 
  ylim(0,100)+
  scale_y_continuous(expand = c(0,0))
#theme_minimal(base_size = 14) 
fig_s3_pa

#3.2 Figure S3, Panel B 

#topic_count_grc is a summary table of of the guidelines with MM
#Check sum(topic_count_grc$freq_mm) = 47 

#3.2.1  Mutate new variable (perc) where freq_mm is divided by sum(topic_count_grc$freq_mm) (n=47)

topic_count_grc <- topic_count_grc %>%
  mutate(perc = (freq_mm / (sum(freq_mm))) * 100) 

#3.2.2 Create ggplot
fig_s3_pb <- ggplot(data=topic_count_grc, aes(x=topic, y=perc))+
  geom_bar(position= "dodge", stat="identity")+
  ylab("Percentage of guidelines informed by mathematical modeling out of total guidelines with mathematical models")+
  xlab("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1.5), 
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "in"), text = element_text(size=20))+
  labs(fill = "Type of Disease")+ 
  coord_flip()+
  scale_x_discrete(limits = order_topic_gl)+ #re-order from low -> high 
  scale_y_continuous(limits= c(0, 100), expand = c(0,0.1))
#theme_minimal(base_size = 14) 
fig_s3_pb

#3.3 Bind the two plots togehter 
figs3_combined <- ggarrange(fig_s3_pa, fig_s3_pb, labels = c("A", "B"), 
                            nrow = 2)
figs3_combined

ggsave(plot = figs3_combined, 
       filename = "/Users/kristinandrejko/Box/01-Research/01-WHO-MM/WHO-Models/plots/figS3.pdf", 
       device = "pdf", 
       width= 19,
       height= 12, 
       units= "in")

#4. Figure S4: Proportion of Recommendations by Health Topic

#4.1 Figure S4, Panel A: Proportion of Recommendations by Health Topic out of total rec per health topic 

#4.1.1 Summarize total recommendations included by topic 
totaldta_yonly_topiccount_2 <- totaldta_yonly %>%
  select(rec_count, topic) %>%
  group_by(topic) %>%
  dplyr::summarise(totalrecs = sum(as.numeric(rec_count))) 

#4.1.2 Rename column n() to freq_total (total number of rec by topic)
names(totaldta_yonly_topiccount_2)[2] <- "freq_total_rec"

#totaldta_yonly_topiccount_2$topic[15]<- "Tuberculosis"

#4.1.3 rename freq to freq_mm for recommendations with MM by otpic 
names(topic.count.df.2)[2] <- "freq_mm_rec"

#4.1.4 merge two tibbles by topic 
merge_topic_rec <- merge(totaldta_yonly_topiccount_2, topic.count.df.2, by= "topic")

#4.1.5 Mutate new variable 
merge_topic_rec <- merge_topic_rec %>%
  mutate(perc = (freq_mm_rec / freq_total_rec) * 100) 

#4.6 Create ggplot with percentage 
fig_s4_pa <- ggplot(data=merge_topic_rec, aes(x=topic, y=perc))+
  geom_bar(position= "dodge", stat="identity")+
  ylab("Percentage of recommendations informed by mathematical modeling out of total recommendations published per health topic")+
  xlab("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1.5), 
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "in"), text = element_text(size=20))+ 
  coord_flip()+
  scale_x_discrete(limits = order_topic_gl)+ #re-order from low -> high 
  scale_y_continuous(limits= c(0, 100), expand = c(0,0.1)) 
fig_s4_pa

#4.2 Figure S4, Panel B: Proportion of Recommendations by Health Topic out of total rec with mm  

#4.2.1 Mutate new variable (perc) where freq_mm is divided by sum(topic_count_grc$freq_mm)

topic.count.df.2 <- topic.count.df.2 %>%
  mutate(perc = (freq_mm_rec / (sum(freq_mm_rec))) * 100) 

#4.2.2 Create ggplot
fig_s4_pb <- ggplot(data=topic.count.df.2, aes(x=topic, y=perc))+
  geom_bar(position= "dodge", stat="identity")+
  ylab("Percentage of recommendations informed by mathematical modeling out of total recommendations with mathematical modeling evidence")+
  xlab("")+
  labs(fill = "Type of Disease")+ 
  coord_flip()+
  scale_x_discrete(limits = order_topic_gl)+ #re-order from low -> high 
  scale_y_continuous(limits= c(0, 100), expand = c(0,0.1))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1.5), 
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "in"), text = element_text(size = 20)) 
fig_s4_pb

#4.3 arrangeplots together 
figs4_combined <- ggarrange(fig_s4_pa, fig_s4_pb, labels = c("A", "B"), nrow = 2)
figs4_combined

ggsave(plot = figs4_combined, 
       filename = "/Users/kristinandrejko/Box/01-Research/01-WHO-MM/WHO-Models/plots/figS4.pdf", 
       device = "pdf", 
       width= 23,
       height= 12, 
       units= "in")

#################################################################
##### Supplemental Analysis- QoE/ SoR for Non-MM Recs #####
#################################################################

#sup_dta_all <- read_excel("~/Dropbox/WHO-MM-Project/03-WHOMM-DataAbstraction/Abstraction_QOE_SOR.xlsx")
#sup_dta_all <- read_excel("/Users/kristinandrejko/Box/01-Research/01-WHO-MM/WHO-Models/data/Abstraction_QOE_SOR.xlsx") #To replicate original Lancet GH analysis, look here
sup_dta_all <- read_excel("/Users/kristinandrejko/Box/01-Research/01-WHO-MM/WHO-Models/data/Abstraction_QOE_SOR_070920.xlsx") #New analysis acocuntinf for 4 rec that Poojan excluded

sup_dta <- sup_dta_all %>%
  filter(model == 0)

#For results paragraph- total recommendations included in GRC approved guiddlines 
sup_dta_1 <- sup_dta_all %>%
  filter(model == 1)

length(unique(sup_dta_1$grc)) #46

#keep only first instance of each grc 
unique_sup_dta_1 <- distinct(sup_dta_1, grc, .keep_all = TRUE) 
length(unique(unique_sup_dta_1$grc)) #46
sum(unique_sup_dta_1$total_rec) #461 total recs from 46 GL
sum(unique_sup_dta_1$total_rec_nomm) #363 nonMM recs from 46 GL

View(unique_sup_dta_1)

#1.1. Data Cleaning (Combine very low to low into very low, and repleace weak w/ conditional)
sup_dta[sup_dta$evidence_quality == "Very low", "evidence_quality"] <- "Very Low" #replace "Very low" with Very Low
sup_dta[sup_dta$evidence_quality == "Very low to low", "evidence_quality"] <- "Very Low" #replace "Very low" with Very Low
sup_dta[sup_dta$evidence_strength == "Weak", "evidence_strength"] <- "Conditional" #replace "Weak" with "Conditional"

#1.2 Re-organize the order of the covariates that appear in the table
sup_dta$evidence_quality_sup <- factor(sup_dta$evidence_quality, 
                                       levels = c("Very Low", "Low", "Moderate",
                                                  "High"))

sup_dta$evidence_strength_sup <- factor(sup_dta$evidence_strength, 
                                        levels = c("Conditional", "Strong")) 

#1.3 Create a label for the X column (quality of evidence)
label(sup_dta$evidence_quality_sup) <- "Quality of evidence (overall)"
label(sup_dta$evidence_strength_sup) <- "Strength of Recommendation"

#1.4 Use Table1 Package to create table using data from uniquerec
quality_v_strength_sup <- table1(~evidence_quality_sup | evidence_strength_sup, data=sup_dta)
quality_v_strength_sup

#1.5 Prop Test: UPDATED!

#Comparing prop of strong in model informed (47) vs. non-model informed (232)
proptest1 <- prop.test(x = c(232, 47), n = c(373, 101))
proptest1 # p-value = 0.006453

#Comparing prop of conditional in model informed (54) vs. non-model informed (141)
proptest2 <- prop.test(x = c(141, 54), n = c(373, 101))
proptest2 # p-value = 0.006453

#Comparing prop of VERY LOW quality in model informed vs. non-model informed
#(less likely to have very low quality of evidence in non-MM)
proptest3 <- prop.test(x = c(90, 42), n = c(373, 101))
proptest3 # p-value = 0.0008183

#Comparing prop of  LOW quality in model informed vs. non-model informed
proptest3.5 <- prop.test(x = c(135, 36), n = c(373, 101))
proptest3.5 #p-value = 1

#Comparing prop of MODERATE in model informed vs. non-model informed
proptest4 <- prop.test(x = c(117, 16), n = c(378, 101))
proptest4 # p-value = 0.003886

#Comparing prop of moderate to high in model informed vs. non-model informed 
proptest5 <- prop.test(x = c(148, 23), n = c(373, 101))
proptest5 # p-value = 0.002513

#Comparing prop of HIGH in model informed vs. non-model informed
proptest6 <- prop.test(x = c(31, 7), n = c(373, 101))
proptest6 # p-value = 0.8052

#Chi squared = strong rec vs. quality of evidence 
# Among recommendations that were issued with a “strong” designation, differences in the distribution 
# of evidence ratings were not evident between recommendations informed by modeling studies or
# not informed by modeling studies 

library(tribble)
two_way <- tribble(~strong_nomm, ~strongmm, 36, 13, 74, 15, 92, 14, 30, 5) 
chisq.test(two_way) 

#fisher = conditional 

two_way_cond <- tribble(~cond_nomm, ~condmm, 54, 29, 61, 21, 25, 2, 1, 2) 
fisher.test(two_way_cond) 
chisq.test(two_way_cond) #not valid becuase expected counts <5 

#Chi squared = all recs 
# Overall, the distribution of quality of evidence ratings differed for recommendations from the same guidelines that included, 
# or did not include, mathematical modeling studies 
# two_way_allrec <- tribble(~nomm, ~mm, 89, 44, 132, 42, 117, 17, 31, 7) 
two_way_allrec <- tribble(~nomm, ~mm, 90, 42, 135, 36, 117, 16, 31, 7) 
chisq.test(two_way_allrec)


#NEW: is there a difference between  prop of CONDITIONAL recommendations between ID and Non-ID among MODEL informed recs?
two_way_cond_id <- tribble(~nomm, ~mm, 9, 21, 19, 5, 2, 0, 1, 1) 
fisher.test(two_way_cond_id) # 0.0003437 

#NEW: is there a difference between  prop of STRONG recommendations between ID and Non-ID among MODEL informed recs?
two_way_strong_id <- tribble(~nomm, ~mm, 1, 12, 11, 4, 12, 3, 4, 1) 
fisher.test(two_way_strong_id) #0.0001545

#conclusion: distribution fo quality of evidence differs between 

#################################################################
##### Discussion Analysis #####
#################################################################

#1. Find prevalence of guideline RECOMMENDATIONS from 2016 - 2018 

#1.1 Total number of recommendations published between 2016 - 2018 
inc_year <- c(2016, 2017, 2018)

totaldta_2016_18 <- totaldta %>%
  filter(pub_year %in% inc_year)
nrow(totaldta_2016_18)

totaldta_2016_18_yonly <- totaldta_2016_18 %>%
  filter(include_guideline == "y")
nrow(totaldta_2016_18_yonly) #total number of guidelines from 2016 - 2018 (59)

totaldta_2016_18_yonly$rec_count <- as.numeric(totaldta_2016_18_yonly$rec_count) 

sum(totaldta_2016_18_yonly$rec_count) 

#1.2 Total number of recommendations w/ models published between 2016 - 2018 
dta_2016_18 <- dta %>%
  filter(pub_date %in% inc_year)
nrow(dta_2016_18)

#1.3 Calculate prevalence
prev_2016_18 <- (nrow(dta_2016_18)) / sum(totaldta_2016_18_yonly$rec_count) * 100

#2. Find prevalence of GUIDELINES from 2016 - 2018 

unique_grc <- distinct(dta, grc, .keep_all = TRUE) 

#2.1 Total number of guidelines published between 2016 -2018
inc_year <- c(2016, 2017, 2018)

totaldta_2016_18 <- totaldta %>%
  filter(pub_year %in% inc_year)
nrow(totaldta_2016_18)

totaldta_2016_18_yonly <- totaldta_2016_18 %>%
  filter(include_guideline == "y")
nrow(totaldta_2016_18_yonly) #total number of guidelines from 2016 - 2018 (59)

#2.2 Total number of guidelines with MM published between 2016 -2018
unique_dta_2016_18 <- unique_grc %>%
  filter(pub_date %in% inc_year)
nrow(unique_dta_2016_18)

#2.3 Calculate prevalence
prev_2016_18 <- nrow(unique_dta_2016_18) / nrow(totaldta_2016_18_yonly) * 100

### SUPPLEMENTAL ANALYSIS LOOKING AT WHETHER ADDITIONAL EVIDENCE WAS AVLAIABLE ### 
length(which(dta$other_evidence == "N")) #2 GL corresponding to 4 recs did NOT have other evidnece (beyond modeling evidence avliable)
View(dta)
#These were guidelines that evaluated the model in an GRADE Evidence Profile 

review6 <- dta %>% 
  filter(other_evidence == "N")
View(review6)

# On 7/10/20 KA confirmed that there were no additional sources informing these 4 recommendaitons
#Note that GRADE table for GRC-09-06-0158 is found on page 6 of pdf of appendix
# and GRADE table for GRC-13-06-0406 is found on page 41 

vec <- c(6,1, 9, 5, 8, 4, 3, 2)

round(((vec/ 38) * 100),  1)

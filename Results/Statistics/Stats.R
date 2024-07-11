rm(list = ls())
#### LOAD LIBRARIES -----
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
#### LOAD & WRANGLE DATA TO IDENTIFY STUDIES FOR METHODOLOGICAL ASSESSMENT ----
Included_PC <- read_delim("Included_PC.txt", 
                          delim = "\t", escape_double = FALSE, 
                          col_names = FALSE, trim_ws = TRUE)
Included_Search <- read_delim("Included_Search_2.txt", 
                            delim = "\t", escape_double = FALSE, 
                            col_names = FALSE, trim_ws = TRUE)

Inclusion_PC<-Included_PC%>%
  select_if(function(x) !(all(is.na(x)) | all(x=="")))%>%
  select(X1, X3, X4, X29, X30, X31, X32, X33, X34)%>%
  filter(!is.na(X29))%>%
  rename(Journal_type           = X1, 
         Year                   = X3,
         Title                  = X4,
         Study_design           = X29, 
         Level_of_analysis      = X30, 
         Primary_outcome        = X31, 
         Vaccination_definition = X32, 
         Infected_defintion     = X33, 
         Mortality_definition   = X34)%>%
  tidyr::separate(Vaccination_definition,c("Vaccination", "Explanation"))%>%
  select(-Explanation)%>%
  tidyr::separate(Infected_defintion,c("Infection", "Explanation"))%>%
  select(-Explanation)%>%
  tidyr::separate(Mortality_definition,c("Mortality", "Explanation"))%>%
  select(-Explanation)%>%
  mutate(Include = if_else(Vaccination=="Clear" & Infection=="Clear" & Mortality=="Clear", "Include", "Exclude"))

Inclusion_search<-Included_Search%>%
  select_if(function(x) !(all(is.na(x)) | all(x=="")))%>%
  select(X1, X3, X4, X29, X30, X31, X32, X33, X34)%>%
  filter(!is.na(X29))%>%
  rename(Journal_type           = X1, 
         Year                   = X3,
         Title                  = X4,
         Study_design           = X29, 
         Level_of_analysis      = X30, 
         Primary_outcome        = X31, 
         Vaccination_definition = X32, 
         Infected_defintion     = X33, 
         Mortality_definition   = X34)%>%
  tidyr::separate(Vaccination_definition,c("Vaccination", "Explanation"))%>%
  select(-Explanation)%>%
  tidyr::separate(Infected_defintion,c("Infection", "Explanation"))%>%
  select(-Explanation)%>%
  tidyr::separate(Mortality_definition,c("Mortality", "Explanation"))%>%
  select(-Explanation)%>%
  mutate(Include = if_else(Vaccination=="Clear" & Infection=="Clear" & Mortality=="Clear", "Include", "Exclude"))

Inclusion_df<-rbind(Inclusion_PC,Inclusion_search)

Inclusion_df%>%
  select(Primary_outcome)%>%
  table()

Inclusion_df%>%
  select(Primary_outcome)%>%
  filter(Primary_outcome=="Mortality"|Primary_outcome=="mortality")%>%
  table()

Inclusion_df%>%
  select(Primary_outcome, Study_design)%>%
  filter(Primary_outcome=="AE")%>%
  select(Study_design)%>%
  table()

Inclusion_df%>%
  select(Primary_outcome, Study_design)%>%
  filter(Primary_outcome=="Mortality"|Primary_outcome=="mortality")%>%
  select(Study_design)%>%
  table()

Inclusion_df%>%
  select(Primary_outcome, Study_design)%>%
  filter(Primary_outcome!="Mortality"&Primary_outcome!="mortality"&Primary_outcome!="AE")%>%
  select(Study_design)%>%
  table()

Inclusion_df%>%
  filter(Primary_outcome=="AE")%>%
  select(Vaccination, Infection, Mortality, Include)%>%
  summarise(vax_sum=sum(Vaccination=="Clear", na.rm=TRUE), 
            infec_sum=sum(Infection=="Clear", na.rm=TRUE), 
            mort_sum=sum(Mortality=="Clear", na.rm=TRUE), 
            include_sum=sum(Include=="Clear", na.rm=TRUE))

Inclusion_df%>%
  filter(Primary_outcome=="Mortality"|Primary_outcome=="mortality")%>%
  filter(Study_design!="Unclear")%>%
  select(Vaccination, Infection, Mortality, Include)%>%
  summarise(vax_sum=sum(Vaccination=="Clear", na.rm=TRUE), 
            infec_sum=sum(Infection=="Clear", na.rm=TRUE), 
            mort_sum=sum(Mortality=="Clear", na.rm=TRUE), 
            include_sum=sum(Include=="Clear", na.rm=TRUE))

Inclusion_df%>%
  filter(Primary_outcome!="Mortality"&Primary_outcome!="mortality"&Primary_outcome!="AE")%>%
  select(Vaccination, Infection, Mortality, Include)%>%
  summarise(vax_sum=sum(Vaccination=="Clear", na.rm=TRUE), 
            infec_sum=sum(Infection=="Clear", na.rm=TRUE), 
            mort_sum=sum(Mortality=="Clear", na.rm=TRUE), 
            include_sum=sum(Include=="Clear", na.rm=TRUE))

Inclusion_df%>%
  filter(Primary_outcome=="Mortality"|Primary_outcome=="mortality")%>%
  filter(Study_design!="Unclear")%>%
  select(Vaccination, Infection, Mortality, Include)%>%
  select(Vaccination)%>%
  table()
Inclusion_df%>%
  filter(Primary_outcome=="Mortality"|Primary_outcome=="mortality")%>%
  filter(Study_design!="Unclear")%>%
  select(Vaccination, Infection, Mortality, Include)%>%
  select(Infection)%>%
  table()
Inclusion_df%>%
  filter(Primary_outcome=="Mortality"|Primary_outcome=="mortality")%>%
  filter(Study_design!="Unclear")%>%
  select(Vaccination, Infection, Mortality, Include)%>%
  select(Mortality)%>%
  table()

Inclusion_df%>%
  filter(Primary_outcome=="Mortality"|Primary_outcome=="mortality")%>%
  #filter(Study_design!="Unclear")%>%
  select(Vaccination, Infection, Mortality, Include)%>%
  select(Include)%>%
  table()
Inclusion_df%>%
  filter(Primary_outcome=="AE")%>%
  #filter(Study_design!="Unclear")%>%
  select(Vaccination, Infection, Mortality, Include)%>%
  select(Include)%>%
  table()
Inclusion_df%>%
  filter(Primary_outcome!="Mortality"&Primary_outcome!="mortality"&Primary_outcome!="AE")%>%
  #filter(Study_design!="Unclear")%>%
  select(Vaccination, Infection, Mortality, Include)%>%
  select(Include)%>%
  table()

Inclusion_df%>%
  #filter(Study_design!="Unclear")%>%
  filter(Include=="Include")%>%
  select(Title, Year)%>%
  arrange(Title)%>%
  print(n=83)

Inclusion_df%>%
  #filter(Study_design!="Unclear")%>%
  filter(Include=="Include")%>%
  filter(Primary_outcome=="Mortality"|Primary_outcome=="mortality")%>%
  select(Title, Year)%>%
  arrange(Title)%>%
  print(n=70)

Inclusion_df%>%
  #filter(Study_design!="Unclear")%>%
  filter(Include=="Include")%>%
  filter(Primary_outcome=="AE")%>%
  select(Title, Year)%>%
  arrange(Title)%>%
  print(n=70)

Inclusion_df%>%
  #filter(Study_design!="Unclear")%>%
  filter(Include=="Include")%>%
  filter(Primary_outcome!="Mortality"&Primary_outcome!="mortality"&Primary_outcome!="AE")%>%
  select(Title, Year)%>%
  arrange(Title)%>%
  distinct()%>%
  print(n=70)
  



#### LOAD & WRANGLE DATA TO ASSESS STUDIES ----
df<- read_excel("~/MSJ/Projects/2024/Stichting Menselijke Maat/Oversterfte _Vaccinatie/Methods/GRADE/Methodological Assessment.xlsx", 
                col_types = c("text", "text", "numeric", 
                              "date", "date", "text", "text", "text", 
                              "text", "text", "text", "text", "text", 
                              "text", "text", "text", "text", "text", 
                              "text", "text", "text", "text", "text", 
                              "text"))
colnames(df)
str(df)

df%>%
  filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  select(Study_design, Primary_outcome, Year)%>%
  group_by(Study_design, Primary_outcome, Year)%>%
  summarise( n = n())%>%
  ggplot()+
  geom_tile(aes(x=as.factor(Year), 
                y=as.factor(Study_design), 
                fill=factor(n)))+
  facet_grid(~Primary_outcome)+
  theme_bw()+
  labs(x="Year", 
       y="Study Design", 
       fill="N", 
       title="Number of studies per primary outcome and study design", 
       subtitle="Columns = Primary Outcome; Rows = Study Design",
       caption="Source: 83 elgible studies")+
  theme(legend.position="bottom")


df%>%
  filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  select(Country, Primary_outcome, Year)%>%
  group_by(Country, Primary_outcome, Year)%>%
  summarise( n = n())%>%
  ggplot()+
  geom_tile(aes(x=as.factor(Year), 
                y=as.factor(Country), 
                fill=factor(n)))+
  facet_grid(~Primary_outcome)+
  theme_bw()+
  labs(x="Year", 
       y="Country", 
       fill="N", 
       title="Number of studies per primary outcome and country", 
       subtitle="Columns = Primary Outcome; Rows = Country of study",
       caption="Source: 83 elgible studies")+
  theme(legend.position="bottom")

df%>%
  filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  select(Patients, Primary_outcome, Year)%>%
  group_by(Patients, Primary_outcome, Year)%>%
  summarise( n = n())%>%
  ggplot()+
  geom_tile(aes(x=as.factor(Year), 
                y=as.factor(Patients), 
                fill=factor(n)))+
  facet_grid(~Primary_outcome)+
  theme_bw()+
  labs(x="Year", 
       y="Type of Patient", 
       fill="N",
       title="Number of studies per primary outcome and type of patients", 
       subtitle="Columns = Primary Outcome; Rows = Type of Patient",
       caption="Source: 83 elgible studies")+
  theme(legend.position="bottom")

df%>%
  mutate(Start = as.POSIXct(Start, format="%Y-%m-%d"), 
         End = as.POSIXct(End, format="%Y-%m-%d"))%>%
  filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Primary_outcome, Start, End, Study, Study_design)%>%
  tidyr::drop_na()%>%
  ggplot()+
  geom_segment(aes(x = Start, xend = End, y = Study, yend=Study, col=Study_design), linewidth=2)+
  facet_grid(~Primary_outcome)+
  theme_bw()+
  labs(x="Date", 
       y="Study", 
       fill="N",
       col="Study design",
       title="Start and end date for each study by outcome",
       subtitle="by Study Design",
       caption="Source: 83 elgible studies")+
  theme(legend.position="bottom")

df%>%
  mutate(Start = as.POSIXct(Start, format="%Y-%m-%d"), 
         End = as.POSIXct(End, format="%Y-%m-%d"))%>%
  filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Primary_outcome, Start, End, Study, Patients)%>%
  tidyr::drop_na()%>%
  ggplot()+
  geom_segment(aes(x = Start, xend = End, y = Study, yend=Study, col=Patients), size=2)+
  facet_grid(~Primary_outcome)+
  theme_bw()+
  labs(x="Date", 
       y="Study", 
       fill="N",
       col="Patients",
       title="Start and end date for each study by outcome", 
       subtitle="by Type of Patient",
       caption="Source: 83 elgible studies")+
  theme(legend.position="bottom")

df%>%
  mutate(Start = as.POSIXct(Start, format="%Y-%m-%d"), 
         End = as.POSIXct(End, format="%Y-%m-%d"))%>%
  filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Primary_outcome, Start, End, Study, Patients, Study_design)%>%
  tidyr::drop_na()%>%
  ggplot()+
  geom_segment(aes(x = Start, xend = End, y = Study, yend=Study, col=Patients), size=2)+
  facet_grid(Primary_outcome~Study_design, scales = "free",space="free")+
  theme_bw()+
  labs(x="Date", 
       y="Study", 
       fill="N",
       col="Type of Patient",
       title="Start and end date for each study", 
       subtitle="Columns = Study Design; Rows = Primary Outcome",
       caption="Source: 83 elgible studies")+
  theme(legend.position="bottom")



df%>%
  mutate(Start = as.POSIXct(Start, format="%Y-%m-%d"), 
         End = as.POSIXct(End, format="%Y-%m-%d"))%>%
  filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Primary_outcome, Start, End, Study, Full_vaccination_lag, Study_design)%>%
  tidyr::drop_na()%>%
  ggplot()+
  geom_segment(aes(x = Start, xend = End, y = Study, yend=Study, col=Full_vaccination_lag), size=2)+
  facet_grid(Primary_outcome~Study_design, scales = "free",space="free")+
  theme_bw()+
  labs(x="Date", 
       y="Study", 
       fill="N",
       col="Full vaccination lag",
       title="Start and end date for each study by outcome")+
  theme(legend.position="bottom")


df%>%
  filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  select(Patients, Primary_outcome, Year, Full_vaccination_lag, Study_design)%>%
  group_by(Study_design, Full_vaccination_lag)%>%
  summarise( n = n())%>%
  ggplot()+
  geom_tile(aes(y=as.factor(Full_vaccination_lag), 
                x=as.factor(Study_design), 
                fill=factor(n)))+
  theme_bw()+
  labs(y="Vaccination lag (days)", 
       x="Study Design", 
       fill="N",
       title="Number of studies per vaccination lag and study design", 
       caption="Source: 83 elgible studies")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

df%>%
  mutate(Start = as.POSIXct(Start, format="%Y-%m-%d"), 
         End = as.POSIXct(End, format="%Y-%m-%d"))%>%
  filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Primary_outcome, Start, End, Study, Full_vaccination_lag, Study_design)%>%
  tidyr::drop_na()%>%
  ggplot()+
  geom_segment(aes(x = Start, xend = End, y = Study, yend=Study, col=Full_vaccination_lag), size=2)+
  facet_grid(Primary_outcome~Study_design, scales = "free",space="free")+
  theme_bw()+
  labs(x="Date", 
       y="Study", 
       fill="N",
       col="Vaccination lag (days)",
       title="Start and end date for each study by outcome, design, and full vaccination lag",
       subtitle="Columns = Study design; Rows = Primary Outcome",
       caption="Source: 83 elgible studies")


df%>%
  mutate(Start = as.POSIXct(Start, format="%Y-%m-%d"), 
         End = as.POSIXct(End, format="%Y-%m-%d"))%>%
  filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Primary_outcome, Start, End, Study, 
         Full_vaccination_lag, Study_design, 
         Diagnosis_as_inclusion_criterium,Vaccination_as_inclusion_criterion)%>%
  tidyr::drop_na()%>%
  ggplot()+
  geom_segment(aes(x = Start, xend = End, y = Study, yend=Study, col=Full_vaccination_lag), size=2)+
  facet_grid(Diagnosis_as_inclusion_criterium~Vaccination_as_inclusion_criterion, scales = "free")+
  theme_bw()+
  labs(x="Date", 
       y="Study", 
       fill="N",
       col="Vaccination lag (days)",
       title="Start and end date for each study by inclusion criteria",
       subtitle="Columns = Inclusion criteria vaccines; Rows = Inclusion criteria Covid-19",
       caption="Source: 83 elgible studies")+
  theme(legend.position="bottom")

df%>%
  mutate(Start = as.POSIXct(Start, format="%Y-%m-%d"), 
         End = as.POSIXct(End, format="%Y-%m-%d"))%>%
  filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Primary_outcome, Start, End, Study, 
         Full_vaccination_lag, Study_design, 
         Diagnosis_as_inclusion_criterium,Vaccination_as_inclusion_criterion)%>%
  tidyr::drop_na()%>%
  ggplot()+
  geom_segment(aes(x = Start, xend = End, y = Study, yend=Study, col=Study_design), size=2)+
  facet_grid(Diagnosis_as_inclusion_criterium~Vaccination_as_inclusion_criterion, scales = "free")+
  theme_bw()+
  labs(x="Date", 
       y="Study", 
       fill="N",
       col="Study design",
       title="Start and end date for each study by inclusion criteria and study design",
       subtitle="Columns = Inclusion criteria vaccines; Rows = Study Design",
       caption="Source: 83 elgible studies")+
  theme(legend.position="bottom")


table(df$Mortality_type)



df%>%
  select(Study_design, Mortality_type, Full_vaccination_lag)%>%
  group_by(Study_design, Mortality_type, Full_vaccination_lag)%>%
  summarise( n = n())%>%
  ggplot()+
  geom_tile(aes(x=as.factor(Full_vaccination_lag), 
                y=as.factor(Mortality_type), 
                fill=factor(n)))+
  facet_grid(~Study_design, scales = "free",space="free")+
  theme_bw()+
  labs(x="Full vaccination lag", 
       y="Type of mortality", 
       fill="N",
       title="Number of studies per type of mortality and full vaccination lag", 
       subtitle="Columns = Study design; Rows = Type of mortality",
       caption="Source: 83 elgible studies")+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
df%>%
  mutate(Start = as.POSIXct(Start, format="%Y-%m-%d"), 
         End = as.POSIXct(End, format="%Y-%m-%d"))%>%
  #filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  filter(Study_design=="Retrospective observational")%>%
  filter(Mortality_type=="All-cause mortality")%>%
  filter(Full_vaccination_lag=="14")%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Start, End, Study, 
         Diagnosis_as_inclusion_criterium,
         Vaccination_as_inclusion_criterion, Patients,
         Country, Confounding_method)%>%
  tidyr::drop_na()%>%
  ggplot()+
  geom_segment(aes(x = Start, xend = End, y = Country, yend=Country, col=Confounding_method), size=2)+
  facet_grid(~Patients, scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       y="Country", 
       fill="N",
       col="Covariate adjustment method",
       title="Start and end date for each study by country, type of patient and adjustment method",
       subtitle="Columns = Type of patient; Rows = Country",
       caption="Source: 11 elgible studies")+
  theme(legend.position="bottom")


df%>%
  mutate(Start = as.POSIXct(Start, format="%Y-%m-%d"), 
         End = as.POSIXct(End, format="%Y-%m-%d"))%>%
  #filter(Primary_outcome=="Mortality" | Primary_outcome=="Unclear")%>%
  filter(Study_design=="Retrospective observational")%>%
  filter(Mortality_type=="All-cause mortality")%>%
  filter(Full_vaccination_lag=="14")%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Start, End, Study, 
         Diagnosis_as_inclusion_criterium,
         Vaccination_as_inclusion_criterion, Confounding_method,
         Statistical_model, Patients)%>%
  tidyr::drop_na()%>%
  ggplot()+
  geom_segment(aes(x = Start, xend = End, y = Patients, yend=Patients, col=Statistical_model), size=2)+
  facet_grid(~Confounding_method, scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       y="Type of patient", 
       fill="Statistical method",
       title="Start and end date for each study by type of patient, covariate adjustment and statistical method",
       subtitle="Columns = Covariate adjustment method; Rows = Type of patient",
       caption="Source: 11 elgible studies")+
  theme(legend.position="bottom")

df%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Title, Study, Study_design, 
         VE_mortality_mean, VE_mortality_low, 
         VE_mortality_high)%>%
  mutate(VE_mortality_mean = as.numeric(gsub(",","",VE_mortality_mean)), 
         VE_mortality_low = as.numeric(gsub(",","",VE_mortality_low)), 
         VE_mortality_high = as.numeric(gsub(",","",VE_mortality_high)))%>%
  tidyr::drop_na()%>%
  ggplot(aes(x=VE_mortality_mean, y=factor(Study), col=factor(Study_design)))+
  geom_point() +
  geom_errorbarh(aes(xmax = VE_mortality_high, xmin = VE_mortality_low))+
  geom_vline(xintercept = 0, lty=2, col="black")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Vaccine effectiveness (VE)", 
       y="Study", 
       col="Study Design",
       title="VE estimates across studies that reported VE",
       subtitle="This plot is just for exploratory purposes - there is too much heterogeneity to infer any summary statistic from it",
       caption="If multiple VE estimates were reported, we chose the longest follow-up time, 
       all-cause mortality, and patients 18 years and older, if possible")

df%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Title, Study, Study_design, Mortality_type, Primary_outcome, 
         VE_mortality_mean, VE_mortality_low, VE_mortality_high, Full_vaccination_lag)%>%
  mutate(VE_mortality_mean = as.numeric(gsub(",","",VE_mortality_mean)), 
         VE_mortality_low = as.numeric(gsub(",","",VE_mortality_low)), 
         VE_mortality_high = as.numeric(gsub(",","",VE_mortality_high)))%>%
  tidyr::drop_na()%>%
  ggplot(aes(x=VE_mortality_mean, y=factor(Study), col=factor(Mortality_type)))+
  geom_point() +
  geom_errorbarh(aes(xmax = VE_mortality_high, xmin = VE_mortality_low))+
  geom_vline(xintercept = 0, lty=2, col="black")+
  facet_grid(~Mortality_type, space="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Vaccine effectiveness (VE)", 
       y="Study", 
       col="Study Design",
       title="VE estimates across studies that reported VE by mortality type",
       subtitle="This plot is just for exploratory purposes - there is too much heterogeneity to infer any summary statistic from it",
       caption="If multiple VE estimates were reported, we chose the longest follow-up time, 
       all-cause mortality, and patients 18 years and older, if possible")

df%>%
  arrange(Title)%>%
  mutate(Study = as.factor(row_number()))%>%
  select(Title, Study, Study_design, Mortality_type, Primary_outcome, 
         VE_mortality_mean, VE_mortality_low, VE_mortality_high, Full_vaccination_lag)%>%
  mutate(VE_mortality_mean = as.numeric(gsub(",","",VE_mortality_mean)), 
         VE_mortality_low = as.numeric(gsub(",","",VE_mortality_low)), 
         VE_mortality_high = as.numeric(gsub(",","",VE_mortality_high)))%>%
  tidyr::drop_na()%>%
  filter(Mortality_type   =="All-cause mortality" | 
           Mortality_type =="All-cause mortality. Covid-19 mortality" | 
           Mortality_type =="Covid-19 mortality")%>%
  ggplot(aes(x=VE_mortality_mean, y=factor(Study), col=factor(Study_design)))+
  geom_point() +
  geom_errorbarh(aes(xmax = VE_mortality_high, xmin = VE_mortality_low))+
  geom_vline(xintercept = 0, lty=2, col="black")+
  facet_grid(~Mortality_type, space="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Vaccine effectiveness (VE)", 
       y="Study", 
       col="Study Design",
       title="VE estimates across studies that reported VE by mortality type and study design",
       subtitle="This plot is just for exploratory purposes - there is too much heterogeneity to infer any summary statistic from it",
       caption="If multiple VE estimates were reported, we chose the longest follow-up time, 
       all-cause mortality, and patients 18 years and older, if possible")




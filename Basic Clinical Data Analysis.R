##Load data
library(tidyverse)


breast_data <- read.csv("DataBreast/donor.tsv", sep = "\t", header = T)
breast_data$disease_status_last_followup[breast_data$donor_vital_status == "deceased"] <- "death"
breast_data$disease_status_last_followup[breast_data$disease_status_last_followup == ""] <- "unknown"


breast_data <- breast_data %>% 
  select(icgc_donor_id, donor_vital_status, donor_age_at_diagnosis, donor_interval_of_last_followup, disease_status_last_followup) %>% 
  filter(!is.na(donor_age_at_diagnosis)) %>% 
  mutate_if(is.character, as.factor) 


summary(breast_data)

summarytools::descr(breast_data)

ovary_data <- read.csv("DataOvary//donor.tsv", sep = "\t", header = T)
ovary_data$disease_status_last_followup[ovary_data$donor_vital_status == "deceased"] <- "death"
ovary_data$disease_status_last_followup[ovary_data$disease_status_last_followup == ""] <- "unknown"


ovary_data <- ovary_data %>% 
  select(icgc_donor_id, donor_vital_status, donor_age_at_diagnosis, donor_interval_of_last_followup, disease_status_last_followup) %>%
  filter(!donor_vital_status == "") %>% 
  mutate_if(is.character, as.factor)

summary(ovary_data)

summarytools::descr(ovary_data)


##Breast cancer

ggplot(data = breast_data, aes(x = donor_age_at_diagnosis, fill = donor_vital_status)) +
  geom_histogram(binwidth = 10,
                 color="black",
                 
                 linetype="dashed") +
  xlab("Age")+
  ylab("Patient") +
  ggtitle("Age at diagnosis - breast cancer") +
  theme_light()

ggsave(filename = "Figures/breast_cancer_his_age_at_diagnosis.png", width = 15, height = 10, scale = 0.5, dpi = 600)


ggplot(data = breast_data, aes(y = donor_age_at_diagnosis, fill = donor_vital_status)) +
  geom_boxplot()+
  # xlab("Age")+
  ylab("Age") +
  ggtitle("Age at diagnosis - breast cancer") +
  theme_light()

ggsave(filename = "Figures/breast_cancer_bocplot_age_at_diagnosis.png", width = 15, height = 10, scale = 0.5, dpi = 600)


##Ovary cancer



ggplot(data = ovary_data, aes(x = donor_age_at_diagnosis, fill = donor_vital_status)) +
  geom_histogram(binwidth = 10,
                 color="black",
                 
                 linetype="dashed") +
  xlab("Age")+
  ylab("Patient") +
  ggtitle("Age at diagnosis - ovary cancer") +
  theme_light()

ggsave(filename = "Figures/ovary_cancer_his_age_at_diagnosis.png", width = 15, height = 10, scale = 0.5, dpi = 600)


ggplot(data = ovary_data, aes(y = donor_age_at_diagnosis, fill = donor_vital_status)) +
  geom_boxplot()+
  # xlab("Age")+
  ylab("Age") +
  ggtitle("Age at diagnosis - ovary cancer") +
  theme_light()

ggsave(filename = "Figures/ovary_cancer_boxplot_age_at_diagnosis.png", width = 15, height = 10, scale = 0.5, dpi = 600)


#Compare

ggplot() +
  geom_boxplot(data = breast_data, aes(x = "Breast cancer", y = donor_age_at_diagnosis, fill = donor_vital_status))+
  geom_boxplot(data = ovary_data, aes(x = "Ovary cancer", y = donor_age_at_diagnosis, fill = donor_vital_status))+
  xlab("")+
  ylab("Age") +
  ggtitle("Age at diagnosis - breast cancer") +
  theme_light()

ggsave(filename = "Figures/compare_breast_ovary_cancer_boxplot_age_at_diagnosis.png", width = 15, height = 10, scale = 0.5, dpi = 600)


ggplot() +
  geom_histogram(data = breast_data, aes(x = donor_age_at_diagnosis, fill = "Breast cancer"))+
  geom_histogram(data = ovary_data, aes(x = donor_age_at_diagnosis, fill = "Ovary cancer"))+
  xlab("Age")+
  ylab("Patient") +
  ggtitle("Age at diagnosis - compare ovary and breast cancer") +
  theme_light()

ggsave(filename = "Figures/compare_breast_ovary_cancer_his_age_at_diagnosis.png", width = 15, height = 10, scale = 0.5, dpi = 600)


ggplot() +
  geom_boxplot(data = breast_data, aes(x = "Breast cancer", y = donor_interval_of_last_followup, fill = "Breast cancer"))+
  geom_boxplot(data = ovary_data, aes(x = "Ovary cancer", y = donor_interval_of_last_followup, fill = "Ovary cancer"))+  ylab("Break duration") +
  ggtitle("Disease followup - compare ovary and breast cancer") +
  facet_grid() +
  xlab("")+
  theme_light() 

ggsave(filename = "Figures/compare_breast_ovary_cancer_boxplot_followup.png", width = 15, height = 10, scale = 0.5, dpi = 600)


summary(breast_data)

status_breast <- as.data.frame(breast_data$disease_status_last_followup)
status_breast$type <- "Breast cancer"

status_breast <- status_breast %>% 
  rename(Followup = `breast_data$disease_status_last_followup`)

status_ovary <- as.data.frame(ovary_data$disease_status_last_followup)
status_ovary$type <- "Ovary cancer"

status_ovary <- status_ovary %>% 
  rename(Followup = `ovary_data$disease_status_last_followup`)

#Breast cancer - status %

all <- status_breast %>% 
  summarise(all = n())
cb <- status_breast %>% 
  filter(Followup == "complete remission") %>% 
  summarise(cb = n())
db <- status_breast %>% 
  filter(Followup == "death") %>% 
  summarise(db = n())
pb <- status_breast %>% 
  filter(Followup == "progression") %>% 
  summarise(pb = n())
ub <- status_breast %>% 
  filter(Followup == "unknown") %>% 
  summarise(ub = n())


complet_remission <- as.numeric(cb/all)
death <- as.numeric(db/all)
progression <- as.numeric(pb/all)
unknown <- as.numeric(ub/all)

c <- c(complet_remission, death, progression, unknown)
n <- c("complet_remission", "death", "progression", "unknown")
df_breast <- data.frame(n,c)

#Ovary cancer - status %

all <- status_ovary %>% 
  summarise(all = n())
cb <- status_ovary %>% 
  filter(Followup == "complete remission") %>% 
  summarise(cb = n())
db <- status_ovary %>% 
  filter(Followup == "death") %>% 
  summarise(db = n())
pb <- status_ovary %>% 
  filter(Followup == "progression") %>% 
  summarise(pb = n())
ub <- status_ovary %>% 
  filter(Followup == "unknown") %>% 
  summarise(ub = n())


complet_remission <- as.numeric(cb/all)
death <- as.numeric(db/all)
progression <- as.numeric(pb/all)
unknown <- as.numeric(ub/all)

c <- c(complet_remission, death, progression, unknown)
n <- c("complet_remission", "death", "progression", "unknown")
df_ovary <- data.frame(n,c)

df_breast$type <- "Breast cancer"
df_ovary$type <- "Ovary cancer"

df_all <- rbind(df_breast, df_ovary)

ggplot() +
  geom_col(data = df_all, aes(y = c, x = n,
                              fill = n), 
           position_dodge(), show.legend = FALSE )+
  geom_col(data = df_all, aes(y = c, x = n,
                              fill = n),
           position_dodge(), show.legend = FALSE)+
  facet_grid(~type)+
  scale_y_continuous(labels = scales::percent)+
  ylab("Patient") +
  xlab("Status")+
  ggtitle("Patients' disease status") +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))

ggsave(filename = "Figures/compare_breast_ovary_cancer_boxplot_disease_status.png", width = 15, height = 10, scale = 0.5, dpi = 600)
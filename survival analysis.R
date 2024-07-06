#prepare
rm(list = ls()) # remove all objects that have been generated before

# read data
mydata = read.csv('Breast Cancer METABRIC.csv') 
str(mydata)




#visualize the data 
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)


# Plot for Age at Diagnosis
p1 <- ggplot(mydata, aes(x = `Age.at.Diagnosis`)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = 'Age at Diagnosis Distribution', x = 'Age', y = '')


# Plot for Cancer Type
p2 <- ggplot(mydata, aes(x = `Cancer.Type`)) +
  geom_bar(fill = "blue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  labs(title = 'Cancer Type Distribution', x = '', y = '')


# Arrange the plots in a 2x2 grid
grid.arrange(p1, p2, ncol = 2, nrow = 1)



# Plot for Overall Survival (Months)


f1 <- ggplot(mydata, aes(x = `Overall.Survival..Months.`)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(size = 6, margin = margin(b = 6))) + 
  labs(title = 'Overall Survival (Months) Distribution', x = '', y = '')

# Plot for Overall Survival Status
f2 <- ggplot(na.omit(mydata), aes(x = `Overall.Survival.Status`)) +
  geom_bar(fill = "blue", color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(size = 6, margin = margin(b = 6)))+
  labs(title = 'Overall Survival Status Distribution', x = '', y = '')

# Plot for Relapse Free Status (Months)
f3 <- ggplot(mydata, aes(x = `Relapse.Free.Status..Months.`)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(size = 6, margin = margin(b = 6)))+
  labs(title = 'Relapse Free Status (Months) Distribution', x = '', y = '')

# Plot for Relapse Free Status
f4 <- ggplot(na.omit(mydata), aes(x = `Relapse.Free.Status`)) +
  geom_bar(fill = "blue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(size = 6, margin = margin(b = 6)))+
  labs(title = 'Relapse Free Status Distribution', x = '', y = '')

# Arrange the plots in a 2x2 grid
grid.arrange(f1, f2, f3, f4, ncol = 2, nrow = 2)

##visualize missing values
# Calculate the number of missing values per column
mydata[mydata == ""] <- NA
missing_values <- colSums(is.na(mydata))

# Create a data frame for plotting
missing_data <- data.frame(
  Columns = names(missing_values),
  MissingValues = missing_values
)

# Sort the data frame by the number of missing values
missing_data <- missing_data[order(missing_data$MissingValues, decreasing = TRUE), ]

# Create the bar plot
ggplot(missing_data, aes(x = MissingValues, y = reorder(Columns, MissingValues))) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(
    title = 'Missing Values in Columns',
    x = 'Number of Missing Rows',
    y = ''
  )
##Missing values in event columns are filled with their most common value and missing values in duration columns are 
#filled with most common values of Cancer Type Detailed, Event groups
#Missing values in ER, PR and HER2 Status columns are filled with the most common values of their measurement 
#technique columnsز Missing values in Chemotherapy, Hormone therapy, and Radio therapy are filled with the most 
#common values in Cancer Type Detailed groups. Missing values in other columns are filled with modes or medians 
library(dplyr)

# Define a mode function
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Event and duration columns
mydata <- mydata %>%
  group_by(`Cancer.Type.Detailed`) %>%
  mutate(`Relapse.Free.Status` = ifelse(is.na(`Relapse.Free.Status`), mode(`Relapse.Free.Status`), `Relapse.Free.Status`)) %>%
  ungroup() %>%
  group_by(`Cancer.Type.Detailed`, `Relapse.Free.Status`) %>%
  mutate(`Relapse.Free.Status..Months.` = ifelse(is.na(`Relapse.Free.Status..Months.`), mean(`Relapse.Free.Status..Months.`, na.rm = TRUE), `Relapse.Free.Status..Months.`)) %>%
  ungroup() %>%
  group_by(`Cancer.Type.Detailed`, `Relapse.Free.Status`) %>%
  mutate(`Overall.Survival.Status` = ifelse(is.na(`Overall.Survival.Status`), mode(`Overall.Survival.Status`), `Overall.Survival.Status`)) %>%
  ungroup() %>%
  group_by(`Cancer.Type.Detailed`, `Overall.Survival.Status`) %>%
  mutate(`Overall.Survival..Months.` = ifelse(is.na(`Overall.Survival..Months.`), mean(`Overall.Survival..Months.`, na.rm = TRUE), `Overall.Survival..Months.`)) %>%
  ungroup()

# ER, PR, HER2 status columns
mydata <- mydata %>%
  mutate(`ER.status.measured.by.IHC` = ifelse(is.na(`ER.status.measured.by.IHC`), mode(`ER.status.measured.by.IHC`), `ER.status.measured.by.IHC`)) %>%
  group_by(`ER.status.measured.by.IHC`) %>%
  mutate(`ER.Status` = ifelse(is.na(`ER.Status`), mode(`ER.Status`), `ER.Status`)) %>%
  ungroup() %>%
  mutate(`HER2.status.measured.by.SNP6` = ifelse(is.na(`HER2.status.measured.by.SNP6`), mode(`HER2.status.measured.by.SNP6`), `HER2.status.measured.by.SNP6`)) %>%
  group_by(`HER2.status.measured.by.SNP6`) %>%
  mutate(`HER2.Status` = ifelse(is.na(`HER2.Status`), mode(`HER2.Status`), `HER2.Status`)) %>%
  ungroup() %>%
  group_by(`Cancer.Type.Detailed`) %>%
  mutate(`PR.Status` = ifelse(is.na(`PR.Status`), mode(`PR.Status`), `PR.Status`)) %>%
  ungroup()

# Chemotherapy, Hormone Therapy, Radio Therapy columns
mydata <- mydata %>%
  group_by(`Cancer.Type.Detailed`) %>%
  mutate(`Chemotherapy` = ifelse(is.na(`Chemotherapy`), mode(`Chemotherapy`), `Chemotherapy`),
         `Hormone.Therapy` = ifelse(is.na(`Hormone.Therapy`), mode(`Hormone.Therapy`), `Hormone.Therapy`),
         `Radio.Therapy` = ifelse(is.na(`Radio.Therapy`), mode(`Radio.Therapy`), `Radio.Therapy`)) %>%
  ungroup()

# Other columns
mydata <- mydata %>%
  group_by(`Cancer.Type.Detailed`) %>%
  mutate(`Age.at.Diagnosis` = ifelse(is.na(`Age.at.Diagnosis`), mean(`Age.at.Diagnosis`, na.rm = TRUE), `Age.at.Diagnosis`),
         `Cohort` = ifelse(is.na(`Cohort`), median(`Cohort`, na.rm = TRUE), `Cohort`),
         `Cellularity` = ifelse(is.na(`Cellularity`), mode(`Cellularity`), `Cellularity`)) %>%
  ungroup() %>%
  group_by(`Cancer.Type.Detailed`, `Cellularity`) %>%
  mutate(`Tumor.Stage` = ifelse(is.na(`Tumor.Stage`), median(`Tumor.Stage`, na.rm = TRUE), `Tumor.Stage`)) %>%
  ungroup() %>%
  group_by(`Cancer.Type.Detailed`) %>%
  mutate(`Tumor.Stage` = ifelse(is.na(`Tumor.Stage`), median(`Tumor.Stage`, na.rm = TRUE), `Tumor.Stage`)) %>%
  ungroup() %>%
  group_by(`Cancer.Type.Detailed`, `Tumor.Stage`) %>%
  mutate(`Tumor.Size` = ifelse(is.na(`Tumor.Size`), median(`Tumor.Size`, na.rm = TRUE), `Tumor.Size`)) %>%
  ungroup() %>%
  group_by(`Cancer.Type.Detailed`) %>%
  mutate(`Tumor.Size` = ifelse(is.na(`Tumor.Size`), median(`Tumor.Size`, na.rm = TRUE), `Tumor.Size`)) %>%
  ungroup() %>%
  mutate(`Tumor.Size` = ifelse(is.na(`Tumor.Size`), mode(`Tumor.Size`), `Tumor.Size`)) %>%
  group_by(`Cancer.Type.Detailed`) %>%
  mutate(`Neoplasm.Histologic.Grade` = ifelse(is.na(`Neoplasm.Histologic.Grade`), mode(`Neoplasm.Histologic.Grade`), `Neoplasm.Histologic.Grade`),
         `Primary.Tumor.Laterality` = ifelse(is.na(`Primary.Tumor.Laterality`), mode(`Primary.Tumor.Laterality`), `Primary.Tumor.Laterality`),
         `Tumor.Other.Histologic.Subtype` = ifelse(is.na(`Tumor.Other.Histologic.Subtype`), mode(`Tumor.Other.Histologic.Subtype`), `Tumor.Other.Histologic.Subtype`)) %>%
  ungroup() %>%
  mutate(`Tumor.Other.Histologic.Subtype` = ifelse(is.na(`Tumor.Other.Histologic.Subtype`), 'Ductal/NST', `Tumor.Other.Histologic.Subtype`)) %>%
  group_by(`Cancer.Type.Detailed`) %>%
  mutate(`Mutation.Count` = ifelse(is.na(`Mutation.Count`), mode(`Mutation.Count`), `Mutation.Count`)) %>%
  ungroup() %>%
  mutate(`Nottingham.prognostic.index` = ifelse(is.na(`Nottingham.prognostic.index`), median(`Nottingham.prognostic.index`, na.rm = TRUE), `Nottingham.prognostic.index`),
         `Lymph.nodes.examined.positive` = ifelse(is.na(`Lymph.nodes.examined.positive`), mode(`Lymph.nodes.examined.positive`), `Lymph.nodes.examined.positive`),
         `Inferred.Menopausal.State` = ifelse(is.na(`Inferred.Menopausal.State`), mode(`Inferred.Menopausal.State`), `Inferred.Menopausal.State`)) %>%
  group_by(`Cancer.Type.Detailed`) %>%
  mutate(`Pam50...Claudin.low.subtype` = ifelse(is.na(`Pam50...Claudin.low.subtype`), mode(`Pam50...Claudin.low.subtype`), `Pam50...Claudin.low.subtype`),
         `Integrative.Cluster` = ifelse(is.na(`Integrative.Cluster`), mode(`Integrative.Cluster`), `Integrative.Cluster`),
         `Type.of.Breast.Surgery` = ifelse(is.na(`Type.of.Breast.Surgery`), mode(`Type.of.Breast.Surgery`), `Type.of.Breast.Surgery`),
         `Primary.Tumor.Laterality` = ifelse(is.na(`Primary.Tumor.Laterality`), mode(`Primary.Tumor.Laterality`), `Primary.Tumor.Laterality`),
         `X3.Gene.classifier.subtype` = ifelse(is.na(`X3.Gene.classifier.subtype`), mode(`X3.Gene.classifier.subtype`), `X3.Gene.classifier.subtype`)) %>%
  ungroup()



# Converting categorical variables to factors
mydata <- mydata %>%
  mutate(
    `Type.of.Breast.Surgery` = as.factor(`Type.of.Breast.Surgery`),
    `Cancer.Type` = as.factor(`Cancer.Type`),
    `Cancer.Type.Detailed` = as.factor(`Cancer.Type.Detailed`),
    `Cellularity` = as.factor(`Cellularity`),
    `Chemotherapy` = as.factor(`Chemotherapy`),
    `Pam50...Claudin.low.subtype` = as.factor(`Pam50...Claudin.low.subtype`),
    `Cohort` = as.factor(`Cohort`),
    `ER.status.measured.by.IHC` = as.factor(`ER.status.measured.by.IHC`),
    `ER.Status` = as.factor(`ER.Status`),
    `Neoplasm.Histologic.Grade` = as.factor(`Neoplasm.Histologic.Grade`),
    `HER2.status.measured.by.SNP6` = as.factor(`HER2.status.measured.by.SNP6`),
    `HER2.Status` = as.factor(`HER2.Status`),
    `Tumor.Other.Histologic.Subtype` = as.factor(`Tumor.Other.Histologic.Subtype`),
    `Hormone.Therapy` = as.factor(`Hormone.Therapy`),
    `Inferred.Menopausal.State` = as.factor(`Inferred.Menopausal.State`),
    `Integrative.Cluster` = as.factor(`Integrative.Cluster`),
    `Primary.Tumor.Laterality` = as.factor(`Primary.Tumor.Laterality`),
    `Lymph.nodes.examined.positive` = as.factor(`Lymph.nodes.examined.positive`),
    `Oncotree.Code` = as.factor(`Oncotree.Code`),
    `Overall.Survival.Status` = as.factor(`Overall.Survival.Status`),
    `PR.Status` = as.factor(`PR.Status`),
    `Radio.Therapy` = as.factor(`Radio.Therapy`),
    `Relapse.Free.Status` = as.factor(`Relapse.Free.Status`),
    `Sex` = as.factor(`Sex`),
    `X3.Gene.classifier.subtype` = as.factor(`X3.Gene.classifier.subtype`),
    `Tumor.Stage` = as.factor(`Tumor.Stage`),
    `Patient.s.Vital.Status` = as.factor(`Patient.s.Vital.Status`)
  )

str(mydata)


#Kaplan-Meier curve
install.packages('survival')
install.packages('survminer')
install.packages("xfun")

library(survival)
library(survminer)
library(ggplot2)


sfit <- survfit(Surv(Overall.Survival..Months., Overall.Survival.Status=='Deceased')~Type.of.Breast.Surgery, data=mydata)
plot(sfit)
ggsurvplot(sfit, data=mydata, conf.int = TRUE, pval = TRUE, risk.table = TRUE, risk.table.height= 0.25  )

#cox regression
coxmodel <- coxph(Surv(Overall.Survival..Months., Overall.Survival.Status=='Deceased')~Type.of.Breast.Surgery, data=mydata) 
coxmodel
exp(confint(coxmodel))
#hazard ratio: 1.3858     p value: 1.24e-07      confidence interval: 1.227971 - 1.563978




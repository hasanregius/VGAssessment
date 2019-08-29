###########################
# Verge Genomics Assessment
# Hasan Sulaeman
###########################

# Data and library setup ----
## Library Setup ----
library(survival) 
library(ranger) 
library(ggplot2)
library(dplyr)
library(ggfortify)

## Reading CSV ----
plate_data = read.csv("ValidationStudy88_PlateID246 (1).csv")

## Doublechecking for NAs ----
table(is.na(plate_data))



# Kaplan-Meier Analysis ----
km_plate_data = with(plate_data, Surv(cell_last_tp, cell_event))
km_plate_data_fit = survfit(Surv(cell_last_tp, cell_event) ~ well_drug_name, data = plate_data)
## NOTE: We're ignoring dosage as a factor since 
##       each drug treatment has only one dose level for each treatment


# Plotting the Kaplan-Meier Survivorship Curves ----
plate_data_plot = autoplot(km_plate_data_fit, conf.int = TRUE, xlim = c(0, 10), xlab = "Time (Days)", ylab = "Percent Survival", 
                        main = "Validaton Study 88, Plate ID: 88 Survivorship Curve")
plate_data_plot + theme_classic()


# Cox proportional hazards model, constant covariates ----
coxph.fit = coxph(Surv(cell_last_tp, cell_event) ~ well_drug_name,
                  method="breslow", data = plate_data)
print(coxph.fit)

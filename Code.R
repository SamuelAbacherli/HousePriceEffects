# ------------------------------------------------------------------------------
# WORKING DIRECTORY
# ------------------------------------------------------------------------------

setwd("/Users/samu_hugo/Google Drive/Career/Education/Universität St. Gallen/3_Spring 2019/Research Seminar Financial Economics (3)")

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(data.table)
library(stargazer)
library(dplyr)
library(xtable)

# ------------------------------------------------------------------------------
# DATA IMPORT
# ------------------------------------------------------------------------------

# HOUSING DATA
file <- "./Statistics/Housingunits.csv"
housingunits <- fread(file, header = TRUE)

file <- "./Statistics/Housingrents.csv"
housingrents <- fread(file, header = TRUE)

# MAIN DATA

file <- "./Statistics/Industries.csv"
industries <- fread(file, header = TRUE)

file <- "./Statistics/Legalform.csv"
legalform <- fread(file, header = TRUE)

file <- "./Statistics/Herfindahl.csv"
herfindahl <- fread(file, header = TRUE)

file <- "./Statistics/Startingcapital.csv"
startingcapital <- fread(file, header = TRUE)

# CONTROL VARIABLES
file <- "./Statistics/Population.csv"
population <- fread(file, header = TRUE)

file <- "./Statistics/Graduates.csv"
graduates <- fread(file, header = TRUE)

file <- "./Statistics/Employment.csv"
employment <- fread(file, header = TRUE)

file <- "./Statistics/Owners.csv"
owners <- fread(file, header = TRUE)

# WEIGHTS
file <- "./Statistics/Households.csv"
households <- fread(file, header = TRUE)

# ------------------------------------------------------------------------------
# DATA AGGREGATION
# ------------------------------------------------------------------------------

# # STARTING CAPITAL
# median <- median(startingcapital$Capitalratio)
# for (x in 1:nrow(startingcapital)) {
#   if (startingcapital$Capitalratio[x] > median) {
#     startingcapital$Capitalintensive[x] <- 1
#   } else {
#     startingcapital$Capitalintensive[x] <- 0
#   }
# }

# ------------------------------------------------------------------------------
# FIGURES
# ------------------------------------------------------------------------------

writeLines(capture.output(stargazer(herfindahl[,2])), "./Figures/herfindahl.tex")

writeLines(capture.output(stargazer(housingprices, elasticities)), "./Figures/summarystatistics.tex")

writeLines(capture.output(stargazer(housingprices)), "./Figures/rentalprices.tex")

# ------------------------------------------------------------------------------
# FIRST STAGE
# ------------------------------------------------------------------------------

# HOUSING SUPPLY ELASTICITIES
elasticities <- -exp(housingrents / housingunits)
names(elasticities) <- "Elasticities"

population <- log(population)

model1 <- lm(housingrents$`Change in Housing Rents` ~ elasticities$Elasticities + population$Population + graduates$Graduates + employment$Employment + owners$Owners)
intercept <- model$coefficients[1]
slope <- model$coefficients[2]

housingprices <- intercept + slope * elasticities

writeLines(capture.output(stargazer(model1, style = "aer", title = "First Stage: House Prices")), "./Figures/firststage.tex")

# ------------------------------------------------------------------------------
# SECOND STAGE
# ------------------------------------------------------------------------------
panel <- na.omit(industries[,1:14])
if (panel[which(panel$Companies == 'Inf'), which = TRUE]) {
  panel <- panel[-panel[which(panel$Companies == 'Inf'), which = TRUE], ]
} else if (panel[which(panel$Employees == 'Inf'), which = TRUE]) {
  panel <- panel[-panel[which(panel$Employees == 'Inf'), which = TRUE], ]
}
panel$Establishment <- as.factor(panel$Establishment)
panel$Size <- as.factor(panel$Size)
panel$NOGA <- as.factor(panel$NOGA)
panel$ID <- as.factor(panel$ID)
panel$Nr <- as.factor(panel$Nr)

# SMALL VS LARGE ESTABLISHMENT SIZES
# BUSINESS CREATION ~ HOUSEPRICES + SIZE + HOUSEPRICES * SIZE + POPULATION + GRADUATES + EMPLOYMENT + OWNERS
model2 <- lm(Companies ~ Houseprices * Establishment + Population + Graduates + Employment + Owners,
             data = panel,
             weights = Households)

# writeLines(capture.output(stargazer(model, style = "aer", title = "Second Stage: Small vs Large Establishments")), "./Figures/establishmentsize.tex")

# TRADABLE VS NON-TRADABLE INDUSTRIES
panel <- na.omit(industries[,c(1:14, 17)])
if (panel[which(panel$Companies == 'Inf'), which = TRUE]) {
  panel <- panel[-panel[which(panel$Companies == 'Inf'), which = TRUE], ]
} else if (panel[which(panel$Employees == 'Inf'), which = TRUE]) {
  panel <- panel[-panel[which(panel$Employees == 'Inf'), which = TRUE], ]
}
panel$Establishment <- as.factor(panel$Establishment)
panel$Size <- as.factor(panel$Size)
panel$NOGA <- as.factor(panel$NOGA)
panel$ID <- as.factor(panel$ID)
panel$Nr <- as.factor(panel$Nr)
panel$Employeeherfindahl <- as.factor(panel$Employeeherfindahl)

# BUSINESS CREATION ~ HOUSEPRICES + SIZE + HOUSEPRICES * SIZE + TRADABILITY + POPULATION + GRADUATES + EMPLOYMENT + OWNERS
model3 <- lm(Companies ~ Houseprices * Establishment + Employeeherfindahl + Population + Graduates + Employment + Owners,
            data = panel,
            weights = Households)

# writeLines(capture.output(stargazer(model, style = "aer", title = "Second Stage: Tradable vs Non-Tradable Industries")), "./Figures/tradability.tex")

# SMALL VS LARGE STARTING CAPITAL
panel <- na.omit(industries[,1:15])
if (panel[which(panel$Companies == 'Inf'), which = TRUE]) {
  panel <- panel[-panel[which(panel$Companies == 'Inf'), which = TRUE], ]
} else if (panel[which(panel$Employees == 'Inf'), which = TRUE]) {
  panel <- panel[-panel[which(panel$Employees == 'Inf'), which = TRUE], ]
}
panel$Establishment <- as.factor(panel$Establishment)
panel$Size <- as.factor(panel$Size)
panel$NOGA <- as.factor(panel$NOGA)
panel$ID <- as.factor(panel$ID)
panel$Nr <- as.factor(panel$Nr)
panel$Capitalintensive <- as.factor(panel$Capitalintensive)

# BUSINESS CREATION ~ HOUSEPRICES + SIZE + HOUSEPRICES * SIZE + CAPITALINTENSIVE + POPULATION + GRADUATES + EMPLOYMENT + OWNERS
model4 <- lm(Companies ~ Houseprices * Establishment + Capitalintensive + Population + Graduates + Employment + Owners,
            data = panel,
            weights = Households)

# writeLines(capture.output(stargazer(model, style = "aer", title = "Second Stage: Low vs High Start Capital")), "./Figures/startingcapital.tex")

# SOLE PROPRIOTORSHIPS VS OTHER LEGAL FORMS
panel <- na.omit(legalform)
if (panel[which(panel$Companies == 'Inf'), which = TRUE]) {
  panel <- panel[-panel[which(panel$Companies == 'Inf'), which = TRUE], ]
} else if (panel[which(panel$Employees == 'Inf'), which = TRUE]) {
  panel <- panel[-panel[which(panel$Employees == 'Inf'), which = TRUE], ]
}
# BUSINESS CREATION ~ HOUSEPRICES + SIZE + HOUSEPRICES * SIZE + LEGALFORM + POPULATION + GRADUATES + EMPLOYMENT + OWNERS
model5 <- lm(Companies ~ Houseprices * Legalform + Population + Graduates + Employment + Owners,
            data = panel,
            weights = Households)

# writeLines(capture.output(stargazer(model, style = "aer", title = "Second Stage: Sole Propriotorships vs Other Legal Forms")), "./Figures/legalforms.tex")

writeLines(capture.output(stargazer(model1, model2, model3, model4, style = "aer")), "./Figures/allmodels.tex")

                          
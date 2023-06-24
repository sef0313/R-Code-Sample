
library(readr)
library(tidyr)
library(dplyr)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Johns Hopkins /Capstone")

PaidLeave <- read.csv("PaidLeave.csv")
FemaleInsurance19_64 <- read.csv("femaleinsurance19_64.csv")
breastfed <- read.csv("breastfed.csv")
births <- read.csv("births.csv")
MaternalMortality <- read.csv("MaternalMortality.csv")

# Join data sets
df2 <- merge(x = PaidLeave, y = FemaleInsurance19_64, by = "Location", all = T)
df3 <- merge(x = df2, y = breastfed, by = "Location", all = T)
WomensHealth <- merge(x = df3, y = births, by = "Location", all = T) %>% as.data.frame()

# Rename variables and drop NA variables 
WomensHealth <- WomensHealth %>% rename(state = Location,
                        paidmedfam = Paid.Family.Leave.and.Medical.Leave,
                        paidsick = Paid.Sick.Leave,
                        anypaid = Any.Paid.Time.Off,
                        employer = Employer,
                        nongroup = Non.Group,
                        medicaid = Medicaid,
                        medicare = Medicare,
                        military = Military,
                        uninsured = Uninsured,
                        everbreast = Ever.Breastfed,
                        breastfed6mo = Breastfeeding.at.6.months,
                        breastfed12mo = Breastfeeding.at.12.months,
                        numbirths = Number.of.Births) %>%
                  subset(select = -c(Total)) %>%
                  drop_na()

# Recode into dummy variables 
WomensHealth$paidmedfam[WomensHealth$paidmedfam == "Yes"] <- 1
WomensHealth$paidmedfam[WomensHealth$paidmedfam == "No"] <- 0

WomensHealth$paidsick[WomensHealth$paidsick == "Yes"] <- 1
WomensHealth$paidsick[WomensHealth$paidsick == "No"] <- 0

WomensHealth$anypaid[WomensHealth$anypaid == "Yes"] <- 1
WomensHealth$anypaid[WomensHealth$anypaid == "No"] <- 0

WomensHealth <- read.csv("WomensHealth.csv") %>% as.data.frame()
PoorHealth <- read.csv("PoorHealth.csv")
BirthRate <- read.csv("BirthRate.csv")
InfantMort <- read.csv("InfantMort.csv")
MaternalMortality <- read.csv("MaternalMortality.csv")
ChildImmunization <- read.csv("ChildImmunization.csv")

head(WomensHealth)

WomensHealth <- merge(x = WomensHealth, y = PoorHealth, by = "state", all = T)
WomensHealth <- merge(x = WomensHealth, y = BirthRate, by = "state", all = T)
WomensHealth <- merge(x = WomensHealth, y = InfantMort, by = "state", all = T)
WomensHealth <- merge(x = WomensHealth, y = MaternalMortality, by = "state", all = T)
WomensHealth <- merge(x = WomensHealth, y = ChildImmunization, by = "state", all = T)

WomensHealth <- WomensHealth %>% subset(WomensHealth, select = -c(X,X.1,X.2,X.3,X.4,X.5,X.6,X.7)) 

write.csv(WomensHealth, "WomensHealth2.0.csv")
WomensHealth2.0_clean <- read_csv("WomensHealth2.0.csv") %>% as.data.frame()

# add race and medinc vars 

race <- read.csv("race.csv")
medinc <- read.csv("MedianIncome.csv")

write.csv(WomensHealth2.0_clean, "WomensHealth4.0.csv")

WomensHealth4.0 <- read_csv("WomensHealth4.0.csv") %>% as.data.frame()
WomensHealth4.0 <- merge(x = WomensHealth4.0, y = race, by = "state", all = T)
WomensHealth4.0 <-merge(x = WomensHealth4.0, y = medinc, by = "state", all = T)

WomensHealth5.0 <- WomensHealth4.0[-c(3,13,38,43,49,52),-c(2,3,17:24)] #deleting NA columns and rows
WomensHealth5.0 <- cbind(WomensHealth5.0, implemented=0)

WomensHealth5.0$state

# replacing values in the implemented column

WomensHealth5.0$implemented[WomensHealth5.0$implemented == 0 & WomensHealth5.0$state == "California"] <- 1
WomensHealth5.0$implemented[WomensHealth5.0$implemented == 0 & WomensHealth5.0$state == "New York"] <- 1
WomensHealth5.0$implemented[WomensHealth5.0$implemented == 0 & WomensHealth5.0$state == "New Jersey"] <- 1
WomensHealth5.0$implemented[WomensHealth5.0$implemented == 0 & WomensHealth5.0$state == "Rhode Island"] <- 1

write.csv(WomensHealth5.0, "WomensHealth5.0.csv")



















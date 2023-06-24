
library(stargazer)
library(ggplot2)
library(readr)

data <- read.csv("WomensHealth5.0.csv") %>% as.data.frame() 
data <- subset(data, select = -c(X))

?stargazer

############## Descriptive Statistics ####################

data$state <- as.factor(data$state)
data_sumvars <- subset(data, select = c(state,implemented,uninsured,poorhealth,population,infantmort,everbreast, 
                                        breastfed6mo,breastfed12mo,immunized,maternmort,nummaterndeath)) %>% as.data.frame()

  # produce summary stats table
stargazer(data_sumvars,
                    digits = 3,
                    header = F,
                    type = "latex",
                    title = "Table 2. Summary Statistics",
                    model.numbers = F,
                    table.layout = "a",
                    out = "capstone_sumstats.html")

data_summary <- summary(data) # whole dataset 

stargazer(data_summary)

maternmort_noNA <- data %>% drop_na(maternmort) # remove NA from maternal mortality
maternmort_noNA <- subset(maternmort_noNA, select = -c(X)) # remove random column
maternmort_noNA_summary <- summary(maternmort_noNA) # create summary for states with maternal mortality data
stargazer(maternmort_noNA_summary)


############## OLS Regression Analysis ####################

# DV: Infant Mortality

infantmort_reg <- lm(infantmort ~ implemented, 
           data = data, na.action=na.omit) 

infantmort_reg2 <- lm(infantmort ~ implemented + uninsured, 
                      data = data, na.action=na.omit) 

infantmort_reg3 <- lm(infantmort ~ implemented + uninsured + poorhealth,
                      data = data, na.action=na.omit) 

stargazer(infantmort_reg,infantmort_reg2, infantmort_reg3,
          type = "latex",
          title = "Table 2. Infant Mortality Regression Results",
          dep.var.labels = "Infant Mortality Rate",
          out = "infantmort_reg.html",
          dep.var.caption = "",
          notes = "Source: KFF State Health Facts",
          omit.stat=c("adj.rsq","f"))

# DV: Ever Breastfed

everbreast_reg1 <- lm(everbreast ~ implemented,
                     data = data, na.action=na.omit) 

everbreast_reg2 <- lm(everbreast ~ implemented + uninsured,
                     data = data, na.action=na.omit) 

everbreast_reg3 <- lm(everbreast ~ implemented + uninsured + poorhealth, 
                     data = data, na.action=na.omit) 

stargazer(everbreast_reg1, everbreast_reg2, everbreast_reg3,
          type = "latex",
          title = "Table 3. Ever Breastfed Regression Results",
          dep.var.labels = "Ever Breastfed",
          out = "everbreastreg.html",
          dep.var.caption = "",
          omit.stat=c("adj.rsq","f"))


# DV: Breastfeeding at 6 months

bf6mo_reg <- lm(breastfed6mo ~ implemented,
            data = data, na.action=na.omit) 

bf6mo_reg2 <- lm(breastfed6mo ~ implemented + uninsured,
                data = data, na.action=na.omit) 

bf6mo_reg3 <- lm(breastfed6mo ~ implemented + uninsured + poorhealth,
                      data = data, na.action=na.omit) 

stargazer(bf6mo_reg,bf6mo_reg2,bf6mo_reg3,
          type = "latex",
          title = "Table 4. Breastfed at 6 Months Regression Results",
          dep.var.labels = "Breastfed at 6 Months",
          out = "bf6mo_reg.html",
          dep.var.caption = "",
          omit.stat=c("adj.rsq","f"))

# DV: Breastfeeding at 12 months

bf12mo_reg <- lm(breastfed12mo ~ implemented,
                data = data, na.action=na.omit) 

bf12mo_reg2 <- lm(breastfed12mo ~ implemented + uninsured,
                 data = data, na.action=na.omit) 

bf12mo_reg3 <- lm(breastfed12mo ~ implemented + uninsured + poorhealth, 
                 data = data, na.action=na.omit) 

stargazer(bf12mo_reg,bf12mo_reg2,bf12mo_reg3,
          type = "latex",
          title = "Table 5. Breastfed at 12 Months Regression Results",
          dep.var.labels = "Breastfed at 12 Months",
          out = "bf12mo_reg.html",
          dep.var.caption = "",
          omit.stat=c("adj.rsq","f"))


# DV: Immunized

immunized_reg <- lm(immunized ~ implemented,
                    data = data, na.action=na.omit) 

immunized_reg2 <- lm(immunized ~ implemented + uninsured, 
                    data = data, na.action=na.omit) 

immunized_reg3 <- lm(immunized ~ implemented + uninsured + poorhealth, 
                    data = data, na.action=na.omit) 

stargazer(immunized_reg,immunized_reg2, immunized_reg3,
          type = "latex",
          title = "Table 3. Immunized Regression Results",
          dep.var.labels = "Immunized",
          out = "immunized_reg.html",
          dep.var.caption = "",
          notes = "Source: KFF State Health Facts",
          omit.stat=c("adj.rsq","f"))


# DV: Number of Maternal Deaths

matdeath_reg <- lm(nummaterndeath ~ implemented + population,
                  data = data, na.action=na.omit) 

matdeath_reg2 <- lm(nummaterndeath ~ implemented + uninsured + population, 
                       data = data, na.action=na.omit) 

matdeath_reg3 <- lm(nummaterndeath ~ implemented + uninsured + poorhealth + population, 
                       data = data, na.action=na.omit)

stargazer(matdeath_reg, matdeath_reg2,matdeath_reg3,
          type = "latex",
          title = "Table 7. Number of Maternal Deaths Regression Results",
          dep.var.labels = "Number of Maternal Deaths",
          out = "maternaldeaths_reg.html",
          dep.var.caption = "",
          omit.stat=c("adj.rsq","f"))

# DV: Number of Maternal Deaths per 100k population

matdeath100 <- lm(materndeath100k ~ implemented,
                  data = data, na.action=na.omit) 

matdeath100_reg2 <- lm(materndeath100k ~ implemented + uninsured, 
                       data = data, na.action=na.omit) 

matdeath100_reg3 <- lm(materndeath100k ~ implemented + uninsured + poorhealth, 
                       data = data, na.action=na.omit)

stargazer( matdeath100, matdeath100_reg2, matdeath100_reg3,
           type = "latex",
           title = "Regression Results",
           dep.var.labels = "Maternal Mortality Rate per 100k Population",
           out = "maternaldeaths100k_reg.html",
           dep.var.caption = "",
           omit.stat=c("adj.rsq","f"))

# DV: Maternal Mortality 

maternalmort_reg <- lm(maternmort ~ implemented,
                         data = data, na.action=na.omit) 

maternalmort_reg2 <- lm(maternmort ~ implemented + uninsured,
                       data = data, na.action=na.omit) 

maternalmort_reg3 <- lm(maternmort ~ implemented + uninsured + poorhealth,
                       data = data, na.action=na.omit) 

stargazer(maternalmort_reg,maternalmort_reg2, maternalmort_reg3,
          type = "latex",
          title = "Table 8. Maternal Mortality Regression Results",
          dep.var.labels = "Maternal Mortality Rate",
          covariate.labels=c("PFL Implemented", "Uninsured", "Poor Health"),
          out = "maternalmort_reg.html",
          dep.var.caption = "",
          omit.stat=c("adj.rsq","f"))


# Regression Results

  # all MV models
stargazer(infantmort_reg3, everbreast_reg3, bf6mo_reg3, bf12mo_reg3, immunized_reg3, maternalmort_reg3, matdeath_reg3, 
          type = "latex",
          title = "Table 9. All Multivariate Regression Models",
          dep.var.labels=c("Infant Mortality Rate", "Ever Breastfed", "Breastfed at 6 Months", "Breastfed at 12 Months",
                           "Immunized", "Maternal Mortality Rate", "Maternal Deaths"),
          out = "all_regs.html",
          dep.var.caption = "",
          omit.stat=c("adj.rsq","f"))

  # infant health outcomes 
stargazer(infantmort_reg,infantmort_reg2, infantmort_reg3,immunized_reg,immunized_reg2, immunized_reg3,
          type = "latex",
          title = "Table 3. Regression Results: Infant Health Outcomes ",
          dep.var.labels = "Infant Mortality Rate",
          out = "infanthealth_reg.html",
          dep.var.caption = "",
          notes = "Source: KFF State Health Facts",
          omit.stat=c("adj.rsq","f"))



  # breastfeeding practices 

#ever breastfed 
stargazer(everbreast_reg1, everbreast_reg2, everbreast_reg3,
          type = "latex",
          title = "Table 4. Regression Results: Ever Breastfed",
          dep.var.labels = c("Ever Breastfed"),
          out = "everbf_reg.html",
          dep.var.caption = "",
          notes = "Source: KFF State Health Facts",
          omit.stat=c("adj.rsq","f"))

# 6 and 12 months
stargazer(bf6mo_reg,bf6mo_reg2,bf6mo_reg3,
          bf12mo_reg,bf12mo_reg2,bf12mo_reg3,
          type = "latex",
          title = "Table 5. Regression Results: Breastfed at 6 and 12 Months",
          dep.var.labels = c("Breastfed at 6 Months", "Breastfed at 12 Months"),
          out = "bf6and12mo_reg.html",
          dep.var.caption = "",
          notes = "Source: KFF State Health Facts",
          omit.stat=c("adj.rsq","f"))


# maternal health outcomes 
stargazer(matdeath_reg, matdeath_reg2,matdeath_reg3,maternalmort_reg,maternalmort_reg2, maternalmort_reg3,
          type = "latex",
          title = "Table 6. Regression Results: Maternal Health Outcomes",
          dep.var.labels = c("Number of Maternal Deaths", "Maternal Mortality Rate"),
          out = "maternalhealth_reg.html",
          dep.var.caption = "",
          notes = "Source: KFF State Health Facts",
          omit.stat=c("adj.rsq","f"))






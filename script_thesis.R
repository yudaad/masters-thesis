library(dplyr)
library(haven)
library(expss)
library(stringr)
library(tidyr)
library(readxl)
library(labelled)
library(ggplot2)
library(MetBrewer)
library(modelsummary)
library(stargazer)

# Import Data----
bk_sc1 <- read_dta('bk_sc1.dta') # Current residency data
b3a_mg1 <- read_dta('b3a_mg1.dta') # migration data
b3a_mg2 <- read_dta('b3a_mg2.dta') # migration destination data
b3a_tk1 <- read_dta('b3a_tk1.dta') # labor characteristics data
b3a_tk2 <- read_dta('b3a_tk2.dta') # labor characteristics data
bk_ar1 <- read_dta('bk_ar1.dta') # individual characteristics data
b3a_dl1 <- read_dta('b3a_dl1.dta') # individual education data
umr_14 <- read_excel('umr_prov_2014.xlsx')
gdrp_14 <- read_excel('gdrp14.xlsx')

#==============================================================================
# Working with bk_sc1 data (current residency data)----
# Create 4-digits BPS city code and dummy variable jabodetabek and metro
bk_sc1 <- bk_sc1 %>%
  mutate_at(c('sc01_14_14', 'sc02_14_14'), as.character) %>%
  mutate(kodekab = ifelse(str_length(sc02_14_14) == 1
                          , paste0("0", sc02_14_14), sc02_14_14),
         kabcode14 = paste0(sc01_14_14, kodekab),
         jabodetabek = ifelse(kabcode14 %in% c(3171, 3172, 3173, 3174, 3175,
                       3271, 3276, 3275, 3671, 3674, 3201, 3216, 3603), 1, 0),
         metro = ifelse(kabcode14 %in% c(3578, 1275, 3273, 7371, 3374, 1671, 
                                         1871, 2171), 1, 0))

# recode urban/rural variable to dummy variable
bk_sc1$sc05[bk_sc1$sc05 == 2] <- 0

# Finishing
# Select Data
city <- bk_sc1 %>%
  rename(urban = sc05,
         provcode14 = sc01_14_14) %>%
  select(hhid14, provcode14, kabcode14, urban, jabodetabek, metro)

# change data type
city$provcode14 <- as.character(city$provcode14)
city$kabcode14 <- as.character(city$kabcode14)
city$urban <- as.factor(city$urban)
city$jabodetabek <- as.factor(city$jabodetabek)
city$metro <- as.factor(city$metro)

# remove value labels
city$urban <- remove_val_labels(city$urban)

# Apply data labels
city <- apply_labels(city,
                     urban = "Urban",
                     provcode14 = "BPS Province Code",
                     kabcode14 = "BPS City Code",
                     jabodetabek = "Jabodetabek",
                     metro = "Metropolitan")


#==============================================================================
# Working with b3a_tk2 data (Working Characteristics Data)----

# Generate income variable
b3a_tk2$income <- rowSums(b3a_tk2[c("tk25a1", "tk26a1", "tk25b1", "tk26b1")], na.rm = T)

# Removing 0 values to NA if all variables are NA
b3a_tk2$income <- ifelse(is.na(b3a_tk2$tk25a1) & is.na(b3a_tk2$tk26a1) 
                      & is.na(b3a_tk2$tk25b1) & is.na(b3a_tk2$tk26b1)
                      , NA
                      , b3a_tk2$income)

b3a_tk2 <- b3a_tk2 %>%
  filter(between(income
                 , quantile(b3a_tk2$income, 0.1, na.rm = T)
                 , quantile(b3a_tk2$income, 0.95, na.rm = T)))


# Generate formal and informal workers category
b3a_tk2 <- b3a_tk2 %>%
  mutate(formal = case_when(
    tk24a == 3 | tk24a == 4 | tk24a == 5 ~ 1,
    tk24a == 1 | tk24a == 2 | tk24a == 6 | tk24a == 7 | tk24a == 8 ~ 0
  ))

# Rename and select variable that will be used
work <- b3a_tk2 %>%
  rename(sector = tk19ab,
         tenure = tk23a2y) %>%
  mutate(tenure2 = tenure^2,
         ln_income = log(income)) %>%
  select(hhid14, pid14, pidlink, income, ln_income, formal, sector, tenure, tenure2)

# Change data type
work$pid14 <- as.character(work$pid14)
work$formal <- as.factor(work$formal)
work$sector <- as.factor(work$sector)

varlist <- c(4,5,8,9)
work[varlist] <- sapply(work[varlist], as.numeric)

# Apply data label
work <- apply_labels(work,
                     sector = "Working Sector",
                     income = "Income",
                     ln_income = "Log Income",
                     formal = "Formal Sector",
                     tenure = "Tenure",
                     tenure2 = "Tenure Squared")

#==============================================================================
# Working with bk_ar1 data (Individual Characteristics Data)----

# Age variables (ar09)
bk_ar1 <- bk_ar1 %>%
  filter(!ar09 %in% c(998, 999),
         ar09 >= 15, ar09 <= 64) %>%
  mutate(age2 = ar09^2)

# Gender variables (ar07)
# Revalue gender variable
bk_ar1$ar07[bk_ar1$ar07 == 3] <- 0

# Transform into factor
bk_ar1$ar07 <- as.factor(bk_ar1$ar07)

# Marital status variable (ar13)
bk_ar1$ar13[bk_ar1$ar13 %in% c(1, 3, 4, 5, 6)] <- 0
bk_ar1$ar13[bk_ar1$ar13 %in% c(8, 9)] <- NA
bk_ar1$ar13[bk_ar1$ar13 == 2] <- 1

# Transform into factor
bk_ar1$ar07 <- as.factor(bk_ar1$ar07)

# remove value label
bk_ar1$ar07 <- remove_val_labels(bk_ar1$ar07)

# Household head variables (ar02b)
bk_ar1 <- bk_ar1 %>%
  mutate(hh_head = case_when(
    !is.na(ar02b) & ar02b == 1 ~ 1,
    !is.na(ar02b) & ar02b != 1 ~ 0
  ))

# Transform as factor
bk_ar1$hh_head <- as.factor(bk_ar1$hh_head)

# Household size variable
bk_ar1 <- bk_ar1 %>%
  group_by(hhid14) %>%
  mutate(hh_size = n())

bk_ar1$hh_size <- as.factor(bk_ar1$hh_size)

# Ethnic variable (ar15d)
bk_ar1$ar15d[bk_ar1$ar15d %in% c(98, 99, 95)] <- NA
bk_ar1$ar15d <- as.factor(bk_ar1$ar15d)

# Cleaning up the data
ch <- bk_ar1 %>%
  rename(age = ar09,
         male = ar07,
         married = ar13,
         ethnic = ar15d) %>%
  filter(ar01a %in% c(1, 2, 5, 11)) %>%
  select(hhid14, pid14, pidlink, age, male, married, hh_head, hh_size, ethnic)

# change data type
ch$pid14 <- as.character(ch$pid14)
ch$age <- as.integer(ch$age)
ch$married <- as.factor(ch$married)
ch$hh_size <- as.integer(ch$hh_size)

# Apply label variable
ch <- apply_labels(ch,
                   age = "Age",
                   male = "Male",
                   married = "Married",
                   hh_head = "Household Head",
                   hh_size = "Household Size",
                   ethnic = "Ethnicity")

#==============================================================================
# Working with b3a_dl1 data (Schooling Data)----

# Create years of schooling variable
b3a_dl1 <- b3a_dl1 %>%
  mutate(schooling = case_when(
    dl04 == 3 ~ 0,
    dl06 == 2 & dl07 == 0 ~ 0,
    
    # Kindergarten
    dl06 == 90 & (dl07 == 0 | dl07 == 1 | dl07 == 2 | dl07 == 3 | dl07 == 4 |
                    dl07 == 5 | dl07 == 6 | dl07 == 7 | dl07 == 98) ~ 0,
    
    # Elementary School
    dl06 == 2 & (dl07 == 1 | dl07 == 98) ~ 1,
    dl06 == 2 & dl07 == 2 ~ 2,
    dl06 == 2 & dl07 == 3 ~ 3,
    dl06 == 2 & dl07 == 4 ~ 4,
    dl06 == 2 & dl07 == 5 ~ 5,
    dl06 == 2 & (dl07 == 6 | dl07 == 7) ~ 6,
    
    # Education A
    dl06 == 11 & dl07 == 0 ~ 0,
    dl06 == 11 & (dl07 == 1 | dl07 == 2 | dl07 == 3 | dl07 == 98) ~ 1,
    dl06 == 11 & (dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 6,
    
    # Madrasah Ibtidaiyah
    dl06 == 72 & dl07 == 0 ~ 0,
    dl06 == 72 & (dl07 == 1 | dl07 == 98) ~ 1,
    dl06 == 72 & dl07 == 2 ~ 2,
    dl06 == 72 & dl07 == 3 ~ 3,
    dl06 == 72 & dl07 == 4 ~ 4,
    dl06 == 72 & dl07 == 5 ~ 5,
    dl06 == 72 & (dl07 == 6 | dl07 == 7) ~ 6,
    
    # Pesantren
    dl06 == 14 & dl07 == 0 ~ 0,
    dl06 == 14 & (dl07 == 1 | dl07 == 2 | dl07 == 3 | dl07 == 4 | dl07 == 5 |
                    dl07 == 6 | dl07 == 7 | dl07 == 98) ~ 6,
    
    # School for Disabled
    dl06 == 17 & dl07 == 0 ~ 0,
    dl06 == 17 & (dl07 == 1 | dl07 == 98) ~ 1,
    dl06 == 17 & dl07 == 2 ~ 2,
    dl06 == 17 & dl07 == 3 ~ 3,
    dl06 == 17 & dl07 == 4 ~ 4,
    dl06 == 17 & dl07 == 5 ~ 5,
    dl06 == 17 & (dl07 == 6 | dl07 == 7) ~ 6,

    # Junior High School
    dl06 == 3 & dl07 == 0 ~ 6,
    dl06 == 3 & (dl07 == 1 | dl07 == 98) ~ 7,
    dl06 == 3 & dl07 == 2 ~ 8,
    dl06 == 3 & (dl07 == 3 | dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 9,
    
    # Education B
    dl06 == 12 & dl07 == 0 ~ 6, 
    dl06 == 12 & (dl07 == 1 | dl07 == 2 | dl07 == 3 | dl07 == 98) ~ 7,
    dl06 == 12 & (dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 9,
    
    # Madrasah Tsanawiyah
    dl06 == 73 & dl07 == 0 ~ 6,
    dl06 == 73 & (dl07 == 1 | dl07 == 98) ~ 7,
    dl06 == 73 & dl07 == 2 ~ 8, 
    dl06 == 73 & (dl07 == 3 | dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 9,
    
    # Vocational Junior High School
    dl06 == 4 & dl07 == 0 ~ 6,
    dl06 == 4 & (dl07 == 1 | dl07 == 98) ~ 7,
    dl06 == 4 & dl07 == 2 ~ 8,
    dl06 == 4 & (dl07 == 3 | dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 9,
    
    # Senior High School
    dl06 == 5 & dl07 == 0 ~ 9,
    dl06 == 5 & (dl07 == 1 | dl07 == 98) ~ 10,
    dl06 == 5 & dl07 == 2 ~ 11,
    dl06 == 5 & (dl07 == 3 | dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 12,
    
    # Madrasah Aliyah
    dl06 == 74 & dl07 == 0 ~ 9,
    dl06 == 74 & (dl07 == 1 | dl07 == 98) ~ 10,
    dl06 == 74 & dl07 == 2 ~ 11,
    dl06 == 74 & (dl07 == 3 | dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 12,
    
    # Education C
    dl06 == 15 & dl07 == 0 ~ 9,
    dl06 == 15 & (dl07 == 1 | dl07 == 2 | dl07 == 3 | dl07 == 98) ~ 10,
    dl06 == 15 & (dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 12,
    
    # Vocational Senior High School
    dl06 == 6 & dl07 == 0 ~ 9,
    dl06 == 6 & (dl07 == 1 | dl07 == 98) ~ 10,
    dl06 == 6 & dl07 == 2 ~ 11,
    dl06 == 6 & (dl07 == 3 | dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 12,
    
    # Diploma Degree (D1/D2/D3)
    dl06 == 60 & dl07 == 0 ~ 12,
    dl06 == 60 & (dl07 == 1 | dl07 == 98) ~ 13,
    dl06 == 60 & dl07 == 2 ~ 14,
    dl06 == 60 & (dl07 == 3 | dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 15,
    
    # Undergraduate Degree
    dl06 == 61 & dl07 == 0 ~ 12,
    dl06 == 61 & (dl07 == 1 | dl07 == 98) ~ 13,
    dl06 == 61 & dl07 == 2 ~ 14,
    dl06 == 61 & dl07 == 3 ~ 15,
    dl06 == 61 & (dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 16,
    
    # Open University
    dl06 == 13 & dl07 == 0 ~ 12,
    dl06 == 13 & (dl07 == 1 | dl07 == 98) ~ 13,
    dl06 == 13 & dl07 == 2 ~ 14,
    dl06 == 13 & dl07 == 3 ~ 15,
    dl06 == 13 & (dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 16,
                    
    # Master's Degree
    dl06 == 62 & dl07 == 0 ~ 16,
    dl06 == 62 & (dl07 == 1 | dl07 == 98) ~ 17,
    dl06 == 62 & (dl07 == 2 | dl07 == 3 | dl07 == 4 | dl07 == 5 |
                    dl07 == 6 | dl07 == 7) ~ 18,
    
    # Doctoral Degree
    dl06 == 63 & dl07 == 0 ~ 18,
    dl06 == 63 & (dl07 == 1 | dl07 == 98) ~ 19,
    dl06 == 63 & dl07 == 2 ~ 20,
    dl06 == 63 & dl07 == 3 ~ 21,
    dl06 == 63 & (dl07 == 4 | dl07 == 5 | dl07 == 6 | dl07 == 7) ~ 22
  ))

# Finishing
yos <- b3a_dl1 %>%
  select(hhid14, pid14, pidlink, schooling)

# change data type
yos$schooling <- as.integer(yos$schooling)
yos$pid14 <- as.character(yos$pid14)

# labeling
yos <- apply_labels(yos,
                    schooling = "Years of Schooling")

#==============================================================================
# Working with Migration Data----

# Creating 4-digits BPS city code, and migration experience to Jabodetabek and metropolitan
b3a_mg2 <- b3a_mg2 %>%
  mutate_at(c('mg21d', 'mg21c'), as.character) %>%
  mutate(kodekab = ifelse(str_length(mg21c) == 1, paste0("0", mg21c), mg21c),
         kabcode14 = paste0(mg21d, kodekab),
         jabodetabek_exp = ifelse(kabcode14 %in% c(3171, 3172, 3173, 3174, 3175,
                 3271, 3276, 3275, 3671, 3674, 3201, 3216, 3603), 1, 0),
         metro_exp = ifelse(kabcode14 %in% c(3578, 1275, 3273, 7371, 3374, 1671, 
                 1871, 2171), 1, 0))

# Replacing zero with missing value (for better interpolation)
b3a_mg2$jabodetabek_exp[b3a_mg2$jabodetabek_exp == 0] <- NA
b3a_mg2$metro_exp[b3a_mg2$metro_exp == 0] <- NA

# Replacing missing values with non-missing value (if any) within group (interpolation)
b3a_mg2 <- b3a_mg2 %>%
  group_by(pidlink) %>%
  fill(c(jabodetabek_exp, metro_exp), .direction = 'updown')

# Replacing back missing value to zero
b3a_mg2$jabodetabek_exp[is.na(b3a_mg2$jabodetabek_exp)] <- 0
b3a_mg2$metro_exp[is.na(b3a_mg2$metro_exp)] <- 0

# Finishing
# select variables
mig <- b3a_mg2 %>%
  rename(provcode14_mig = mg21d,
         kabcode14_mig = kabcode14) %>%
  select(hhid14, pid14, pidlink, provcode14_mig, kabcode14_mig, jabodetabek_exp, metro_exp)

# Remove duplicates
mig <- distinct(mig, pidlink, .keep_all = TRUE)

# Eliminate those who are currently living in Jabodetabek and metropolitan area
# merge with city currently living
mig <- inner_join(city, mig, by = 'hhid14')

# recode value jabodetabek_exp/metro_exp to 0 if one's currently living in jabodetabek/metro
mig$jabodetabek_exp[mig$jabodetabek == 1] <- 0
mig$metro_exp[mig$metro == 1] <- 0

# Select data
mig <- mig %>%
  select(hhid14, pid14, pidlink, jabodetabek_exp, metro_exp)

# change data types
mig$jabodetabek_exp <- as.factor(mig$jabodetabek_exp)
mig$metro_exp <- as.factor(mig$metro_exp)
mig$pid14 <- as.character(mig$pid14)

# Insert label
mig <- apply_labels(mig,
                    jabodetabek_exp = "Jabodetabek Experience",
                    metro_exp = "Metropolitan Experience")

#==============================================================================
# Working with macro data ----
# cleaning up gdrp data
gdrp_14 <- gdrp_14 %>%
  mutate_at('kodekab14', as.character) %>%
  rename(kabcode14 = kodekab14,
         regency = nama_wilayah) %>%
  mutate(ln_gdrp14 = log(gdrp14)) %>%
  select(kabcode14, regency, gdrp14, ln_gdrp14)

gdrp_14 <- apply_labels(gdrp_14,
                        kabcode14 = "BPS City Code",
                        regency = "Regency/City Name",
                        gdrp14 = 'GDRP',
                        ln_gdrp14 = "Log GDRP")

# cleaning up minimum wage data
umr_14 <- umr_14 %>%
  mutate_at('kodeprov14', as.character) %>%
  rename(provcode14 = kodeprov14,
         province = provinsi,
         mw14 = umr14) %>%
  mutate(ln_mw14 = log(mw14)) %>%
  select(provcode14, province, mw14, ln_mw14)

umr_14 <- apply_labels(umr_14,
                       provcode14 = "BPS Province Code",
                       province = "Province",
                       mw14 = "Minimum Wage",
                       ln_mw14 = "Log Minimum Wage")
  

#==============================================================================
# Merging data----
# Merging individual characteristics and working data
compiled <- inner_join(ch, work, by = c('hhid14', 'pid14', 'pidlink'))

# merging compiled data with city currently living data
compiled <- inner_join(compiled, city, by = 'hhid14')

# merging with GDRP data
compiled <- inner_join(compiled, gdrp_14, by = 'kabcode14')

# merging with minimum wage data
compiled <- inner_join(compiled, umr_14, by = 'provcode14')

# merging with schooling data
compiled <- inner_join(compiled, yos, by = c('hhid14', 'pid14', 'pidlink'))

# merging with migration experience data
compiled <- left_join(compiled, mig, by = c('hhid14', 'pid14', 'pidlink'))

# assign zero value to NA in variable jabodetabek_exp and metro_exp
compiled$jabodetabek_exp[is.na(compiled$jabodetabek_exp)] <- 0
compiled$metro_exp[is.na(compiled$metro_exp)] <- 0

#==============================================================================
# Plotting Data ----

# Plot metropolitan experience and mean income, grouped by job informality
ggplot(data = compiled, aes(x = metro_exp, y = income, fill = formal)) +
  stat_summary(fun = "mean", geom = 'bar', position = 'dodge') +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=3.5,
               vjust = 5, position = position_dodge(width = 0.9), color = 'black') +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.9), 
               width=0.4, color = 'black') +
  scale_fill_manual(values=met.brewer("Isfahan2", n = 2)) +
  theme_classic()

# Plot jabodetabek experience and mean income, grouped by job informality
ggplot(data = compiled, aes(x = jabodetabek_exp, y = income, fill = formal)) +
  stat_summary(fun = "mean", geom = 'bar', position = 'dodge') +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=3.5,
               vjust = 5, position = position_dodge(width = 0.9), color = 'black') +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.9), 
               width=0.4, color = 'black') +
  scale_fill_manual(values=met.brewer("Isfahan2", n = 2)) +
  theme_classic()

# Plot living in Jabodetabek and mean income, grouped by job informality
ggplot(data = compiled, aes(x = jabodetabek, y = income, fill = formal)) +
  stat_summary(fun = "mean", geom = 'bar', position = 'dodge') +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=3.5,
               vjust = 5, position = position_dodge(width = 0.9), color = 'black') +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.9), 
               width=0.4, color = 'black') +
  scale_fill_manual(values=met.brewer("Isfahan2", n = 2)) +
  theme_classic()

# Plot living in metropolitan and mean income, grouped by job informality
ggplot(data = compiled, aes(x = metro, y = income, fill = formal)) +
  stat_summary(fun = "mean", geom = 'bar', position = 'dodge') +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=3.5,
               vjust = 5, position = position_dodge(width = 0.9), color = 'black') +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.9), 
               width=0.4, color = 'black') +
  scale_fill_manual(values=met.brewer("Isfahan2", n = 2)) +
  theme_classic()

#==============================================================================
# Regression ----
ols1 <- lm(ln_income ~ jabodetabek_exp + metro_exp + jabodetabek + metro + 
             tenure + tenure2 + formal + sector + schooling + age + 
             male + married + hh_head + hh_size + ln_gdrp14 + ln_mw14
             , data = compiled)


ols_informal <- lm(ln_income ~ jabodetabek_exp + metro_exp + jabodetabek + metro + 
                   tenure + tenure2 + sector + schooling + age + 
                   male + married + hh_head + hh_size + ln_gdrp14 + ln_mw14 
                   , data = compiled %>% filter(formal == 0))


ols_formal <- lm(ln_income ~ jabodetabek_exp + metro_exp + jabodetabek + metro + 
                     tenure + tenure2 + sector + schooling + age + 
                     male + married + hh_head + hh_size + ln_gdrp14 + ln_mw14
                     , data = compiled %>% filter(formal == 1))

stargazer(ols1, ols_informal, ols_formal
          , type = 'html'
          , out = 'regression_output.html')












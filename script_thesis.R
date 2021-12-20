library(dplyr)
library(haven)
library(expss)
library(stringr)
library(tidyverse)

# Set Working Directory
setwd("D:/R/thesis")

# Import Data----
b3a_mg1 <- read_dta('b3a_mg1.dta')
b3a_mg2 <- read_dta('b3a_mg2.dta')
b3a_tk1 <- read_dta('b3a_tk1.dta')
b3a_tk2 <- read_dta('b3a_tk2.dta')
bk_ar1 <- read_dta('bk_ar1.dta')
b3a_dl1 <- read_dta('b3a_dl1.dta')

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

# Apply data label
work <- apply_labels(work,
                     sector = "Working Sector",
                     income = "Income",
                     ln_income = "Log Income",
                     formal = "Formal Sector",
                     tenure = "Tenure",
                     tenure2 = "Tenure Squared")

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

ch <- apply_labels(ch,
                   age = "Age",
                   male = "Male",
                   married = "Married",
                   hh_head = "Household Head",
                   hh_size = "Household Size",
                   ethnic = "Ethnicity")

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

yos <- apply_labels(yos,
                    schooling = "Years of Schooling")


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

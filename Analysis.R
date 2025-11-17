library(haven)
library(tidyverse)
library(broom)
library(pROC)
library(probably)

#pulling together the different datasets from the years
df2024 <- read_xpt("/Users/ianholdroyd/Downloads/LLCP2024.xpt")
df2023 <- read_xpt("/Users/ianholdroyd/Downloads/LLCP2023.xpt")
df2022 <- read_xpt("/Users/ianholdroyd/Downloads/LLCP2022.xpt")

df2024$HLTHPL <- df2024$'_HLTHPL2'
df2023$HLTHPL <- df2023$'_HLTHPL1'
df2022$HLTHPL <- df2022$`_HLTHPLN`


df <- bind_rows(df2023, df2022)
df1 <- bind_rows(df, df2024)
df2_backupdf1 <- df1
df1<-df2_backupdf1

df$HLTHPL


#this gets the ace columns in a list which is helpful for later on
ace_cols <- grep("^ACE", names(df1), value = TRUE)

# extracting the columns that I wan't, and renaming them to something more intuiative

cols_to_get <-c("_STATE", "IMONTH", "IYEAR", "DISPCODE", "SEXVAR", "EDUCA", "MARITAL",
                "_METSTAT", "_URBSTAT", "_IMPRACE", "_LLCPWT", "_AGEG5YR","_INCOMG1", 
                "HLTHPL", "HPVADVC4", "HPVADSHT", "TETANUS1", "_PSU", "_STSTR",
                ace_cols)

new_name_cols <- c("state", "i_month", "i_year", "id", "sex", "edu", "marital",
                   "metropolitan", "urban", "race", "weight_survey", "age","income",
                   "insurance", "hpv_ever_had", "hpv_numberofshots", "tetanus", "psu", "strata",
                   ace_cols)

df1<- df1[cols_to_get]
df1<- df1|>
  rename(setNames(cols_to_get, new_name_cols))

# this bit is renaming the data within the file to 1. remove any number codes that actually mean NA, and also to change arouind the ACE columns so that they all mean the same thing
df1.mutatedvars <- df1 |>
  mutate(
    across(c(ace_cols, insurance, hpv_ever_had, race, marital), ~ ifelse(.x %in% c(7, 8, 9), NA, .x)),
    across(c(hpv_numberofshots), ~ ifelse(.x %in% c(77, 99), NA, .x)),
    across(c(age), ~ ifelse(.x %in% c(14), NA, .x)),
    across(c(income, edu), ~ ifelse(.x %in% c(9), NA, .x)),
    across(c(hpv_numberofshots), ~ifelse(.x%in% c(4, 10, 77, 99), NA, .x)),
    ACESEXCOMBINED = ifelse(
      rowSums(across(c(ACETOUCH, ACETTHEM, ACEHVSEX)) > 1, na.rm = TRUE) > 0, "Yes",
      ifelse(
        rowSums(across(c(ACETOUCH, ACETTHEM, ACEHVSEX)) == 1, na.rm = TRUE) == 3, "No",
        NA
      )
    ),
    across(c(ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, ACEDIVRC, insurance),
           ~ ifelse(.x == 1, "Yes", ifelse(.x == 2, "No", .x))),
    across(c(hpv_ever_had), ~ifelse(.x == 1, 1, ifelse((.x %in% c(NA, 3)), NA, 0))),
    across(c(ACEPUNCH, ACEHURT1, ACESWEAR, ACETOUCH, ACETTHEM, ACEHVSEX),
           ~ ifelse(.x > 1, "Yes", ifelse(.x == 1, "No", .x))),
    across(c(ACEADNED, ACEADSAF),
           ~ ifelse(.x > 2, "No", ifelse(.x < 3, "Yes", .x))),
    ACEANY = case_when(
      if_any(all_of(ace_cols), ~ .x == "Yes") ~ "Yes",
      if_all(all_of(ace_cols), ~ .x == "No") ~ "No",
      if_any(all_of(ace_cols), is.na) ~ NA_character_
    )
  )

table(df1.mutatedvars$insurance)

#gets covariates as discrete variables if this is appropriate
df1.mutatedvars<- df1.mutatedvars |> mutate(
  across(c(race, state, marital, insurance, age, income, edu), ~ as.character(.x)),
)


#  regression without confounders 
mod1 <- glm(data = df1.mutatedvars, formula(hpv_ever_had ~ ACEANY ), family = binomial(link="logit"), weights = weight_survey)
summary(mod1)
exp(mod1$coefficients)


# these tables show that there were only a handful of states that recorded information on ACEs and whether someone had had HPV vaccinaiton
table(df1.mutatedvars$hpv_ever_had, df1.mutatedvars$state)
table(df1.mutatedvars$ACEANY, df1.mutatedvars$state)


# makes a dataframe using only the rows that have complete data for all the points. this helps for making the ROC later
analysis_df.mod2 <- df1.mutatedvars[complete.cases(df1.mutatedvars[, c(
  "hpv_ever_had", "ACEANY", "state", "urban", "metropolitan", "insurance",
  "i_year", "sex", "edu", "income", "race"
)]),
]

#looking at the variables
table(analysis_df.mod2$edu)
table(analysis_df.mod2$hpv_ever_had)
table(analysis_df.mod2$ACEANY)
table(analysis_df.mod2$state)
table(analysis_df.mod2$urban)
table(analysis_df.mod2$metropolitan)
table(analysis_df.mod2$i_year)
table(analysis_df.mod2$sex)
table(analysis_df.mod2$income)
table(analysis_df.mod2$insurance)
table(analysis_df.mod2$race)
table(is.na(analysis_df.mod2$weight_survey), analysis_df.mod2$i_year)


# there's only 6 people who never attended school. This is going to result in silly standard errors using this as the reference to compare to. 
# I've reclassified into 1. didn't complete high school education, high school education, college education, and graduate college education
analysis_df.mod2<- analysis_df.mod2 |>
  mutate(
    edu = case_when(
      edu %in% c("1", "2", "3") ~ "less than high school",
      edu == 4 ~ "high school",
      edu == 5 ~ "college",
      edu == 6 ~ "graduate"
    )
  )



#relevel so that income of 4 is set as the reference
analysis_df.mod2$income <- relevel(factor(analysis_df.mod2$income), ref = "4")

table(analysis_df.mod2$tetanus)

analysis_df.mod2 <- analysis_df.mod2 |>
  mutate(
    tetanus1 = case_when(
      tetanus < 4 ~ 1,
      tetanus == 4 ~ 0,
      tetanus %in% c(7, 9) | is.na(tetanus) ~ NA_real_
    )
  )


#model using all the covariates
mod2 <- glm(data = analysis_df.mod2, formula(hpv_ever_had ~ ACEANY + state + urban + metropolitan + insurance + i_year *sex + edu+ income+race), family=binomial(link="logit"))
summary(mod2)
library(car)
vif(mod2)
mod2results <- tidy(mod2, exponentiate = TRUE, conf.int = TRUE)
colnames(mod2results)<- c("Term","OR", "SE", "Test Statistic", "P Value", "Lower Bound", "Upper Bound")



# this bit just gets an ROC curve and the auc
plot.roc(x = mod2$y,
         predictor = mod2$fitted.values)
auc(response = mod2$y,
    predictor = mod2$fitted.values)

plot_calibration <- function(model, n_bins = 10, title = "Calibration Curve") {
  data <- data.frame(
    truth = model$y,
    pred = model$fitted.values
  ) |>
    mutate(bin = ntile(pred, n_bins)) |>
    group_by(bin) |>
    summarise(
      mean_pred = mean(pred),
      mean_obs = mean(truth),
      n = n(),
      .groups = "drop"
    )
  
  ggplot(data, aes(x = mean_pred, y = mean_obs)) +
    geom_point(size = 2) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
    labs(
      x = "Predicted probability",
      y = "Observed proportion",
      title = title
    ) +
    theme_minimal()
}

plot_calibration(mod2)






#work through to get the effects of each different form of ace
results <- list()




# this last bit of code assesses the effect of each of the ACE variables independantly 
ace_cols.updated <- ace_cols[!ace_cols %in% c("ACETTHEM", "ACEHVSEX", "ACETOUCH")]
ace_cols.updated <- c(ace_cols.updated, "ACESEXCOMBINED")


for (v in ace_cols.updated) {
  form <- as.formula(paste0("hpv_ever_had ~ ", v, 
                            " + state + urban + metropolitan + i_year*sex + edu + income + race"))
  
  mod2 <- glm(data = df1.mutatedvars, formula = form, family = binomial(link = "logit"), weights = weight_survey)
  
  results[[v]] <- list(
    tidy(mod2, conf.int = TRUE, exponentiate = TRUE)[2,]
  )
}


results
table(analysis_df.mod2$age)

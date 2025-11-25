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

## i had to change this part of the code to make sure merge is in the right direction
# and merged set doesn't break if a variable is missing from one yr. only selecting 
# and renaming columns that exist
df1 <- df1 |>
  dplyr::select(any_of(cols_to_get)) |>
  dplyr::rename_with(
    ~ new_name_cols[match(.x, cols_to_get)],
    .cols = cols_to_get
  )


# remnoving anyone over the age of 54:
df1 <- df1|> filter(
  age < 8
)




# this bit is renaming the data within the file to 1. remove any number codes that actually mean NA, and also to change arouind the ACE columns so that they all mean the same thing
df1.mutatedvars <- df1 |>
  mutate(
    across(all_of(ace_cols), ~ ifelse(.x %in% c(7, 8, 9), NA, .x)),
    across(c(insurance, hpv_ever_had, race, marital), ~ ifelse(.x %in% c(7, 8, 9), NA, .x)),
    across(c(hpv_numberofshots), ~ ifelse(.x %in% c(77, 99), NA, .x)),
    across(c(age), ~ ifelse(.x %in% c(14), NA, .x)),
    across(c(income, edu), ~ ifelse(.x %in% c(9), NA, .x)),
    across(c(hpv_numberofshots), ~ ifelse(.x %in% c(4, 10, 77, 99), NA, .x)),
    
    ACESEXCOMBINED = ifelse(
      rowSums(across(c(ACETOUCH, ACETTHEM, ACEHVSEX)) > 1, na.rm = TRUE) > 0, "Yes",
      ifelse(
        rowSums(across(c(ACETOUCH, ACETTHEM, ACEHVSEX)) == 1, na.rm = TRUE) == 3, "No",
        NA
      )
    ),
    
    across(c(ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, ACEDIVRC, insurance),
           ~ ifelse(.x == 1, "Yes", ifelse(.x == 2, "No", .x))),
    
    across(c(hpv_ever_had), ~ ifelse(.x == 1, 1, ifelse((.x %in% c(NA, 3)), NA, 0))),
    
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


#gets covariates as discrete variables if this is appropriate
df1.mutatedvars <- df1.mutatedvars |>
  mutate(
    across(c(race, state, marital, insurance, age, income, edu), ~ as.character(.x)),
    ACEANY = factor(ACEANY, levels = c("No", "Yes"))
  )


## survey design set up to run regression that accounts for complex weighting
library(survey)
library(srvyr)
## adding this here bc creates error later
# what it does is tell survey how to handle lonely PSUs (we must set before creating design)
options(survey.lonely.psu = "adjust")

# to keep rows with non-missing design variables
df_svy <- df1.mutatedvars |>
  filter(!is.na(weight_survey), !is.na(psu), !is.na(strata))

design_all <- df_svy |>
  as_survey_design(
    ids = psu,
    strata = strata,
    weights = weight_survey,
    nest = TRUE
  )

#  regression without confounders (unadjusted but using complex survey-weights)
mod1 <- svyglm(hpv_ever_had ~ ACEANY,
               design = design_all,
               family = quasibinomial())
summary(mod1)


# #the below code is because summary(mod1) is super slow on my laptop for some reason. If summary(mod1 works for you, then pls ignore it)
# out <- cbind(
#   OR = exp(coef(mod1)),
#   exp(confint(mod1))
# )
# colnames(out) <- c("OR", "CI_lower", "CI_upper")
# out

# these tables show that there were only a handful of states that recorded information on ACEs and whether someone had had HPV vaccinaiton
states_available<- intersect(names(which(colSums(table(df1.mutatedvars$hpv_ever_had, df1.mutatedvars$state)) > 0)),
                           names(which(colSums(table(df1.mutatedvars$ACEANY, df1.mutatedvars$state)) > 0)))



# analysis to see systematic differences between those who did and did not reply yes to ACE questions
acena_subset <- df1.mutatedvars |>
  filter(state %in% states_available) |> 
  mutate(ACEANY_missing = if_else(is.na(ACEANY), "ACEANY is NA", "ACEANY not NA"))
  

library(tableone)
continuous_vars <- c("hpv_ever_had", "sex", "hpv_numberofshots")
string_vars <- c("race", "state", "marital", "insurance", "age", "income", "edu")
vars <- c(continuous_vars, string_vars)
group <- "ACEANY_missing"
tab1 <- CreateTableOne(
  vars = vars,
  strata = group,
  data = acena_subset,
  factorVars = string_vars,
  includeNA = TRUE
)
tab1




# makes a dataframe using only the rows that have complete data for all the points. this helps for making the ROC later
analysis_df.mod2 <- df1.mutatedvars[complete.cases(df1.mutatedvars[, c(
  "hpv_ever_had", "ACEANY", "state", "urban", "metropolitan", "insurance",
  "i_year", "sex", "edu", "income", "race"
)]),
]

# #looking at the variables
# table(analysis_df.mod2$edu)
# table(analysis_df.mod2$hpv_ever_had)
# table(analysis_df.mod2$ACEANY)
# table(analysis_df.mod2$state)
# table(analysis_df.mod2$urban)
# table(analysis_df.mod2$metropolitan)
# table(analysis_df.mod2$i_year)
# table(analysis_df.mod2$sex)
# table(analysis_df.mod2$income)
# table(analysis_df.mod2$insurance)
# table(analysis_df.mod2$race)
# table(is.na(analysis_df.mod2$weight_survey), analysis_df.mod2$i_year)


# there's only 6 people who never attended school. This is going to result in silly standard errors using this as the reference to compare to. 
# I've reclassified into 1. didn't complete high school education, high school education, college education, and graduate college education
analysis_df.mod2<- analysis_df.mod2 |>
  mutate(
    edu = case_when(
      edu %in% c("1", "2", "3") ~ "less than high school",
      edu == 4 ~ "high school",
      edu == 5 ~ "college",
      edu == 6 ~ "graduate",
      TRUE ~ NA_character_
    )
  )


#relevel so that income of 4 is set as the reference
analysis_df.mod2$income <- relevel(factor(analysis_df.mod2$income), ref = "4")


# change - make a survey design just for the complete-case dataset used in mod2
design_mod2 <- analysis_df.mod2 |>
  filter(!is.na(weight_survey), !is.na(psu), !is.na(strata)) |>
  as_survey_design(
    ids = psu,
    strata = strata,
    weights = weight_survey,
    nest = TRUE,
    lonely.psu = "adjust" 
  )

#model using all the covariates (use syrvey weighted glm)
mod2 <- svyglm(hpv_ever_had ~ ACEANY + state + urban + metropolitan + insurance + i_year *sex + edu+ income+race, 
      design = design_mod2, family=quasibinomial()
)
summary(mod2)
library(car)
mod2results <- tidy(mod2, exponentiate = TRUE, conf.int = TRUE)
colnames(mod2results)<- c("Term","OR", "SE", "Test Statistic", "P Value", "Lower Bound", "Upper Bound")


# this bit just gets an ROC curve and the auc
plot.roc(x = mod2$y,
         predictor = mod2$fitted.values)
auc(response = mod2$y,
    predictor = mod2$fitted.values)

plot_calibration <- function(model) {
  data <- data.frame(
    truth = model$y,
    pred = model$fitted.values
  ) 
  data<- data|>
    mutate(bin = ntile(pred, 8)) |>
    group_by(bin) |>
    summarise(
      mean_pred = mean(pred),
      mean_obs = mean(truth),
      n = n(),
      .groups = "drop"
    )
  
  ggplot(data, aes(x = mean_pred, y = mean_obs)) +
    geom_point() +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
    labs(
      x = "Predicted probability",
      y = "Observed proportion",
    ) +
    theme_minimal()
}

plot_calibration(mod2)


#work through to get the effects of each different form of ace (recoded)

library(purrr)
library(broom)

ace_cols.updated <- c(
  setdiff(ace_cols, c("ACETTHEM", "ACEHVSEX", "ACETOUCH")),
  "ACESEXCOMBINED"
)




ace_results_df <- map_dfr(
  ace_cols.updated,
  ~ {
    form <- as.formula(
      paste0("hpv_ever_had ~ ", .x,
             " + state + urban + metropolitan + insurance + i_year*sex + edu + income + race")
    )
    
    analysis_df.mod3 <- df1.mutatedvars[complete.cases(df1.mutatedvars[, c(
      "hpv_ever_had", .x, "state", "urban", "metropolitan", "insurance",
      "i_year", "sex", "edu", "income", "race"
    )]),
    ]
    
    design_mod3 <- analysis_df.mod3 |>
      filter(!is.na(weight_survey), !is.na(psu), !is.na(strata)) |>
      as_survey_design(
        ids = psu,
        strata = strata,
        weights = weight_survey,
        nest = TRUE,
        lonely.psu = "adjust" 
      )
    
    tidy(svyglm(form, design = design_mod3, family = quasibinomial()),
         exponentiate = TRUE, conf.int = TRUE)[2, ]
  },
  .id = "ACE_variable"
)

ace_results_df


# repeat results but only for those below 35
df1.mutatedvars.sensitivity_analysis <- df1.mutatedvars |> filter(age<4)
ace_results_df_sensitivity_analysis_age <- map_dfr(
  ace_cols.updated,
  ~ {
    form <- as.formula(
      paste0("hpv_ever_had ~ ", .x,
             " + state + urban + metropolitan + insurance + i_year*sex + edu + income + race")
    )
    
    analysis_df.mod3 <- df1.mutatedvars.sensitivity_analysis[complete.cases(df1.mutatedvars.sensitivity_analysis[, c(
      "hpv_ever_had", .x, "state", "urban", "metropolitan", "insurance",
      "i_year", "sex", "edu", "income", "race"
    )]),
    ]
    
    design_mod3 <- analysis_df.mod3 |>
      filter(!is.na(weight_survey), !is.na(psu), !is.na(strata)) |>
      as_survey_design(
        ids = psu,
        strata = strata,
        weights = weight_survey,
        nest = TRUE,
        lonely.psu = "adjust" 
      )
    
    tidy(svyglm(form, design = design_mod3, family = quasibinomial()),
         exponentiate = TRUE, conf.int = TRUE)[2, ]
  },
  .id = "ACE_variable"
)
ace_results_df_sensitivity_analysis_age

library(haven)
library(tidyverse)
library(broom)
library(pROC)
library(probably)

df2024 <- read_xpt("/Users/ianholdroyd/Downloads/LLCP2024.xpt")
df2023 <- read_xpt("/Users/ianholdroyd/Downloads/LLCP2023.xpt")
df2022 <- read_xpt("/Users/ianholdroyd/Downloads/LLCP2022.xpt")

df <- bind_rows(df2023, df2022)
df1 <- bind_rows(df, df2024)
df2_backupdf1 <- df1
df1<-df2_backupdf1

ace_cols <- grep("^ACE", names(df1), value = TRUE)


# chosen the more combined for income and race
# do we need to categorise the types of insurance or just keep them all as seperate

cols_to_get <-c("_STATE", "IMONTH", "IYEAR", "DISPCODE", "SEXVAR", "EDUCA", "MARITAL",
                "_METSTAT", "_URBSTAT", "_RACEGR3", "_LLCPWT", "_AGEG5YR","_INCOMG1", 
                "PRIMINS1", "HPVADVC4", "HPVADSHT", 
                ace_cols)

new_name_cols <- c("state", "i_month", "i_year", "id", "sex", "edu", "marital",
                   "metropolitan", "urban", "race", "weight_survey", "age","income",
                   "insurance", "hpv_ever_had", "hpv_numberofshots",
                   ace_cols)

df1<- df1[cols_to_get]
df1<- df1|>
  rename(setNames(cols_to_get, new_name_cols))



df1.mutatedvars <- df1 |>
  mutate(
    across(c(ace_cols, hpv_ever_had, race, marital), ~ ifelse(.x %in% c(7, 8, 9), NA, .x)),
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
    across(c(ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, ACEDIVRC),
           ~ ifelse(.x == 1, "Yes", ifelse(.x == 2, "No", .x))),
    across(c(hpv_ever_had), ~ifelse(.x == 1, 1, ifelse(is.na(.x), NA, 0))),
    across(c(ACEPUNCH, ACEHURT1, ACESWEAR, ACETOUCH, ACETTHEM, ACEHVSEX),
           ~ ifelse(.x > 1, "Yes", ifelse(.x == 1, "No", .x))),
    across(c(ACEADNED, ACEADSAF),
           ~ ifelse(.x > 3, "No", ifelse(.x < 4, "Yes", .x))),
    ACEANY = case_when(
      if_any(all_of(ace_cols), ~ .x == "Yes") ~ "Yes",
      if_all(all_of(ace_cols), ~ .x == "No") ~ "No",
      if_any(all_of(ace_cols), is.na) ~ NA_character_
    )
  )


df1.mutatedvars<- df1.mutatedvars |> mutate(
  across(c(race, state, marital, insurance, age, income, edu), ~ as.character(.x)),
)


# df11 <- df12 |>filter(if_all(all_of(ace_cols), ~ !is.na(.x)))

mod1 <- glm(data = df1.mutatedvars, formula(hpv_ever_had ~ ACEANY ), family = binomial(link="logit"))
summary(mod1)
exp(mod1$coefficients)


summary(df1.mutatedvars)

# these tables show that there were only a handful of states that recorded information on ACEs and whether someone had had HPV vaccinaiton
table(df1.mutatedvars$hpv_ever_had, df1.mutatedvars$state)
table(df1.mutatedvars$ACEANY, df1.mutatedvars$state)


analysis_df.mod2 <- df1.mutatedvars[complete.cases(df1.mutatedvars[, c(
  "hpv_ever_had", "ACEANY", "state", "urban", "metropolitan",
  "i_year", "sex", "edu", "income", "race"
)]),
]

analysis_df.mod2$edu

summary(analysis_df.mod2$edu)
mod2 <- glm(data = analysis_df.mod2, formula(hpv_ever_had ~ ACEANY + state + urban + metropolitan + i_year*sex + edu+ income+race), family = binomial(link="logit"))
summary(mod2)
exp(mod2$coefficients)


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


results <- list()


ace_cols.updated <- ace_cols[!ace_cols %in% c("ACETTHEM", "ACEHVSEX", "ACETOUCH")]
ace_cols.updated <- c(ace_cols.updated, "ACESEXCOMBINED")


for (v in ace_cols.updated) {
  form <- as.formula(paste0("hpv_ever_had ~ ", v, 
                            " + state + urban + metropolitan + i_year*sex + edu + income + race"))
  
  mod2 <- glm(data = df1.mutatedvars, formula = form, family = binomial(link = "logit"))
  
  results[[v]] <- list(
    tidy(mod2, conf.int = TRUE, exponentiate = TRUE)[2,]
  )
}


results


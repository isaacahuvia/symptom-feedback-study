## Reliability coding

## Load packages
library(openxlsx)
library(irrCAC)
library(tidyverse)
library(scales)
`%+%` <- paste0

## Reliability functions
fill_table <- function(table) {

  if(ncol(table) == 1) {

    table <- cbind(table, c(0, 0))

  }

  if(nrow(table) == 1) {

    table <- rbind(table, c(0, 0))

  }

  return(table)

}

calculate_reliability <- function(df1, df2, codes) {

  if(
    !all(names(df1) == names(df2)) |
    !all(dim(df1) == dim(df2))
  ) stop("Datasets must have equal dimensions, column names")

  if(!all(codes %in% names(df1))) stop("All codes must appear in datasets")

  out <- tibble(
    code = codes,
    prevalence = NA,
    agreement = NA,
    ac1 = NA,
    kappa = NA
  )

  for(i in 1:nrow(out)) {

    x <- out$code[i]

    if(
      !is.numeric(df1[[x]]) |
      !is.numeric(df2[[x]])
    ) stop("Variable " %+% x %+% " must be numeric")

    ratings <- bind_cols(
      "x1" = df1[[x]],
      "x2" = df2[[x]]
    ) %>%
      replace_na(
        list(
          x1 = 0,
          x2 = 0
        )
      )

    out$prevalence[i] <- ratings %>%
      rowMeans() %>%
      mean()

    out$agreement[i] <- ratings %>%
      pa.coeff.raw() %>%
      pluck("est") %>%
      pluck("pa")

    out$ac1[i] <- ratings %>%
      gwet.ac1.raw() %>%
      pluck("est") %>%
      pluck("coeff.val")

    out$kappa[i] <- ratings %>%
      table() %>%
      fill_table() %>%
      kappa2.table() %>%
      pluck("coeff.val")

  }

  return(out)

}


## Load data
data_directory <- "H:\\My Drive\\Research\\Projects\\Depression Symptom Feedback\\Content Analysis\\Qualitative Responses\\"

# Round 1
isaac_r1 <- read.xlsx(
  data_directory %+% "250305 Responses (250313 Codebook) - Isaac.xlsx",
  rows = c(1, 102:151)
) %>%
  select(-notes)

kyle_r1 <- read.xlsx(
  data_directory %+% "250305 Responses (250313 Codebook) - Kyle.xlsx",
  rows = c(1, 102:151)
)

# Round 2
isaac_r2 <- read.xlsx(
  data_directory %+% "250305 Responses (250321 Codebook) - Isaac.xlsx",
  rows = c(1, 152:201)
) %>%
  select(-notes)

kyle_r2 <- read.xlsx(
  data_directory %+% "250305 Responses (250321 Codebook) - Kyle.xlsx",
  rows = c(1, 152:201)
) %>%
  select(-X16)


## Calculate reliability
# Round 1
r1_codes <- c("concerned", "sad", "unsure", "relieved", "motivated", "validated", "indifferent", "misc_good", "misc_bad", "expected", "did_not_expect", "valid", "not_valid")

r1_reliability <- calculate_reliability(isaac_r1, kyle_r1, r1_codes)

r1_reliability

write.xlsx(
  r1_reliability,
  data_directory %+% "Round 1 Reliability Results.xlsx",
)

# Round 2
r2_codes <- c("concerned", "sad", "unsure", "relieved", "motivated", "validated", "indifferent", "surprised", "expected", "skeptical", "curious", "misc_good", "misc_bad")

r2_reliability <- calculate_reliability(isaac_r2, kyle_r2, r2_codes)

r2_reliability

write.xlsx(
  r2_reliability,
  data_directory %+% "Round 2 Reliability Results.xlsx",
)

# Combined
isaac_combined <- isaac_r1 %>%
  rename(
    surprised = did_not_expect,
    skeptical = not_valid
  ) %>%
  bind_rows(isaac_r2)

kyle_combined <- kyle_r1 %>%
  rename(
    surprised = did_not_expect,
    skeptical = not_valid
  ) %>%
  bind_rows(kyle_r2)

combined_reliability <- calculate_reliability(isaac_combined, kyle_combined, r2_codes)

combined_reliability

write.xlsx(
  combined_reliability,
  data_directory %+% "Combined Reliability Results.xlsx",
)

# Screen the screener data

# load packages
library(qualtRics)
library(tidyverse)
`%+%` <- paste0

# load data
data_directory <- "H:\\My Drive\\Research\\Projects\\Depression Symptom Feedback\\Data\\"
data_filename <- "Symptom Feedback Study Screener - Response Data.csv"

screener_data_raw <- read_survey(data_directory %+% data_filename)

# clean data
clean_data <- function(.data) {

  .data %>%
    mutate(
      across(
        all_of("phq_8_t1_" %+% 1:8),
        ~ case_match(
          .,
          "Not at all" ~ 0,
          "Several days" ~ 1,
          "More than half the days" ~ 2,
          "Nearly every day" ~ 3
        )
      ),
      phq_8_t1_sum = phq_8_t1_1 + phq_8_t1_2 + phq_8_t1_3 + phq_8_t1_4 + phq_8_t1_5 + phq_8_t1_6 + phq_8_t1_7 + phq_8_t1_8
    ) %>%
    select(
      participant_id = participantId,
      ip_address = IPAddress,
      finished = Finished,
      date = StartDate,
      duration = `Duration (in seconds)`,
      ends_with("t0"),
      starts_with("phq")
    )

}

screener_data_clean <- clean_data(screener_data_raw)

# review responses
get_duplicated_ids <- function(.data) {

  .data %>%
    count(participant_id) %>%
    filter(n > 1) %>%
    pull(participant_id)

}

get_duplicated_ips <- function(.data) {

  .data %>%
    count(ip_address) %>%
    filter(n > 1) %>%
    pull(ip_address)

}

review_responses <- function(.data) {

  # Get counts
  for(x in c("consent_t0", "age_t0", "self_label_pres_t0", "self_label_past_t0", "diagnosis_t0", "treatment_pres_t0", "treatment_past_t0", "treatment_seeking_t0", "phq_8_t1_sum")) {

    .data %>%
      count(get(x)) %>%
      rename(!!x := `get(x)`) %>%
      print()

  }

  # Check duplicates
  print("Duplicate participant IDs:")
  .data %>%
    filter(participant_id %in% get_duplicated_ids(.)) %>%
    select(participant_id, ip_address, finished, duration, date) %>%
    arrange(participant_id, date) %>%
    print()

  print("Duplicate IP addresses:")
  .data %>%
    filter(ip_address %in% get_duplicated_ips(.)) %>%
    select(participant_id, ip_address, finished, duration, date) %>%
    arrange(ip_address, date) %>%
    print()

}

review_responses(screener_data_clean)

# get eligible responders
get_eligible <- function(.data) {

  .data  %>%
    filter(
      !participant_id %in% get_duplicated_ids(.),
      !ip_address %in% get_duplicated_ips(.),
      consent_t0 == "I agree",
      age_t0 %in% 18:29,
      phq_8_t1_sum >= 5,
      self_label_pres_t0 %in% c("No", "I'm not sure"),
      diagnosis_t0 %in% c("No", "I'm not sure"),
      treatment_pres_t0 == "No",
      treatment_past_t0 %in% c("No", "I'm not sure"),
      treatment_seeking_t0 == "No"
    )

}

screener_data_eligible <- get_eligible(screener_data_clean)

# n/eligible
nrow(screener_data_eligible)

# eligibility rate
nrow(screener_data_eligible) / nrow(screener_data_clean)

# double check
review_responses(screener_data_eligible)

# export csv of IDs
screener_data_eligible %>%
  select(Participant = participant_id) %>%
  write.csv(data_directory %+% "Whitelisted Participants.csv", row.names = F)

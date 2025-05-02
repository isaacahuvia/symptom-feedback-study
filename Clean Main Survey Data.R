####  Startup  ####
# Load packages
library(qualtRics)
library(openxlsx2)
library(tidyverse)
library(scales)
library(openxlsx2)
`%+%` <- paste0

# Load data
data_directory <- "H:\\My Drive\\Research\\Projects\\Depression Symptom Feedback\\Data\\"
response_data_filename <- "Symptom Feedback Study - Response Data.csv"
user_data_filename <- "Symptom Feedback Study - CloudResearch User Data.csv"

response_data_raw <- read_survey(data_directory %+% response_data_filename)
user_data_raw <- read.csv(data_directory %+% user_data_filename)


####  Clean Data  ####
## Clean columns
# Set functions
recode_belief <- function(x, reversed = F) {

  x_scored <- x %>%
    case_match(
      "Strongly Disagree" ~ 1,
      "Disagree" ~ 2,
      "Neither Agree nor Disagree" ~ 3,
      "Agree" ~ 4,
      "Strongly Agree" ~ 5
    )

  if(reversed) {

    x_corrected = 6 - x_scored
    return(x_corrected)

  } else {

    return(x_scored)

  }

}

# Clean columns, using functions
data_clean_columns <- response_data_raw %>%

  # Recode items
  mutate(

    # Identifiers and metadata
    participant_id = participantId, # Participant ID, from CloudResearch
    ip_address = IPAddress, # IP address, from Qualtrics
    completed_survey = Finished, # Survey completion
    response_datetime = RecordedDate, # Datetime of response
    survey_duration = `Duration (in seconds)`,
    consent, # Consent
    # Group. Note: Qualtrics did not collect data on who was assigned to what group,
    # so I'm basing this on who responded to in-SSI items. Because non-response rates
    # to survey items are almost unanimously 0%, it is very unlikely that an SSI
    # recipient would not be captured with this method
    group = !is.na(review_permission) | !is.na(review_talk_back) | !is.na(review_advice) | !is.na(plan_behavior) | !is.na(plan_thoughts),
    group_char = if_else(group, "Psychoeducation", "Feedback Only"),

    # Demographics
    age_t1, # Age
    gender_t1 = case_when(
      gender_t1 == "Man/boy" ~ "Man",
      gender_t1 == "Woman/girl" ~ "Woman",
      !is.na(gender_t1) ~ "TGD",
      is.na(gender_t1) ~ NA_character_
    ), # Gender
    sexuality_t1 = case_when(
      sexuality_t1 == "Straight/heterosexual" ~ "Straight",
      !is.na(sexuality_t1) ~ "LGBQ+",
      is.na(sexuality_t1) ~ NA_character_
    ), # Sexual orientation
    race_eth_t1 = case_when(
      is.na(race_eth_t1) ~ NA_character_,
      grepl(",", race_eth_t1) ~ "Multiple",
      !grepl(",", race_eth_t1) ~ race_eth_t1
    ), # Race/ethnicity

    # Depression experience and self-labeling
    self_label_ever_t1 = factor(
      self_label_past_t1,
      levels = c("No", "I'm not sure", "Yes")
    ), # Lifetime self-labeling
    self_label_ever_t1_bin = self_label_ever_t1 == "Yes", # Binary version for some analyses
    self_label_t1 = factor(
      self_label_pres_t1,
      levels = c("No", "I'm not sure")
    ), # Self-labeling at t1
    self_label_t1_bin = self_label_t1 == "Yes", # Binary version for some analyses
    self_label_t2 = factor(
      self_label_t2,
      levels = c("No", "I'm not sure", "Yes")
    ), # Self-labeling at t2
    self_label_t2_bin = self_label_t2 == "Yes", # Binary version for some analyses
    self_label_inc = self_label_t2 == "Yes" | (self_label_t1 == "No" & self_label_t2 == "I'm not sure"), # Any increase
    diagnosis_ever_t1 = diagnosis_t1, # Lifetime diagnosis
    treatment_now_t1 = treatment_pres_t1, # Current treatment for depression
    treatment_ever_t1 = treatment_past_t1, # Lifetime treatment for depression
    treatment_seeking_t1, # Current treatment seeking for depression
    curious_t1, # Curiosity re: depression test results

    # Beliefs about depression
    dep_pc_t1_1 = recode_belief(dep_beliefs_t1_1), # Personal control item 1 ("power to influence")
    dep_pc_t1_2 = recode_belief(dep_beliefs_t1_2), # Personal control item 2 ("course... depends")
    dep_pc_t1_3 = recode_belief(dep_beliefs_t1_3), # Personal control item 3 ("to reduce their symptoms")
    dep_pc_t1_4 = recode_belief(dep_beliefs_t1_4), # Personal control item 4 ("can determine... better")
    dep_tc_t1_1 = recode_belief(dep_beliefs_t1_5), # Therapy control item 1 ("can reduce")
    dep_tc_t1_2 = recode_belief(dep_beliefs_t1_6), # Therapy control item 2 ("is effective")
    dep_tc_t1_3 = recode_belief(dep_beliefs_t1_7), # Therapy control item 3 ("negative effects")
    dep_mc_t1_1 = recode_belief(dep_beliefs_t1_8), # Medication control item 1 ("can reduce")
    dep_mc_t1_2 = recode_belief(dep_beliefs_t1_9), # Medication control item 2 ("is effective")
    dep_mc_t1_3 = recode_belief(dep_beliefs_t1_10), # Medication control item 3 ("negative effects")
    dep_pp_t1_1 = recode_belief(dep_beliefs_t1_11), # Prognostic pessimism item 1 ("long time")
    dep_pp_t1_2_r = recode_belief(dep_beliefs_t1_12, T), # Prognostic pessimism item 2 ("short time")
    dep_pp_t1_3 = recode_belief(dep_beliefs_t1_13), # Prognostic pessimism item 3 ("lifelong condition")
    dep_bl_t1_1 = recode_belief(dep_beliefs_t1_14), # Blame item 1 ("own fault")
    dep_bl_t1_2 = recode_belief(dep_beliefs_t1_15), # Blame item 2 ("are to blame")
    dep_bl_t1_3_r = recode_belief(dep_beliefs_t1_16, T), # Blame item 3 ("shouldn't be blamed")

    # Depression symptom severity
    phq_8_sum, # PHQ-8 sum score (calculated in Qualtrics)
    across(
      all_of("phq_8_t1_" %+% 1:8),
      ~ case_match(
        .,
        "Not at all" ~ 0,
        "Several days" ~ 1,
        "More than half the days" ~ 2,
        "Nearly every day" ~ 3
      )
    ), # PHQ-8 items
    test_result, # Test result (PHQ-8 categories)
    test_result_fct = case_when(
      test_result %in% c("Moderately Severe Depression", "Severe Depression") ~ "Moderately Severe or Severe Depression",
      T ~ test_result
    ) %>%
      factor(
        levels = c("Mild Depression", "Moderate Depression", "Moderately Severe or Severe Depression")
      ), # Test result (setting reference level and removing sparse levels)
    phq_8_duration_t1 = factor(
      phq_8_duration_t1,
      levels = c("One month or less", "More than a month but less than a year", "A year or longer")
    ), # Symptom duration

    # Beliefs about one's problems, t1
    prob_pc_t1_1 = recode_belief(problem_beliefs_t1_1), # Personal control item 1 ("a lot I can do")
    prob_pc_t1_2 = recode_belief(problem_beliefs_t1_2), # Personal control item 2 ("can determine")
    prob_pc_t1_3_r = recode_belief(problem_beliefs_t1_3, T), # Personal control item 3 ("nothing I do")
    prob_pc_t1_4 = recode_belief(problem_beliefs_t1_4), # Personal control item 4 ("I have the power")
    prob_tc_t1_1 = recode_belief(problem_beliefs_t1_5), # Therapy control item 1 ("can be effective")
    prob_tc_t1_2 = recode_belief(problem_beliefs_t1_6), # Therapy control item 2 ("can control")
    prob_tc_t1_3_r = recode_belief(problem_beliefs_t1_7, T), # Therapy control item 3 ("nothing therapy can do")
    prob_mc_t1_1 = recode_belief(problem_beliefs_t1_8), # Medication control item 1 ("can be effective")
    prob_mc_t1_2 = recode_belief(problem_beliefs_t1_9), # Medication control item 2 ("can control")
    prob_mc_t1_3_r = recode_belief(problem_beliefs_t1_10, T), # Medication control item 3 ("nothing medication can do")
    prob_pp_t1_1_r = recode_belief(problem_beliefs_t1_11, T), # Prognostic pessimism item 1 ("a short time")
    prob_pp_t1_2 = recode_belief(problem_beliefs_t1_12), # Prognostic pessimism item 2 ("permanent rather than temporary")
    prob_pp_t1_3 = recode_belief(problem_beliefs_t1_13), # Prognostic pessimism item 3 ("a long time")
    prob_pp_t1_4_r = recode_belief(problem_beliefs_t1_14, T), # Prognostic pessimism item 4 ("pass quickly")
    prob_pp_t1_5 = recode_belief(problem_beliefs_t1_15), # Prognostic pessimism item 5 ("rest of my life")
    prob_pp_t1_6_r = recode_belief(problem_beliefs_t1_16, T), # Prognostic pessimism item 6 ("improve in time")
    prob_bl_t1_1 = recode_belief(problem_beliefs_t1_17), # Blame item 1 ("my fault")
    prob_bl_t1_2_r = recode_belief(problem_beliefs_t1_18, T), # Blame item 2 ("not my fault")
    prob_bl_t1_3 = recode_belief(problem_beliefs_t1_19), # Blame item 3 ("I blame myself")

    # Beliefs about one's problems, t2
    prob_pc_t2_1 = recode_belief(beliefs_t2_1), # Personal control item 1 ("a lot I can do")
    prob_pc_t2_2 = recode_belief(beliefs_t2_2), # Personal control item 2 ("can determine")
    prob_pc_t2_3_r = recode_belief(beliefs_t2_3, T), # Personal control item 3 ("nothing I do")
    prob_pc_t2_4 = recode_belief(beliefs_t2_4), # Personal control item 4 ("I have the power")
    prob_tc_t2_1 = recode_belief(beliefs_t2_5), # Therapy control item 1 ("can be effective")
    prob_tc_t2_2 = recode_belief(beliefs_t2_6), # Therapy control item 2 ("can control")
    prob_tc_t2_3_r = recode_belief(beliefs_t2_7, T), # Therapy control item 3 ("nothing therapy can do")
    prob_mc_t2_1 = recode_belief(beliefs_t2_8), # Medication control item 1 ("can be effective")
    prob_mc_t2_2 = recode_belief(beliefs_t2_9), # Medication control item 2 ("can control")
    prob_mc_t2_3_r = recode_belief(beliefs_t2_10, T), # Medication control item 3 ("nothing medication can do")
    prob_pp_t2_1_r = recode_belief(beliefs_t2_11, T), # Prognostic pessimism item 1 ("a short time")
    prob_pp_t2_2 = recode_belief(beliefs_t2_12), # Prognostic pessimism item 2 ("permanent rather than temporary")
    prob_pp_t2_3 = recode_belief(beliefs_t2_13), # Prognostic pessimism item 3 ("a long time")
    prob_pp_t2_4_r = recode_belief(beliefs_t2_14, T), # Prognostic pessimism item 4 ("pass quickly")
    prob_pp_t2_5 = recode_belief(beliefs_t2_15), # Prognostic pessimism item 5 ("rest of my life")
    prob_pp_t2_6_r = recode_belief(beliefs_t2_16, T), # Prognostic pessimism item 6 ("improve in time")
    prob_bl_t2_1 = recode_belief(beliefs_t2_17), # Blame item 1 ("my fault")
    prob_bl_t2_2_r = recode_belief(beliefs_t2_18, T), # Blame item 2 ("not my fault")
    prob_bl_t2_3 = recode_belief(beliefs_t2_19), # Blame item 3 ("I blame myself")

    # Exploratory outcomes
    opt_in_t2, # Opt-in to receive more treatment resources (i.e., help-seeking behavior)
    feedback_impact_t1.5, # Impact of symptom feedback
    why_not_no_t2, # For those who said "no" to self_label_t2, why?
    why_not_unsure_t2, # For those who said "unsure" to self_label_t2, why?

    # SSI usage
    review_talk_back, review_advice, review_permission, plan_behavior, plan_thoughts,

    .keep = "none"

  ) %>%

  # Compute scales
  rowwise() %>%
  mutate(

    dep_pc_t1 = mean(c_across(starts_with("dep_pc_t1"))),
    prob_pc_t1 = mean(c_across(starts_with("prob_pc_t1"))),
    prob_pc_t2 = mean(c_across(starts_with("prob_pc_t2"))),
    dep_tc_t1 = mean(c_across(starts_with("dep_tc_t1"))),
    prob_tc_t1 = mean(c_across(starts_with("prob_tc_t1"))),
    prob_tc_t2 = mean(c_across(starts_with("prob_tc_t2"))),
    dep_mc_t1 = mean(c_across(starts_with("dep_mc_t1"))),
    prob_mc_t1 = mean(c_across(starts_with("prob_mc_t1"))),
    prob_mc_t2 = mean(c_across(starts_with("prob_mc_t2"))),
    dep_pp_t1 = mean(c_across(starts_with("dep_pp_t1"))),
    prob_pp_t1 = mean(c_across(starts_with("prob_pp_t1"))),
    prob_pp_t2 = mean(c_across(starts_with("prob_pp_t2"))),
    dep_bl_t1 = mean(c_across(starts_with("dep_bl_t1"))),
    prob_bl_t1 = mean(c_across(starts_with("prob_bl_t1"))),
    prob_bl_t2 = mean(c_across(starts_with("prob_bl_t2")))

  ) %>%
  ungroup()


## Clean rows
nrow(data_clean_columns)

# Remove participants who were not approved per CloudResearch
approved_users <- user_data_raw %>%
  filter(Status == "Approved") %>%
  select(participant_id = ParticipantId)

data_approved <- data_clean_columns %>%
  semi_join(approved_users, by = "participant_id")

nrow(data_approved)

# Remove incomplete responses (and confirm this means everybody consented)
data_complete_responses <- data_approved %>%
  filter(completed_survey)

count(data_complete_responses, consent)

nrow(data_complete_responses)

# Remove duplicates
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

data_no_duplicates <- data_complete_responses %>%
  filter(
    !participant_id %in% get_duplicated_ids(.),
    !ip_address %in% get_duplicated_ips(.)
  )

nrow(data_no_duplicates)

# Remove ineligible responders
data_eligible_responders <- data_no_duplicates %>%
  filter(
    consent == "I agree",
    age_t1 %in% 18:29,
    phq_8_sum >= 5,
    self_label_t1 != "Yes",
    diagnosis_ever_t1 != "Yes",
    treatment_now_t1 != "Yes",
    treatment_ever_t1 != "Yes",
    treatment_seeking_t1 != "Yes"
  )

nrow(data_eligible_responders)

# Remove responders who took less than three minutes
data_enough_time <- data_eligible_responders %>%
  filter(survey_duration >= 180)

nrow(data_enough_time)

# Remove low-attention responders
reversed_items <- data_enough_time %>%
  select(ends_with("_r")) %>%
  colnames()

reversed_item_differences <- data_enough_time %>%
  pivot_longer(
    cols = c(
      starts_with("prob_"),
      starts_with("dep_")
    )
  ) %>%
  select(participant_id, name, value) %>%
  mutate(
    scale = str_extract(name, "^([^_]+_[^_]+_[^_]+)"),
    item_type = if_else(
      name %in% reversed_items,
      "reversed",
      "normal"
    )
  ) %>%
  group_by(participant_id, scale, item_type) %>%
  summarize(mean = mean(value, na.rm = T), .groups = "drop") %>%
  pivot_wider(
    id_cols = c("participant_id", "scale"),
    names_from = "item_type",
    values_from = "mean"
  ) %>%
  mutate(scale_reversed_item_difference = abs(normal - reversed)) %>%
  group_by(participant_id) %>%
  summarize(average_reversed_item_difference = mean(scale_reversed_item_difference, na.rm = T))

hist(reversed_item_differences$average_reversed_item_difference)

low_attention_responders <- reversed_item_differences %>%
  filter(average_reversed_item_difference > 1.5) %>%
  pull(participant_id)

length(low_attention_responders)

data_high_attention <- data_enough_time %>%
  filter(!participant_id %in% low_attention_responders)

nrow(data_high_attention)


## Check missingness
count_missing <- function(x) {

  n_missing <- sum(is.na(x))
  pct_missing <- percent(mean(is.na(x)), .1)

  if(n_missing > 0) {

    return(pct_missing %+% " (n = " %+% n_missing %+% ")")

  } else {

    return(pct_missing)

  }

}

data_high_attention %>%
  group_by(group) %>%
  summarize(
    across(
      everything(),
      count_missing
    )
  ) %>%
  pivot_longer(-group) %>%
  pivot_wider(
    id_cols = name,
    names_from = "group",
    values_from = "value"
  ) %>%
  print(n = nrow(.))


## Add qualitative data
qualitative_data <- list.files(
  path = "H:\\My Drive\\Research\\Projects\\Depression Symptom Feedback\\Content Analysis\\Qualitative Responses\\Coded Responses\\",
  pattern = "^Response Chunk.*.xlsx$",
  full.names = T
) %>%
  map(read_xlsx) %>%
  bind_rows() %>%
  drop_na(participant_id) %>%
  select(-feedback_impact_t1.5) %>% # Dropping because this is the same as what's in the main data, plus some weird symbols from Excel
  rename_with(
    .cols = concerned:notes,
    .fn = ~ "feedback_impact_" %+% .
  )

data_combined <- data_high_attention %>%
  left_join(
    qualitative_data,
    by = c("participant_id"),
    relationship = "one-to-one"
  )

# Manually remove GPT responses
nrow(data_combined)

analysis_ready_data <- data_combined %>%
  filter(!grepl("AI|GPT", feedback_impact_notes))

nrow(analysis_ready_data)
# There were three in the qualitative data, but these were all caught by previous checks!



####  Save Data  ####
# Save analysis-ready data
saveRDS(analysis_ready_data, data_directory %+% "Clean Main Survey Data.rds")

# Save anonymous data
analysis_ready_data %>%
  select(
    -c(
      ip_address,
      response_datetime,
      feedback_impact_t1.5,
      feedback_impact_notes,
      review_talk_back,
      review_advice,
      review_permission,
      plan_behavior,
      plan_thoughts,
      why_not_no_t2,
      why_not_unsure_t2
    )
  ) %>%
  write.csv(
    data_directory %+% "Clean Main Survey Data - Anonymized.csv",
    row.names = F
  )

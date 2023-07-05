library(lubridate)
library(qualtRics)
library(tidyverse)

#### Source this file to produce clean dataset and metadata for Study 2 (US)

#### Part 1 - Games ####

part1 <-
  qualtRics::read_survey("study2_part1_games/PUN_BATTERY_GAMES+-+US_June+27,+2023_10.37.csv",
                         time_zone = "UTC") %>%
  # remove testing rows and rows where Prolific IDs are missing
  filter(!is.na(PROLIFIC_PID) & !is.na(prolific_id)) %>%
  # remove rows with duplicated prolific IDs (keep more complete submission)
  filter(!(ResponseId %in% c("R_1hzh4TPukLMGgJc", "R_bJzUSWXdJJj8VTX", "R_BG0PQUuvS5VV1T3",
                             "R_2aQq2FpvJ7Z5NNq", "R_32PGvi2ktzIDEWM", "R_2SeipWu3jgLc0y6",
                             "R_300O7jzueLXV1lC", "R_1memDQb8WFYYljC", "R_XTVA5YYKncUxcDD",
                             "R_2Sw6awU7yj7VSlL", "R_28UO5K5U20rDSiE", "R_PI188DrMDAxaMnL"))) %>%
  # keep only participants that completed all the games
  filter(!is.na(Explain))


# load prolific demographics (part 1)
demographics <-
  read_csv("study2_part1_games/Demographic data_games.csv") %>%
  # introduce NAs for data expiry or revoked consent
  mutate_all(function(x) ifelse(x %in% c("DATA_EXPIRED", "CONSENT_REVOKED"), NA, x)) %>%
  # rename and keep only relevant columns
  transmute(
    PROLIFIC_PID = `Participant id`,
    ProlificApprovals = `Total approvals`,
    FluentLanguages = `Fluent languages`,
    Age = as.numeric(Age),
    Gender = Sex,
    Ethnicity = `Ethnicity simplified`,
    CountryBirth = `Country of birth`,
    CountryResidence = `Country of residence`,
    Nationality = Nationality,
    Language = Language,
    Student = `Student status`,
    Employment = `Employment status`
  )

# combine the part 1 datasets
part1 <-
  part1 %>%
  # link demographic data
  left_join(demographics, by = "PROLIFIC_PID") %>%
  # rename consequences variable
  rename(
    DecisionConsequences = Consequences,
    DecisionConsequences_Text = Consequences_3_TEXT
  ) %>%
  # clean dataset
  dplyr::select(PROLIFIC_PID, ProlificApprovals:Employment, everything()) %>%
  dplyr::select(!c(Status, IPAddress, ExternalReference, ResponseId, 
                   RecipientFirstName, RecipientLastName, RecipientEmail, 
                   LocationLatitude, LocationLongitude, DistributionChannel,
                   UserLanguage, counter, prolific_id, SESSION_ID, STUDY_ID)) %>%
  # rename qualtrics variables
  rename(
    StartDate_Part1       = StartDate,
    EndDate_Part1         = EndDate,
    Progress_Part1        = Progress,
    DurationSeconds_Part1 = `Duration (in seconds)`,
    Finished_Part1        = Finished,
    RecordedDate_Part1    = RecordedDate,
    Consent1_Part1        = Consent_1,
    Consent2_Part1        = Consent_2,
    Consent3_Part1        = Consent_3,
    Consent4_Part1        = Consent_4,
    Consent5_Part1        = Consent_5,
    Consent6_Part1        = Consent_6,
    Consent7_Part1        = Consent_7
    )

#### Part 2 - Surveys ####

part2 <-
  qualtRics::read_survey("study2_part2_surveys/PUN_BATTERY_SURVEYS+-+US_July+3,+2023_10.18.csv",
                         time_zone = "UTC") %>%
  # use ProlificID as PROLIFIC_PID has 2 NAs
  dplyr::select(!PROLIFIC_PID) %>%
  # remove one case where prolific ID was not in part 1
  filter(ProlificID %in% part1$PROLIFIC_PID) %>%
  # remove duplicated prolific IDs (keep first case with complete data)
  filter(!(ResponseId %in% c("R_2XaMS7HRuFbDG2a", "R_C9e6SjIH4V6lsZP", "R_3ET0LhJjCl2SgTQ",
                             "R_2QmOrulPe77WWBW", "R_3iKitSgRMxp5LrB", "R_2bHNBt2y4rEBJLS",
                             "R_xFrjsL0LHPYWWZ3", "R_21uYPFUDAFScUoQ", "R_1isLml6vDXkUF7a",
                             "R_3KxUmDcxYAP9PYV", "R_vekaeQNcLMQ7P7X", "R_1esDgEUnWH3buY5"))) %>%
  # clean dataset
  dplyr::select(!c(Status, ResponseId, DistributionChannel, UserLanguage, counter)) %>%
  # rename qualtrics variables
  rename(
    StartDate_Part2       = StartDate,
    EndDate_Part2         = EndDate,
    Progress_Part2        = Progress,
    DurationSeconds_Part2 = `Duration (in seconds)`,
    Finished_Part2        = Finished,
    RecordedDate_Part2    = RecordedDate,
    Consent1_Part2        = Consent1,
    Consent2_Part2        = Consent2,
    Consent3_Part2        = Consent3,
    Consent4_Part2        = Consent4,
    Consent5_Part2        = Consent5,
    Consent6_Part2        = Consent6
  )

### Combine Part 1 and Part 2 ####

d <-
  part1 %>%
  left_join(part2, by = c("PROLIFIC_PID" = "ProlificID")) %>%
  # replace prolific ID with ID number
  mutate(ID = 20000 + 1:nrow(.)) %>%
  dplyr::select(ID, everything()) %>%
  dplyr::select(!PROLIFIC_PID)

#### Write to file ####

# write to csv file
write_csv(d, file = "study2_clean.csv")

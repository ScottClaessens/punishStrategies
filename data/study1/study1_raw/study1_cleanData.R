library(lubridate)
library(qualtRics)
library(tidyverse)

#### Source this file to produce clean dataset and metadata for Study 1 (UK)

#### Part 1 - Games ####

# load completes
part1completes <-
  qualtRics::read_survey("study1_part1_games/Qualtrics_complete_responses_PUN_BATTERY_GAMES_November+30,+2022_15.32.csv",
                         time_zone = "UTC") %>%
  # remove one testing row
  filter(!is.na(PROLIFIC_PID)) %>%
  # remove one duplicated prolific ID (keep first submission)
  filter(ResponseId != "R_31jE0BAgDjMSW0U")

# load incompletes
part1incompletes <-
  qualtRics::read_survey("study1_part1_games/incomplete_responses_PUN_BATTERY_GAMES_November+29,+2022_12.24.csv",
                         col_types = readr::cols(StartDate = readr::col_character())) %>%
  # keep only participants that completed all the games
  filter(!is.na(Explain)) %>%
  # remove one duplicated prolific ID (keep first submission)
  filter(ResponseId != "FS_1pW4oBKeoR1vtlV") %>%
  # times as UTC
  mutate(
    StartDate = as.POSIXct(StartDate, tz = "UTC", format = "%d/%m/%Y %H:%M"),
    EndDate   = as.POSIXct(EndDate,   tz = "UTC", format = "%d/%m/%Y %H:%M")
  )

# load prolific demographics
demographics <-
  read_csv("study1_part1_games/Demographics.csv") %>%
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

# additional slider data
part1sliders <-
  read_survey("study1_part1_games/Missing_data_sliders.csv") %>%
  # select only relevant columns
  dplyr::select(starts_with("Q"))

# combine the part 1 datasets
part1 <-
  part1completes %>%
  bind_rows(part1incompletes) %>%
  # remove duplicate prolific IDs between completes and incompletes (keep completes)
  filter(!(ResponseId %in% c("FS_1fjglTvxURwBw4I", "FS_3EhRh9UCu727bpv",
                             "FS_2xIZoxk65UkUfF7", "FS_1mCQqwq74ayEQLY",
                             "FS_1Lv5n6vQ2ppCAPT", "FS_doslxV3d3adL2h3",
                             "FS_3kzWKWR5wH0V1V1", "FS_2cpZZhb7sMWYlKg"))) %>%
  # link demographic data
  left_join(demographics, by = "PROLIFIC_PID") %>%
  # link slider data
  left_join(part1sliders, by = c("PROLIFIC_PID" = "Q1")) %>%
  # add data from slider survey and clean
  mutate(
    SelfRate_1  = ifelse(is.na(SelfRate_1 ), Q3_1 , SelfRate_1 ),
    SelfRate_2  = ifelse(is.na(SelfRate_2 ), Q3_2 , SelfRate_2 ),
    SelfRate_3  = ifelse(is.na(SelfRate_3 ), Q3_3 , SelfRate_3 ),
    SelfRate_4  = ifelse(is.na(SelfRate_4 ), Q3_4 , SelfRate_4 ),
    SelfRate_5  = ifelse(is.na(SelfRate_5 ), Q3_5 , SelfRate_5 ),
    SelfRate_6  = ifelse(is.na(SelfRate_6 ), Q3_6 , SelfRate_6 ),
    SelfRate_7  = ifelse(is.na(SelfRate_7 ), Q3_7 , SelfRate_7 ),
    SelfRate_8  = ifelse(is.na(SelfRate_8 ), Q3_8 , SelfRate_8 ),
    SelfRate_9  = ifelse(is.na(SelfRate_9 ), Q3_9 , SelfRate_9 ),
    SelfRate_10 = ifelse(is.na(SelfRate_10), Q3_10, SelfRate_10),
    SelfRate_11 = ifelse(is.na(SelfRate_11), Q3_11, SelfRate_11),
    DecisionConsequences = ifelse(is.na(Q120), Q4, Q120),
    DecisionConsequences_Text = ifelse(is.na(Q120_3_TEXT), Q4_3_TEXT, Q120_3_TEXT)
  ) %>%
  dplyr::select(!starts_with("Q")) %>%
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
  read_csv("study1_part2_surveys/pun_final.csv", na = c("", "NA", "#N/A")) %>%
  # use ProlificID rather than PROLIFIC_PID as the latter has NAs
  dplyr::select(!PROLIFIC_PID) %>%
  # remove one case where prolific ID was not in part 1
  filter(ProlificID %in% part1$PROLIFIC_PID) %>%
  # remove duplicated prolific IDs (keep first case with available data)
  filter(!(ResponseId %in% c("R_3JmiQ2KfI56N2BH", "R_2VaUTf8LRYpkSAV", "R_XZp4NkUhMgs9g9r",
                             "R_2AMuEqIaEHoQm9J", "R_XiGQpTPHKGdqWo9", "R_1JLW8myHOyj7GOy",
                             "R_VPXVdBeMavlQ8p3", "R_1QoLxcz58k2iKAX", "R_1oe93mIxzWV3m4N",
                             "R_WCiwlbKDPnaGkPD", "R_3MKCZ6sJJ5zLgPk", "R_OycdtrbfjwUEi77",
                             "R_31hdCwfT136lBqq", "R_3QVJAH3MiP959Zf", "R_3k819SSi2X35ggR"))) %>%
  # times as UTC
  mutate(
    StartDate    = as.POSIXct(StartDate,    tz = "UTC", format = "%d/%m/%Y %H:%M"),
    EndDate      = as.POSIXct(EndDate,      tz = "UTC", format = "%d/%m/%Y %H:%M"),
    RecordedDate = as.POSIXct(RecordedDate, tz = "UTC", format = "%d/%m/%Y %H:%M")
  ) %>%
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

#### Combine Part 1 and Part 2 ####

d <-
  part1 %>%
  left_join(part2, by = c("PROLIFIC_PID" = "ProlificID")) %>%
  # replace prolific ID with ID number
  mutate(ID = 10000 + 1:nrow(.)) %>%
  dplyr::select(ID, everything()) %>%
  dplyr::select(!PROLIFIC_PID) %>%
  mutate(YearsLivedCountry = NA)

#### Write to file ####

# write to csv file
write_csv(d, file = "study1_clean.csv")

# custom functions

# load study 1 data
loadData1 <- function(fileStudy1) {
  # labels for rwa scale
  rwaLabels <- c(
    "Very strongly disagree"     = 1,
    "Strongly disagree"          = 2,
    "Somewhat disagree"          = 3,
    "Slightly disagree"          = 4,
    "Neither agree nor disagree" = 5,
    "Slightly agree"             = 6,
    "Somewhat agree"             = 7,
    "Strongly agree"             = 8,
    "Very strongly agree"        = 9
    )
  # labels for god controls scale
  godLabels <- c(
    "Strongly disagree" = 1,
    "Somewhat disagree" = 2,
    "Slightly disagree" = 3,
    "Neutral"           = 4,
    "Slightly agree"    = 5,
    "Somewhat agree"    = 6,
    "Strongly agree"    = 7
  )
  # labels for education
  educationLabels <- c(
    "High School"                              = 1,
    "Diploma / other professional certificate" = 2,
    "Completed university"                     = 3,
    "Masters degree"                           = 4,
    "PhD or equivalent"                        = 5
  )
  # load data
  read_csv(fileStudy1) %>%
    # create variables
    mutate(
      
      ### punishment games
      # numeric punishment vars
      pun1_1 = ifelse(str_starts(NoDI1_Take,    "Yes"), 1, 0),
      pun1_2 = ifelse(str_starts(NoDI1_Nothing, "Yes"), 1, 0),
      pun2_1 = ifelse(str_starts(NoDI2_Take,    "Yes"), 1, 0),
      pun2_2 = ifelse(str_starts(NoDI2_Nothing, "Yes"), 1, 0),
      pun3_1 = ifelse(str_starts(NoDI3_Take,    "Yes"), 1, 0),
      pun3_2 = ifelse(str_starts(NoDI3_Nothing, "Yes"), 1, 0),
      pun4_1 = ifelse(str_starts(NoDI4_Take,    "Yes"), 1, 0),
      pun4_2 = ifelse(str_starts(NoDI4_Nothing, "Yes"), 1, 0),
      pun5_1 = ifelse(str_starts(DI_Take,       "Yes"), 1, 0),
      pun5_2 = ifelse(str_starts(DI_Nothing,    "Yes"), 1, 0),
      pun6_1 = ifelse(str_starts(`3PP_Take`,    "Yes"), 1, 0),
      pun6_2 = ifelse(str_starts(`3PP_Nothing`, "Yes"), 1, 0),
      # comprehension failures
      fail1 = !is.na(NoDI1_TryAgain) & !str_detect(NoDI1_TryAgain, fixed("0.40")),
      fail2 = !is.na(NoDI2_TryAgain) & !str_detect(NoDI2_TryAgain, fixed("0.40")),
      fail3 = !is.na(NoDI3_TryAgain) & !str_detect(NoDI3_TryAgain, fixed("0.40")),
      fail4 = !is.na(NoDI4_TryAgain) & (str_detect(NoDI4_TryAgain, fixed("0.50")) | str_detect(NoDI4_TryAgain, fixed("0.20"))),
      fail5 = !is.na(DI_TryAgain)    & (str_detect(DI_TryAgain, fixed("0.50")) | str_detect(DI_TryAgain, fixed("0.70")) | str_detect(DI_TryAgain, fixed("0.80"))),
      fail6 = !is.na(`3PP_TryAgain`) & (str_detect(`3PP_TryAgain`, fixed("0.00")) | str_detect(`3PP_TryAgain`, fixed("0.70")) | str_detect(`3PP_TryAgain`, fixed("1.30"))),
      
      ### social value orientation
      # reference: Murphy, R. O., Ackermann, K. A., & Handgraaf, M. (2011). Measuring social value orientation. Judgment and Decision making, 6(8), 771-781.
      # tutorial: http://ryanomurphy.com/resources/SVO_second_item_tutorial.pdf
      # SVO is a 15-item survey which can be split into primary measure (items 1-6) and secondary measure (items 7-15)
      # first, code which option was chosen
      SVO1  = c("85\n|\n|\n85"  = 1, "85\n|\n|\n76" = 2, "85\n|\n|\n68"  = 3,
                "85\n|\n|\n59"  = 4, "85\n|\n|\n50" = 5, "85\n|\n|\n41"  = 6,
                "85\n|\n|\n33"  = 7, "85\n|\n|\n24" = 8, "85\n|\n|\n15"  = 9)[SVO_01] %>% as.numeric(),
      SVO2  = c("85\n|\n|\n15"  = 1, "87\n|\n|\n19" = 2, "89\n|\n|\n24"  = 3,
                "91\n|\n|\n28"  = 4, "93\n|\n|\n33" = 5, "94\n|\n|\n37"  = 6,
                "96\n|\n|\n41"  = 7, "98\n|\n|\n46" = 8, "100||50"       = 9)[SVO_02] %>% as.numeric(),
      SVO3  = c("50\n|\n|\n100" = 1, "54\n|\n|\n98" = 2, "59\n|\n|\n96"  = 3,
                "63\n|\n|\n94"  = 4, "68\n|\n|\n93" = 5, "72\n|\n|\n91"  = 6,
                "76\n|\n|\n89"  = 7, "81\n|\n|\n87" = 8, "85\n|\n|\n85"  = 9)[SVO_03] %>% as.numeric(),
      SVO4  = c("50\n|\n|\n100" = 1, "54\n|\n|\n89" = 2, "59\n|\n|\n79"  = 3,
                "63\n|\n|\n68"  = 4, "68\n|\n|\n58" = 5, "72\n|\n|\n47"  = 6,
                "76\n|\n|\n36"  = 7, "81\n|\n|\n26" = 8, "85\n|\n|\n15"  = 9)[SVO_04] %>% as.numeric(),
      SVO5  = c("100\n|\n|\n50" = 1, "94\n|\n|\n56" = 2, "88\n|\n|\n63"  = 3,
                "81\n|\n|\n69"  = 4, "75\n|\n|\n75" = 5, "69\n|\n|\n81"  = 6,
                "63\n|\n|\n88"  = 7, "56\n|\n|\n94" = 8, "50\n|\n|\n100" = 9)[SVO_05] %>% as.numeric(),
      SVO6  = c("100\n|\n|\n50" = 1, "98\n|\n|\n54" = 2, "96\n|\n|\n59"  = 3,
                "94\n|\n|\n63"  = 4, "93\n|\n|\n68" = 5, "91\n|\n|\n72"  = 6,
                "89\n|\n|\n76"  = 7, "87\n|\n|\n81" = 8, "85\n|\n|\n85"  = 9)[SVO_06] %>% as.numeric(),
      SVO7  = c("100\n|\n|\n50" = 1, "96\n|\n|\n56" = 2, "93\n|\n|\n63"  = 3,
                "89\n|\n|\n69"  = 4, "85\n|\n|\n75" = 5, "81\n|\n|\n81"  = 6,
                "78\n|\n|\n88"  = 7, "74\n|\n|\n94" = 8, "70\n|\n|\n100" = 9)[SVO_07] %>% as.numeric(),
      SVO8  = c("90\n|\n|\n100" = 1, "91\n|\n|\n99" = 2, "93\n|\n|\n98"  = 3,
                "94\n|\n|\n96"  = 4, "95\n|\n|\n95" = 5, "96\n|\n|\n94"  = 6,
                "98\n|\n|\n93"  = 7, "99\n|\n|\n91" = 8, "100\n|\n|\n90" = 9)[SVO_08] %>% as.numeric(),
      SVO9  = c("100\n|\n|\n70" = 1, "94\n|\n|\n74" = 2, "88\n|\n|\n78"  = 3,
                "81\n|\n|\n81"  = 4, "75\n|\n|\n85" = 5, "69\n|\n|\n89"  = 6,
                "63\n|\n|\n93"  = 7, "56\n|\n|\n96" = 8, "50\n|\n|\n100" = 9)[SVO_09] %>% as.numeric(),
      SVO10 = c("100\n|\n|\n70" = 1, "99\n|\n|\n74" = 2, "98\n|\n|\n78"  = 3,
                "96\n|\n|\n81"  = 4, "95\n|\n|\n85" = 5, "94\n|\n|\n89"  = 6,
                "93\n|\n|\n93"  = 7, "91\n|\n|\n96" = 8, "90\n|\n|\n100" = 9)[SVO_10] %>% as.numeric(),
      SVO11 = c("70\n|\n|\n100" = 1, "74\n|\n|\n96" = 2, "78\n|\n|\n93"  = 3,
                "81\n|\n|\n89"  = 4, "85\n|\n|\n85" = 5, "89\n|\n|\n81"  = 6,
                "93\n|\n|\n78"  = 7, "96\n|\n|\n74" = 8, "100\n|\n|\n70" = 9)[SVO_11] %>% as.numeric(),
      SVO12 = c("50\n|\n|\n100" = 1, "56\n|\n|\n99" = 2, "63\n|\n|\n98"  = 3,
                "69\n|\n|\n96"  = 4, "75\n|\n|\n95" = 5, "81\n|\n|\n94"  = 6,
                "88\n|\n|\n93"  = 7, "94\n|\n|\n91" = 8, "100\n|\n|\n90" = 9)[SVO_12] %>% as.numeric(),
      SVO13 = c("50\n|\n|\n100" = 1, "56\n|\n|\n94" = 2, "63\n|\n|\n88"  = 3,
                "69\n|\n|\n81"  = 4, "75\n|\n|\n75" = 5, "81\n|\n|\n69"  = 6,
                "88\n|\n|\n63"  = 7, "94\n|\n|\n56" = 8, "100\n|\n|\n50" = 9)[SVO_13] %>% as.numeric(),
      SVO14 = c("100\n|\n|\n90" = 1, "96\n|\n|\n91" = 2, "93\n|\n|\n93"  = 3,
                "89\n|\n|\n94"  = 4, "85\n|\n|\n95" = 5, "81\n|\n|\n96"  = 6,
                "78\n|\n|\n98"  = 7, "74\n|\n|\n99" = 8, "70\n|\n|\n100" = 9)[SVO_14] %>% as.numeric(),
      SVO15 = c("90\n|\n|\n100" = 1, "91\n|\n|\n94" = 2, "93\n|\n|\n88"  = 3,
                "94\n|\n|\n81"  = 4, "95\n|\n|\n75" = 5, "96\n|\n|\n69"  = 6,
                "98\n|\n|\n63"  = 7, "99\n|\n|\n56" = 8, "100\n|\n|\n50" = 9)[SVO_15] %>% as.numeric(),
      # payoff to self for chosen option
      SVO1self  = parse_number(str_sub(SVO_01, 1, 3)),
      SVO2self  = parse_number(str_sub(SVO_02, 1, 3)),
      SVO3self  = parse_number(str_sub(SVO_03, 1, 3)),
      SVO4self  = parse_number(str_sub(SVO_04, 1, 3)),
      SVO5self  = parse_number(str_sub(SVO_05, 1, 3)),
      SVO6self  = parse_number(str_sub(SVO_06, 1, 3)),
      SVO7self  = parse_number(str_sub(SVO_07, 1, 3)),
      SVO8self  = parse_number(str_sub(SVO_08, 1, 3)),
      SVO9self  = parse_number(str_sub(SVO_09, 1, 3)),
      SVO10self = parse_number(str_sub(SVO_10, 1, 3)),
      SVO11self = parse_number(str_sub(SVO_11, 1, 3)),
      SVO12self = parse_number(str_sub(SVO_12, 1, 3)),
      SVO13self = parse_number(str_sub(SVO_13, 1, 3)),
      SVO14self = parse_number(str_sub(SVO_14, 1, 3)),
      SVO15self = parse_number(str_sub(SVO_15, 1, 3)),
      # payoff to other for chosen option
      SVO1other  = parse_number(str_sub(SVO_01, -3, -1)),
      SVO2other  = parse_number(str_sub(SVO_02, -3, -1)),
      SVO3other  = parse_number(str_sub(SVO_03, -3, -1)),
      SVO4other  = parse_number(str_sub(SVO_04, -3, -1)),
      SVO5other  = parse_number(str_sub(SVO_05, -3, -1)),
      SVO6other  = parse_number(str_sub(SVO_06, -3, -1)),
      SVO7other  = parse_number(str_sub(SVO_07, -3, -1)),
      SVO8other  = parse_number(str_sub(SVO_08, -3, -1)),
      SVO9other  = parse_number(str_sub(SVO_09, -3, -1)),
      SVO10other = parse_number(str_sub(SVO_10, -3, -1)),
      SVO11other = parse_number(str_sub(SVO_11, -3, -1)),
      SVO12other = parse_number(str_sub(SVO_12, -3, -1)),
      SVO13other = parse_number(str_sub(SVO_13, -3, -1)),
      SVO14other = parse_number(str_sub(SVO_14, -3, -1)),
      SVO15other = parse_number(str_sub(SVO_15, -3, -1)),
      # for secondary svo items, calculate mean absolute difference from archetypal inequality aversion (DIA)
      # by subtracting subject's own option from option that would yield perfect equality in outcomes
      DIA1 = abs(SVO7  - 6) / 8,
      DIA2 = abs(SVO8  - 5) / 8,
      DIA3 = abs(SVO9  - 4) / 8,
      DIA4 = abs(SVO10 - 7) / 8,
      DIA5 = abs(SVO11 - 5) / 8,
      DIA6 = abs(SVO12 - 8) / 8,
      DIA7 = abs(SVO13 - 5) / 8,
      DIA8 = abs(SVO14 - 3) / 8,
      DIA9 = abs(SVO15 - 2) / 8,
      # for secondary svo items, calculate mean absolute difference from archetypal joint gain maximisation (DJG)
      DJG1 = abs(SVO7  - 9) / 8,
      # -- note: DJG2 = no calculation possible for SVO8
      DJG3 = abs(SVO9  - 1) / 8,
      DJG4 = abs(SVO10 - 9) / 8,
      # -- note: DJG5 = no calculation possible for SVO11
      DJG6 = abs(SVO12 - 9) / 8,
      # -- note: DJG7 = no calculation possible for SVO13
      DJG8 = abs(SVO14 - 1) / 8,
      DJG9 = abs(SVO15 - 1) / 8,
      # for secondary svo items, calculate mean absolute difference from archetypal altruism (DAL)
      DAL1 = abs(SVO7  - 9) / 8,
      DAL2 = abs(SVO8  - 1) / 8,
      DAL3 = abs(SVO9  - 9) / 8,
      DAL4 = abs(SVO10 - 9) / 8,
      DAL5 = abs(SVO11 - 1) / 8,
      DAL6 = abs(SVO12 - 1) / 8,
      DAL7 = abs(SVO13 - 1) / 8,
      DAL8 = abs(SVO14 - 9) / 8,
      DAL9 = abs(SVO15 - 1) / 8,
      # for secondary svo items, calculate mean absolute difference from archetypal indivdualism/competitiveness (DIC)
      DIC1 = abs(SVO7  - 1) / 8,
      DIC2 = abs(SVO8  - 9) / 8,
      DIC3 = abs(SVO9  - 1) / 8,
      DIC4 = abs(SVO10 - 1) / 8,
      DIC5 = abs(SVO11 - 9) / 8,
      DIC6 = abs(SVO12 - 9) / 8,
      DIC7 = abs(SVO13 - 9) / 8,
      DIC8 = abs(SVO14 - 1) / 8,
      DIC9 = abs(SVO15 - 9) / 8,
      
      ### social dominance orientation
      # reference: Ho, A. K., Sidanius, J., Kteily, N., Sheehy-Skeffington, J., Pratto, F., Henkel, K. E., ... & Stewart, A. L. (2015). The nature of social dominance orientation: Theorizing and measuring preferences for intergroup inequality using the new SDO₇ scale. Journal of personality and social psychology, 109(6), 1003.
      # SDO is a 8-item survey where participants indicate agreement on a 7-point Likert scale. Higher score = higher SDO
      # N.B. items 3, 4, 7, 8 are reverse-coded
      SDO1 =     parse_number(SDO_1),
      SDO2 =     parse_number(SDO_2),
      SDO3 = 8 - parse_number(SDO_3r),
      SDO4 = 8 - parse_number(SDO_4r),
      SDO5 =     parse_number(SDO_5),
      SDO6 =     parse_number(SDO_6),
      SDO7 = 8 - parse_number(SDO_7r),
      SDO8 = 8 - parse_number(SDO_8r),
      
      ### personality (mini ipip-6)
      # reference: Sibley, C. G., Luyten, N., Purnomo, M., Mobberley, A., Wootton, L. W., Hammond, M. D., ... & Robertson, A. (2011). The Mini-IPIP6: Validation and extension of a short measure of the Big-Six factors of personality in New Zealand. New Zealand Journal of Psychology, 40(3).
      # a 24-item survey measuring 6 aspects of personality: Openness, Conscientiousness, Extraversion, Agreeableness, Neuroticism, and Honesty-Humility
      # Openness (items O2, O3, and O4 are reverse coded)
      Open1   =     parse_number(Pers_O1),
      Open2   = 8 - parse_number(Pers_O2r),
      Open3   = 8 - parse_number(Pers_O3r),
      Open4   = 8 - parse_number(Pers_O4r),
      # Conscientiousness (items C3 and C4 are reverse coded)
      Consc1  =     parse_number(Pers_C1),
      Consc2  =     parse_number(Pers_C2),
      Consc3  = 8 - parse_number(Pers_C3r),
      Consc4  = 8 - parse_number(Pers_C4r),
      # Extraversion (items E2 and E3 are reverse coded)
      Extra1  =     parse_number(Pers_E1),
      Extra2  = 8 - parse_number(Pers_E2r),
      Extra3  = 8 - parse_number(Pers_E3r),
      Extra4  =     parse_number(Pers_E4),
      # Agreeableness (items A2 and A4 are reverse coded)
      Agree1  =     parse_number(Pers_A1),
      Agree2  = 8 - parse_number(Pers_A2r),
      Agree3  =     parse_number(Pers_A3),
      Agree4  = 8 - parse_number(Pers_A4r),
      # Neuroticism (items N2 and N4 are reverse coded)
      Neur1   =     parse_number(Pers_N1),
      Neur2   = 8 - parse_number(Pers_N2r),
      Neur3   =     parse_number(Pers_N3),
      Neur4   = 8 - parse_number(Pers_N4r),
      # Honesty-Humility (all items reverse coded)
      Honest1 = 8 - parse_number(Pers_Narc1),
      Honest2 = 8 - parse_number(Pers_Narc2),
      Honest3 = 8 - parse_number(Pers_HonHum1r),
      Honest4 = 8 - parse_number(Pers_HonHum2r),
      
      ### right-wing authoritarianism
      # reference: Bizumic, B., & Duckitt, J. (2018). Investigating right wing authoritarianism with a very short authoritarianism scale.
      # a 6-item survey using 9-point Likert scale. Higher score indicates higher RWA.
      # N.B. Items 1, 4, 5 are reverse coded
      RWA1 = 10 - as.numeric(rwaLabels[RWA_1r]),
      RWA2 =      as.numeric(rwaLabels[RWA_2]),
      RWA3 =      as.numeric(rwaLabels[RWA_3]),
      RWA4 = 10 - as.numeric(rwaLabels[RWA_4r]),
      RWA5 = 10 - as.numeric(rwaLabels[RWA_5r]),
      RWA6 =      as.numeric(rwaLabels[RWA_6]),
      
      ### religion questions
      # god controls events
      GodC = as.numeric(godLabels[GodControls]),
      # religiosity
      Religiosity = parse_number(HowReligious),
      
      # bring up/down on SES ladder
      BringDown = parse_number(BringDown_1),
      BringUp   = parse_number(BringUp_1),
      
      # education as numeric
      EducationNum = as.numeric(educationLabels[Education])
      
    ) %>%
    rowwise() %>%
    mutate(
      
      ### final SVO calculations
      # mean payoffs to self and other for primary items
      SVOselfMean = mean(c(SVO1self, SVO2self, SVO3self, SVO4self, SVO5self, SVO6self)),
      SVOotherMean = mean(c(SVO1other, SVO2other, SVO3other, SVO4other, SVO5other, SVO6other)),
      # to compute SVO angle, calculate inverse tangent of ratio of mean payoffs allocated to other vs self
      SVOangle = atan((SVOotherMean - 50) / (SVOselfMean - 50)) * (180 / 3.14159265359),
      # define SVO categories
      SVOcategory = ifelse(SVOangle <= -12.04, "Competitive",
                           ifelse(SVOangle <= 22.45, "Individualistic",
                                  ifelse(SVOangle <= 57.15, "Cooperative", 
                                         ifelse(SVOangle > 57.15, "Altruistic", "N/A")))),
      # secondary item means (only for "cooperative" svo classifications)
      DIA = ifelse(SVOcategory == "Cooperative", mean(c(DIA1, DIA2, DIA3, DIA4, DIA5, DIA6, DIA7, DIA8, DIA9)), NA),
      DJG = ifelse(SVOcategory == "Cooperative", mean(c(DJG1, DJG3, DJG4, DJG6, DJG8, DJG9)), NA),
      DAL = ifelse(SVOcategory == "Cooperative", mean(c(DAL1, DAL2, DAL3, DAL4, DAL5, DAL6, DAL7, DAL8, DAL9)), NA),
      DIC = ifelse(SVOcategory == "Cooperative", mean(c(DIC1, DIC2, DIC3, DIC4, DIC5, DIC6, DIC7, DIC8, DIC9)), NA),
      # calculate inequality aversion index
      IAIndex = ifelse(
        # only consider subjects whose choice patterns in secondary items are consistent with both IA and JGM, and not with individualism / altruism
        # to do this, ensure that DIA <= [DAL,DIC] and DJG <= [DAL,DIC]
        (DIA <= DAL) & (DIA <= DIC) & (DJG <= DAL) & (DJG <= DIC),
        # for tsujects that pass the above "transitivity check", calculate inequality aversion index
        DIA / (DIA + DJG),
        # for remaining subjects, return NA
        NA),
      
      ### means for sdo, rwa, and personality
      SDO    = mean(c(SDO1, SDO2, SDO3, SDO4, SDO5, SDO6, SDO7, SDO8)),
      RWA    = mean(c(RWA1, RWA2, RWA3, RWA4, RWA5, RWA6)),
      Open   = mean(c(Open1, Open2, Open3, Open4)),
      Consc  = mean(c(Consc1, Consc2, Consc3, Consc4)),
      Extra  = mean(c(Extra1, Extra2, Extra3, Extra4)),
      Agree  = mean(c(Agree1, Agree2, Agree3, Agree4)),
      Neur   = mean(c(Neur1, Neur2, Neur3, Neur4)),
      Honest = mean(c(Honest1, Honest2, Honest3, Honest4)),
      
      ### raw strategy classification
      # proportion of decisions in line with each strategy
      propStrategy_Competitive   = mean(c(pun1_1 == 1, pun1_2 == 1, pun2_1 == 1, pun2_2 == 1,
                                          pun3_1 == 1, pun3_2 == 1, pun4_1 == 0, pun4_2 == 0,
                                          pun5_1 == 1, pun5_2 == 1, pun6_1 == 1, pun6_2 == 1)),
      propStrategy_AvoidDI       = mean(c(pun1_1 == 0, pun1_2 == 0, pun2_1 == 0, pun2_2 == 0,
                                          pun3_1 == 0, pun3_2 == 0, pun4_1 == 0, pun4_2 == 0,
                                          pun5_1 == 1, pun5_2 == 0, pun6_1 == 0, pun6_2 == 0)),
      propStrategy_Egalitarian   = mean(c(pun1_1 == 0, pun1_2 == 0, pun2_1 == 0, pun2_2 == 0,
                                          pun3_1 == 0, pun3_2 == 0, pun4_1 == 0, pun4_2 == 0,
                                          pun5_1 == 1, pun5_2 == 0, pun6_1 == 1, pun6_2 == 0)),
      propStrategy_SeekAI        = mean(c(pun1_1 == 0, pun1_2 == 0, pun2_1 == 1, pun2_2 == 0,
                                          pun3_1 == 1, pun3_2 == 0, pun4_1 == 0, pun4_2 == 0,
                                          pun5_1 == 0, pun5_2 == 0, pun6_1 == 0, pun6_2 == 0)),
      propStrategy_Retributive   = mean(c(pun1_1 == 1, pun1_2 == 0, pun2_1 == 1, pun2_2 == 0,
                                          pun3_1 == 1, pun3_2 == 0, pun4_1 == 1, pun4_2 == 0,
                                          pun5_1 == 1, pun5_2 == 0, pun6_1 == 0, pun6_2 == 0)),
      propStrategy_Deterrent     = mean(c(pun1_1 == 1, pun1_2 == 0, pun2_1 == 1, pun2_2 == 0,
                                          pun3_1 == 0, pun3_2 == 0, pun4_1 == 1, pun4_2 == 0,
                                          pun5_1 == 1, pun5_2 == 0, pun6_1 == 0, pun6_2 == 0)),
      propStrategy_NormEnforcing = mean(c(pun1_1 == 1, pun1_2 == 0, pun2_1 == 1, pun2_2 == 0, 
                                          pun3_1 == 0, pun3_2 == 0, pun4_1 == 1, pun4_2 == 0,
                                          pun5_1 == 1, pun5_2 == 0, pun6_1 == 1, pun6_2 == 0)),
      propStrategy_Antisocial    = mean(c(pun1_1 == 0, pun1_2 == 1, pun2_1 == 0, pun2_2 == 1, 
                                          pun3_1 == 0, pun3_2 == 1, pun4_1 == 0, pun4_2 == 1,
                                          pun5_1 == 0, pun5_2 == 1, pun6_1 == 0, pun6_2 == 1)),
      propStrategy_NoPunish      = mean(c(pun1_1 == 0, pun1_2 == 0, pun2_1 == 0, pun2_2 == 0,
                                          pun3_1 == 0, pun3_2 == 0, pun4_1 == 0, pun4_2 == 0,
                                          pun5_1 == 0, pun5_2 == 0, pun6_1 == 0, pun6_2 == 0)),
      # exact strategy classification (i.e. if any of the above == 1)
      strategy = ifelse(propStrategy_Competitive   == 1, "Competitive",                       "N/A"),
      strategy = ifelse(propStrategy_AvoidDI       == 1, "Avoid disadvantageous inequity",    strategy),
      strategy = ifelse(propStrategy_Egalitarian   == 1, "Egalitarian",                       strategy),
      strategy = ifelse(propStrategy_SeekAI        == 1, "Seek advantageous inequity",        strategy),
      strategy = ifelse(propStrategy_Retributive   == 1, "Retributive",                       strategy),
      strategy = ifelse(propStrategy_Deterrent     == 1, "Deterrent",                         strategy),
      strategy = ifelse(propStrategy_NormEnforcing == 1, "Norm-enforcing",                    strategy),
      strategy = ifelse(propStrategy_Antisocial    == 1, "Exclusively antisocial punishment", strategy),
      strategy = ifelse(propStrategy_NoPunish      == 1, "Anti-punish",                       strategy),
      strategy = factor(strategy, levels = c("Competitive", "Avoid disadvantageous inequity",
                                             "Egalitarian", "Seek advantageous inequity",
                                             "Retributive", "Deterrent", "Norm-enforcing",
                                             "Exclusively antisocial punishment",
                                             "Anti-punish", "N/A"))
    ) %>%
    ungroup() %>%
    # rename some variables
    rename(
      SES        = SES_1, 
      PolSlider  = PolSlider_19,
      SelfRate1  = SelfRate_1,
      SelfRate2  = SelfRate_2,
      SelfRate3  = SelfRate_3,
      SelfRate4  = SelfRate_4,
      SelfRate5  = SelfRate_5,
      SelfRate6  = SelfRate_6,
      SelfRate7  = SelfRate_7,
      SelfRate8  = SelfRate_8,
      SelfRate9  = SelfRate_9,
      SelfRate10 = SelfRate_10,
      SelfRate11 = SelfRate_11
      )
}

# data exclusions from study 1
excludeData1 <- function(d1) {
  # labels for rwa scale
  rwaLabels <- c(
    "Very strongly disagree"     = 1,
    "Strongly disagree"          = 2,
    "Somewhat disagree"          = 3,
    "Slightly disagree"          = 4,
    "Neither agree nor disagree" = 5,
    "Slightly agree"             = 6,
    "Somewhat agree"             = 7,
    "Strongly agree"             = 8,
    "Very strongly agree"        = 9
  )
  # exclusions
  # n = 1019
  d1 %>%
    # exclude participants who sped through surveys (criteria = 2 SDs below median duration)
    filter(DurationSeconds_Part1 > median(DurationSeconds_Part1, na.rm = TRUE) - sd(DurationSeconds_Part1, na.rm = TRUE)*2) %>%
    filter(ifelse(is.na(DurationSeconds_Part2), TRUE, 
                  DurationSeconds_Part2 > median(DurationSeconds_Part2, na.rm = TRUE) - sd(DurationSeconds_Part2, na.rm = TRUE)*2)) %>%
    # n = 1019
    # exclude participants who failed "days of week" attention check
    filter(is.na(DaysWeek_TryAgain) | DaysWeek_TryAgain == 7) %>%
    # n = 1019
    # exclude participants who failed "weeks in year" attention check
    filter(is.na(WeeksYear_TryAgain) | WeeksYear_TryAgain == 52) %>%
    # n = 1019
    # exclude participants who provide nonsensical answer to breakfast question
    slice(c(-21,-279,-335,-872,-912)) %>%
    # n = 1014
    # exclude participants who flatline (criteria = two or more matrix tables)
    rowwise() %>%
    mutate(
      flatSDO   = sd(parse_number(c_across(starts_with("SDO_"))), na.rm = TRUE),
      flatRWA   = sd(rwaLabels[c_across(starts_with("RWA_"))], na.rm = TRUE),
      flatPers  = sd(parse_number(c_across(starts_with("Pers_"))), na.rm = TRUE),
      flatTotal = sum(c(flatSDO == 0, flatRWA == 0, flatPers == 0), na.rm = TRUE)
      ) %>%
    ungroup() %>%
    filter(!(flatTotal >= 2)) %>%
    # n = 1014
    # remove specific game data for participants who fail comprehension checks
    mutate(
      NoDI1_Take    = ifelse(fail1, NA, NoDI1_Take),
      NoDI1_Nothing = ifelse(fail1, NA, NoDI1_Nothing),
      NoDI2_Take    = ifelse(fail2, NA, NoDI2_Take),
      NoDI2_Nothing = ifelse(fail2, NA, NoDI2_Nothing),
      NoDI3_Take    = ifelse(fail3, NA, NoDI3_Take),
      NoDI3_Nothing = ifelse(fail3, NA, NoDI3_Nothing),
      NoDI4_Take    = ifelse(fail4, NA, NoDI4_Take),
      NoDI4_Nothing = ifelse(fail4, NA, NoDI4_Nothing),
      DI_Take       = ifelse(fail5, NA, DI_Take),
      DI_Nothing    = ifelse(fail5, NA, DI_Nothing),
      `3PP_Take`    = ifelse(fail6, NA, `3PP_Take`),
      `3PP_Nothing` = ifelse(fail6, NA, `3PP_Nothing`)
    )
}

# plot sample demographics
plotSampleStudy1 <- function(d1) {
  # age
  pA <- 
    ggplot(d1, aes(x = Age)) +
    geom_histogram(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # gender
  pB <-
    ggplot(d1, aes(x = Gender)) +
    geom_bar(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # ethnicity
  pC <-
    ggplot(d1, aes(x = Ethnicity)) +
    geom_bar(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # student
  pD <-
    ggplot(d1, aes(x = Student)) +
    geom_bar(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # employment
  pE <-
    d1 %>%
    mutate(
      Employment = ifelse(str_starts(Employment, fixed("Not")), "Not in paid work", Employment),
      Employment = ifelse(str_starts(Employment, fixed("Un")), "Unemployed", Employment),
      Employment = ifelse(str_starts(Employment, fixed("Due")), "Starting new job", Employment)
    ) %>%
    ggplot(aes(x = Employment)) +
    geom_bar(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 7))
  # put together and save
  out <- 
    plot_grid(
      plot_grid(pA, pC, nrow = 1), 
      plot_grid(pD, pB, pE, nrow = 1), 
      nrow = 2
    )
  ggsave(out, filename = "figures/study1/sample.pdf", width = 7, height = 5)
  return(out)
}

# plot wordcloud for open-ended question
plotWordcloud <- function(d1) {
  # from: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
  # clean responses
  d1$Explain <- gsub("@\\S*", "", d1$Explain) 
  d1$Explain <- gsub("amp", "", d1$Explain) 
  d1$Explain <- gsub("[\r\n]", "", d1$Explain)
  d1$Explain <- gsub("[[:punct:]]", "", d1$Explain)
  # create document term matrix
  words <- 
    d1 %>%
    dplyr::select(Explain) %>%
    unnest_tokens(word, Explain) %>%
    count(word, sort = TRUE) %>%
    # remove common words
    filter(!(word %in% c("to","i","the","my","as","was","not",
                         "of","and","if","it","in","be","a","me",
                         "from","for","that","would","did","didnt",
                         "so","they","make","with","had","wanted",
                         "want","or","much","but","have","on","no",
                         "by","do","at","am","is","2","010","its",
                         "there","your","dont","them","also","when",
                         "any","their","see","than","then","what",
                         "an","well","3")))
  # create wordcloud and save to file
  set.seed(2113)
  png(file = "figures/study1/wordcloud.png", width = 400, height = 400)
  wordcloud(
    words = words$word,
    freq = words$n,
    min.freq = 10,
    colors = brewer.pal(8, "Dark2")
  )
  dev.off()
}

# plot punishment rates
plotPunDecisions <- function(d1) {
  # games vector
  games <- c(
    "No Disadvantageous\nInequity 1",
    "No Disadvantageous\nInequity 2",
    "No Disadvantageous\nInequity 3 (Computer)",
    "No Disadvantageous\nInequity 4 (1:1 Fee Fine)",
    "Disadvantageous\nInequity",
    "Third-Party"
  )
  # plot
  out <-
    d1 %>%
    # from wide to long
    pivot_longer(
      cols = starts_with("pun"),
      names_to = "Decision",
      values_to = "Punishment"
    ) %>%
    # prepare for plot
    separate(Decision, into = c("Game", "Other")) %>%
    transmute(
      # game labels
      Game = ifelse(Game == "pun1", games[1], Game),
      Game = ifelse(Game == "pun2", games[2], Game),
      Game = ifelse(Game == "pun3", games[3], Game),
      Game = ifelse(Game == "pun4", games[4], Game),
      Game = ifelse(Game == "pun5", games[5], Game),
      Game = ifelse(Game == "pun6", games[6], Game),
      Game = factor(Game, levels = games),
      # other labels
      Other = factor(ifelse(Other == 1, "Take", "Do nothing"), levels = c("Take", "Do nothing")),
      # punishment
      Punishment = Punishment
    ) %>%
    # summarise
    group_by(Game, Other) %>%
    summarise(Punishment = mean(Punishment), .groups = "drop") %>%
    # plot
    ggplot(aes(x = Game, y = Punishment, fill = Other)) +
    geom_col(position = "dodge") +
    scale_y_continuous(name = "Proportion of punishment", limits = c(0, 1),
                       expand = c(0, 0)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 50, hjust = 1, size = 9),
      axis.title.x = element_blank(),
      legend.title = element_blank()
    )
  # save
  ggsave(out, filename = "figures/study1/propPunish.pdf", width = 6, height = 5)
  return(out)
}

# plot slider ratings
plotSliderRatings1 <- function(d1) {
  # named vector for converting to plot labels
  labels <- c(
    "SelfRate1"  = "I wanted to punish people who harmed others",
    "SelfRate2"  = "I wanted to have a higher final bonus than others",
    "SelfRate3"  = "I wanted to avoid having a lower final bonus than others",
    "SelfRate4"  = "I wanted all players to have the same final bonus",
    "SelfRate5"  = "I wanted to stop others from cheating",
    "SelfRate6"  = "I wanted to show that I disapproved of others' actions",
    "SelfRate7"  = "I made decisions at random",
    "SelfRate8"  = "I wanted to punish people who DID NOT harm me or others",
    "SelfRate9"  = "I didn't want to reduce anyone's bonus, no matter what they did",
    "SelfRate10" = "I didn't want to PAY to reduce anyone's bonus but I would have done so if it were free",
    "SelfRate11" = "I wanted to punish people if they harmed me but not if they harmed others"
  )
  # plot
  out <-
    d1 %>%
    # from wide to long
    pivot_longer(
      cols = starts_with("SelfRate"),
      names_to = "SelfRateSlider",
      values_to = "SelfRate"
    ) %>%
    # edit slider labels
    mutate(SelfRateSlider = factor(labels[SelfRateSlider], levels = labels)) %>%
    # plot
    ggplot(aes(x = SelfRate, y = fct_rev(SelfRateSlider))) +
    geom_boxplot(outlier.shape = NA) +
    labs(x = "Rating", y = NULL) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/study1/sliders1.pdf", width = 7, height = 3)
  return(out)
}

# plot slider ratings
plotSliderRatings2 <- function(d1) {
  # named vector for converting to plot labels
  labels <- c(
    "SelfRate1"  = "I wanted to punish people who harmed others",
    "SelfRate2"  = "I wanted to have a higher final bonus than others",
    "SelfRate3"  = "I wanted to avoid having a lower final bonus than others",
    "SelfRate4"  = "I wanted all players to have the same final bonus",
    "SelfRate5"  = "I wanted to stop others from cheating",
    "SelfRate6"  = "I wanted to show that I disapproved of others' actions",
    "SelfRate7"  = "I made decisions at random",
    "SelfRate8"  = "I wanted to punish people who DID NOT harm me or others",
    "SelfRate9"  = "I didn't want to reduce anyone's bonus, no matter what they did",
    "SelfRate10" = "I didn't want to PAY to reduce anyone's bonus but I would have done so if it were free",
    "SelfRate11" = "I wanted to punish people if they harmed me but not if they harmed others"
  )
  # plot
  out <-
    d1 %>%
    # create average
    mutate(avgSelfRate = rowMeans(dplyr::select(d1, starts_with("SelfRate")))) %>%
    # from wide to long
    pivot_longer(
      cols = starts_with("SelfRate"),
      names_to = "SelfRateSlider",
      values_to = "SelfRate"
    ) %>%
    # edit slider labels and create deviations from mean
    mutate(
      SelfRateSlider = factor(labels[SelfRateSlider], levels = labels),
      SelfRate = SelfRate - avgSelfRate
      ) %>%
    # plot
    ggplot(aes(x = SelfRate, y = fct_rev(SelfRateSlider))) +
    geom_boxplot(outlier.shape = NA) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Deviation from average\nslider rating", y = NULL) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/study1/sliders2.pdf", width = 7, height = 3)
  return(out)
}

# plot correlations between survey measures
plotSurveyCorrelations <- function(d1) {
  out <-
    d1 %>%
    transmute(
      PolIdeology = PolSlider,
      SDO = SDO,
      RWA = RWA,
      Religiosity = Religiosity,
      OpennessToExperience = Open,
      Conscientiousness = Consc,
      Extraversion = Extra,
      Agreeableness = Agree,
      Neuroticism = Neur,
      HonestyHumility = Honest,
      SVOAngle = SVOangle
      ) %>%
    cor(
      method = "spearman",
      use = "pairwise"
      ) %>%
    ggcorrplot(
      type = "lower", 
      legend.title = "Correlation",
      lab = TRUE,
      lab_size = 2.5
      )
  ggsave(out, filename = "figures/study1/surveyCors.pdf", width = 7, height = 7)
  return(out)
}

# table of comprehension rates
makeCompTable <- function(d1) {
  # games vector
  games <- c(
    "No Disadvantageous Inequity 1",
    "No Disadvantageous Inequity 2",
    "No Disadvantageous Inequity 3 (Computer)",
    "No Disadvantageous Inequity 4 (1:1 Fee Fine)",
    "Disadvantageous Inequity",
    "Third-Party"
  )
  # make table
  d1 %>%
    # from wide to long
    pivot_longer(
      cols = starts_with("fail"),
      names_to = "Game",
      values_to = "Comprehension"
    ) %>%
    # mutate game labels
    transmute(
      Game = ifelse(Game == "fail1", games[1], Game),
      Game = ifelse(Game == "fail2", games[2], Game),
      Game = ifelse(Game == "fail3", games[3], Game),
      Game = ifelse(Game == "fail4", games[4], Game),
      Game = ifelse(Game == "fail5", games[5], Game),
      Game = ifelse(Game == "fail6", games[6], Game),
      Game = factor(Game, levels = games),
      Comprehension = Comprehension
    ) %>%
    # summarise
    group_by(Game) %>%
    summarise(`Comprehension Rate` = round(mean(!Comprehension), 2))
}

# table of raw strategy counts
makeStrategyCountTable <- function(d1) {
  d1 %>%
    rename(Strategy = strategy) %>%
    group_by(Strategy) %>%
    summarise(N = n(), Proportion = round(n() / nrow(.), 3))
}

# table of behaviour patterns
makePatternsTable <- function(d1) {
  d1 %>%
    # concatenate patterns of behaviour into single string
    unite(
      col = "Pattern",
      starts_with("pun"),
      sep = ""
    ) %>%
    # get five most common strings
    group_by(Pattern) %>%
    summarise(N = n(), Proportion = round(n() / nrow(.), 2)) %>%
    arrange(desc(N)) %>%
    slice(1:5) %>%
    # explain the five most common strings
    mutate(
      Explanation = c(
        "Anti-punish strategy (exact)",
        "Avoid DI strategy (exact)",
        "Egalitarian strategy (exact)",
        "Only punish in third-party game",
        "Punish in No DI 2 and DI games"
      )
    ) %>%
    # organise columns for table
    dplyr::select(Pattern, Explanation, N, Proportion)
}

# plot model results
plotModel1 <- function(post) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice")
  # calculate probabilities
  P <- post$alpha
  for (i in 1:nrow(P)) P[i,] <- softmax(P[i,])
  # plot
  out <- 
    tibble(
      p = c(P[,1], P[,2], P[,3], P[,4], P[,5], 
            P[,6], P[,7], P[,8], P[,9]),
      strategy = rep(strategies, each = length(P[,1]))
    ) %>%
    mutate(strategy = factor(strategy, levels = strategies)) %>%
    ggplot(aes(x = p, y = fct_rev(strategy))) +
    geom_density_ridges(rel_min_height = 0.02, colour = "white", scale = 1) +
    stat_pointinterval() +
    labs(x = "Probability of using punishment strategy", y = NULL) +
    theme_classic()
  # save
  ggsave(out, filename = "figures/study1/model1.pdf", height = 5, width = 6)
  return(out)
}

# plot effect of predictor on strategy usage
plotModelPred <- function(d, post, pred, xlab, xBreaks, file) {
  # mean, sd, min, and max for predictor
  predMean <- d %>% pull(pred) %>% mean(na.rm = TRUE)
  predSD   <- d %>% pull(pred) %>% sd(na.rm = TRUE)
  predMin  <- d %>% pull(pred) %>% min(na.rm = TRUE)
  predMax  <- d %>% pull(pred) %>% max(na.rm = TRUE)
  # predictor sequence
  predSeq <- seq(
    d %>% pull(pred) %>% scale() %>% as.numeric() %>% min(na.rm = TRUE), 
    d %>% pull(pred) %>% scale() %>% as.numeric() %>% max(na.rm = TRUE), 
    length.out = 100
    )
  # posterior predictions
  postPred <- tibble()
  # median and 95% CIs for plotting
  for (i in 1:length(predSeq)) {
    # on logit scale
    p <- post$alpha + post$beta*predSeq[i]
    # on probability scale
    for (j in 1:nrow(p)) p[j,] <- softmax(p[j,])
    # add to post
    postPred <- bind_rows(postPred, tibble(predictor = predSeq[i], 
                                           strategy = 1:10, 
                                           med = apply(p, 2, median),
                                           lower = apply(p, 2, quantile, 0.025),
                                           upper = apply(p, 2, quantile, 0.975)))
  }
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  postPred$strategy <- strategies[postPred$strategy]
  postPred$strategy <- factor(postPred$strategy, levels = strategies)
  # data for geom_text (slopes)
  dataText <-
    tibble(
      strategy = factor(strategies, levels = strategies),
      label = paste0(
        "b = ", format(round(apply(post$beta, 2, median), 2), nsmall = 2),
        ", 95% CI [", format(round(apply(post$beta, 2, quantile, 0.025), 2), nsmall = 2),
        " ", format(round(apply(post$beta, 2, quantile, 0.975), 2), nsmall = 2), "]"
        )
      )
  # limits
  limits <- c(
    ifelse(xBreaks[1] <= predMin, xBreaks[1], predMin),
    ifelse(tail(xBreaks, n = 1) >= predMax, tail(xBreaks, n = 1), predMax)
  )
  # plot
  out <-
    ggplot(data = postPred) +
    geom_ribbon(aes(x = predictor, ymin = lower, ymax = upper), fill = "grey") +
    geom_line(aes(y = med, x = predictor)) +
    geom_text(data = dataText, aes(x = -Inf, y = -Inf, label = label), 
              hjust = -0.15, vjust = -27.5, size = 1.93) +
    facet_wrap(. ~ strategy, nrow = 2) +
    scale_y_continuous(name = "Probability of using strategy", limits = c(0, 1)) +
    scale_x_continuous(name = xlab, labels = function(x) round((x * predSD) + predMean, 0),
                       breaks = (xBreaks - predMean) / predSD,
                       limits = (limits - predMean) / predSD) +
    theme_classic()
  # save plot
  ggsave(out, filename = file, height = 4, width = 7)
  return(out)
}

# plot effect of categorical predictor on strategy usage
plotModelPredCat <- function(d, post, pred, xlab, file) {
  # posterior predictions
  postPred <- tibble()
  # categories
  categories <- d %>% drop_na(all_of(pred)) %>% pull(all_of(pred)) %>% as.factor() %>% levels()
  # median and 95% CIs for plotting
  for (i in 1:length(categories)) {
    # on logit scale
    p <- post$alpha[,i,]
    # on probability scale
    for (j in 1:nrow(p)) p[j,] <- softmax(p[j,])
    # add to post
    postPred <- bind_rows(postPred, tibble(predictor = categories[i], 
                                           strategy = 1:10, 
                                           med = apply(p, 2, median),
                                           lower = apply(p, 2, quantile, 0.025),
                                           upper = apply(p, 2, quantile, 0.975)))
  }
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  postPred$strategy <- strategies[postPred$strategy]
  postPred$strategy <- factor(postPred$strategy, levels = strategies)
  # reorder predictor for plot
  if (pred == "Gender")    postPred$predictor <- factor(postPred$predictor, levels = c("Male", "Female"))
  if (pred == "Ethnicity") postPred$predictor <- factor(postPred$predictor, levels = c("White", "Asian", "Black", "Mixed", "Other"))
  if (pred == "Student")   postPred$predictor <- factor(postPred$predictor, levels = c("Yes", "No"))
  if (pred == "Education") {
     postPred$predictor <- ifelse(postPred$predictor == "Diploma / other professional certificate", "Diploma", postPred$predictor)
     postPred$predictor <- factor(postPred$predictor, levels = c("High School", "Diploma", "Completed university", "Masters degree", "PhD or equivalent"))
  }
  # plot
  out <-
    ggplot(data = postPred, aes(x = predictor, y = med, ymin = lower, ymax = upper)) +
    geom_pointrange(size = 0.3) +
    facet_wrap(. ~ strategy, nrow = 2) +
    scale_y_continuous(name = "Probability of using strategy", limits = c(0, 1)) +
    scale_x_discrete(name = xlab) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  # save plot
  ggsave(out, filename = file, height = 4, width = 7)
  return(out)
}

# custom functions

# load UK/US data
loadData <- function(fileStudy1, fileStudy2) {
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
  # function to load data
  loadDataFun <- function(file, country = "") {
    read_csv(file) %>%
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
      ) %>%
      # add column for country
      mutate(Country = country)
  }
  # combine data from UK and US
  out <-
    loadDataFun(fileStudy1, country = "United Kingdom") %>%
    bind_rows(loadDataFun(fileStudy2, country = "United States"))
  return(out)
}

# data exclusions
excludeData <- function(d) {
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
  # n = 2024 (UK and US combined)
  d %>%
    # exclude participants who sped through surveys (criteria = 2 SDs below median duration)
    filter(DurationSeconds_Part1 > median(DurationSeconds_Part1, na.rm = TRUE) - sd(DurationSeconds_Part1, na.rm = TRUE)*2) %>%
    filter(ifelse(is.na(DurationSeconds_Part2), TRUE, 
                  DurationSeconds_Part2 > median(DurationSeconds_Part2, na.rm = TRUE) - sd(DurationSeconds_Part2, na.rm = TRUE)*2)) %>%
    # n = 2024
    # exclude participants who failed "days of week" attention check
    filter(is.na(DaysWeek_TryAgain) | DaysWeek_TryAgain == 7) %>%
    # n = 2024
    # exclude participants who failed "weeks in year" attention check
    filter(is.na(WeeksYear_TryAgain) | WeeksYear_TryAgain == 52) %>%
    # n = 2024
    # exclude participants who provide nonsensical answer to breakfast question
    slice(c(-21,-279,-335,-872,-912,-1253,-1565)) %>%
    # n = 2017
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
    # n = 2010
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

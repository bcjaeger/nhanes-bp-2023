#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author bcjaeger
#' @export
nhanes_load_hiq <- function() {

 fnames_hiq <- c(
  "2021_2023" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/HIQ_L.xpt",
  "2017_2020" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_HIQ.xpt",
  "2015_2016" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HIQ_I.xpt",
  "2013_2014" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/HIQ_H.xpt"
 )

 nhanes_hiq <- map(fnames_hiq, read_xpt)

 nhanes_hiq[c("2017_2020", "2021_2023")] %<>% map(
  ~ mutate(.x,
           HIQ031A = if_else(HIQ032A == 1, 14, HIQ032A),
           HIQ031B = if_else(HIQ032B == 2, 15, HIQ032B),
           HIQ031C = if_else(HIQ032C == 3, 16, HIQ032C),
           HIQ031D = if_else(HIQ032D == 4, 17, HIQ032D),
           HIQ031E = if_else(HIQ032E == 5, 18, HIQ032E),
           HIQ031F = NA_integer_,
           HIQ031G = NA_integer_,
           HIQ031H = if_else(HIQ032H == 8, 21, HIQ032H),
           HIQ031I = if_else(HIQ032I == 9, 22, HIQ032I))
 )

 nhanes_hiq$`2021_2023` %<>% mutate(
  HIQ031F = if_else(HIQ032F == 6, 19, HIQ032F),
 )

 output <- nhanes_hiq %>%
  map_dfr(
   select,
   SEQN,
   health_ins_none = HIQ011,
   health_ins_private = HIQ031A,
   health_ins_medicare = HIQ031B,
   health_ins_medicaid = HIQ031D,
   health_ins_othergovtc = HIQ031C,
   health_ins_othergovte = HIQ031E,
   health_ins_othergovtf = HIQ031F,
   health_ins_othergovtg = HIQ031G,
   health_ins_othergovth = HIQ031H,
   health_ins_othergovti = HIQ031I,
   .id = 'exam'
  ) %>%
  transmute(

   SEQN,

   exam,

   health_ins_none = case_when(
    health_ins_none == 2 ~ "Yes",
    health_ins_none %in% c(7,9) ~ NA_character_,
    is.na(health_ins_none) ~ NA_character_,
    TRUE ~ "No"
   ),

   health_ins_private = case_when(
    health_ins_private == 14 ~ "Yes",
    health_ins_private %in% c(77, 99) ~ NA_character_,
    TRUE ~ "No"
   ),

   health_ins_medicare = case_when(
    health_ins_medicare == 15 ~ "Yes",
    TRUE ~ "No"
   ),

   health_ins_medicaid = case_when(
    health_ins_medicaid == 17 ~ "Yes",
    TRUE ~ "No"
   ),

   health_ins_othergovt = case_when(
    health_ins_othergovtc == 16 ~ "Yes",
    health_ins_othergovte == 18 ~ "Yes",
    health_ins_othergovtf == 19 ~ "Yes",
    health_ins_othergovtg == 20 ~ "Yes",
    health_ins_othergovth == 21 ~ "Yes",
    health_ins_othergovti == 22 ~ "Yes",
    TRUE ~ "No"
   ),

   health_ins_none = if_else(
    health_ins_none == "No" &
     health_ins_private == "No" &
     health_ins_medicare == "No" &
     health_ins_medicaid == "No" &
     health_ins_othergovt == "No",
    true = NA_character_,
    false = health_ins_none
   )

  )

 tmp = output

 count(tmp, health_ins_none)

 # test_pm <- read_sas('data/test_newvars.sas7bdat') %>%
 #  select(SEQN,
 #         health_ins_none = none,
 #         health_ins_private = private,
 #         health_ins_medicare = medicare,
 #         health_ins_medicaid = medicaid,
 #         health_ins_othergovt = othergovt) %>%
 #  mutate(across(starts_with("health_ins"),
 #                ~ recode(.x, "0" = 'No', "1" = "Yes"))) %>%
 #  mutate(SEQN = factor(SEQN, levels = output$SEQN)) %>%
 #  arrange(SEQN) %>%
 #  select(-SEQN)

 # for(i in names(test_pm)){
 #  testthat::expect_equal( test_pm[[i]], output[[i]] )
 # }

 output

}





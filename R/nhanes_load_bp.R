#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author bcjaeger
#' @export
nhanes_load_bp <- function() {

 # blood pressure
 fnames_bp <- c(
  "2021_2023" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/BPXO_L.xpt",
  "2017_2020" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_BPXO.xpt",
  "2015_2016" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BPX_I.xpt",
  "2013_2014" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/BPX_H.xpt"
 )

 nhanes_bp <- map(fnames_bp, read_xpt)

 nhanes_bp[c('2021_2023', '2017_2020')] %<>%
  map(
   ~ select(.x,
            SEQN,
            bp_sys_1 = BPXOSY1,
            bp_dia_1 = BPXODI1,
            bp_sys_2 = BPXOSY2,
            bp_dia_2 = BPXODI2,
            bp_sys_3 = BPXOSY3,
            bp_dia_3 = BPXODI3)
  )

 nhanes_bp[c('2013_2014', '2015_2016')] %<>%
  map(
   ~ select(.x,
            SEQN,
            bp_sys_1 = BPXSY1,
            bp_dia_1 = BPXDI1,
            bp_sys_2 = BPXSY2,
            bp_dia_2 = BPXDI2,
            bp_sys_3 = BPXSY3,
            bp_dia_3 = BPXDI3,
            bp_sys_4 = BPXSY4,
            bp_dia_4 = BPXDI4)
  )

 bp_concat <- bind_rows(nhanes_bp, .id = 'exam')

 bp_long <- bp_concat %>%
  pivot_longer(starts_with("bp")) %>%
  separate(name, into = c("drop", 'type', 'msr'))

 # aggregate and calibrate BP

 bp_smry <- bp_long %>%
  select(-drop) %>%
  group_by(exam, SEQN, type) %>%
  drop_na(value) %>%
  filter(value > 0) %>%
  summarize(bp = mean(value)) %>%
  pivot_wider(names_from = type, values_from = bp) %>%
  ungroup() %>%
  mutate(

   dia_calib = case_when(
    exam %in% c("2017_2020", "2021_2023") ~ dia - 1.3,
    TRUE ~ dia
   ),

   sys_calib = case_when(
    exam %in% c("2017_2020", "2021_2023") ~ sys + 1.5,
    TRUE ~ sys
   )

  )

 # this dataset will be empty if there are no rows shared by nhanes_data
 # and bp_smry where the calibrated SBP and DBP are different.

 bp_check <- nhanes_data %>%
  select(SEQN = svy_id, bp_sys_mean, bp_dia_mean) %>%
  inner_join(bp_smry) %>%
  filter(
   abs(bp_sys_mean - sys_calib) > 1e-5 | abs(bp_dia_mean - dia_calib) > 1e-5
  )

 testthat::expect_equal(nrow(bp_check), 0)

 # return simplified calibrated data
 bp_smry %>%
  select(SEQN, exam,
         bp_sys_mean_uncalib = sys,
         bp_dia_mean_uncalib = dia,
         bp_sys_mean = sys_calib,
         bp_dia_mean = dia_calib)

}

nhanes_load_bpq <- function() {

 # blood pressure
 fnames_bpq <- c(
  "2021_2023" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/BPQ_L.xpt",
  "2017_2020" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_BPQ.xpt",
  "2015_2016" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BPQ_I.xpt",
  "2013_2014" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/BPQ_H.xpt"
 )



 nhanes_bpq <- map(fnames_bpq, read_xpt)

 nhanes_bpq_labs <- map(nhanes_bpq, .f = function(data){
  map_chr(data, ~attr(.x, 'label')) %>%
   enframe()
 })

 nhanes_bpq$`2021_2023` %<>%
  transmute(SEQN,
            bp_ever_told_high = BPQ020,
            bp_meds_now = BPQ150)

 nhanes_bpq[c("2017_2020", "2015_2016", "2013_2014")] %<>%
  map(
   ~ transmute(.x,
               SEQN,
               bp_ever_told_high = BPQ020,
               bp_ever_told_meds = BPQ040A,
               bp_meds_now = BPQ050A)
  )

 bpq_concat <- bind_rows(nhanes_bpq, .id = 'exam')

 # count(bpq_concat,
 #       exam,
 #       bp_ever_told_high,
 #       bp_ever_told_meds,
 #       bp_meds_now) %>%
 #  View()

 bpq_concat %<>%
  mutate(
   # recode
   across(starts_with("bp"),
          ~ case_when(.x == 1 ~ "Yes",
                      .x == 2 ~ "No",
                      .x == 7 ~ "Refused",
                      .x == 9 ~ "Dont_know",
                      TRUE ~ NA_character_)),
   bp_ever_told_meds = coalesce(bp_ever_told_meds, bp_ever_told_high),
   bp_meds_now = coalesce(bp_meds_now, bp_ever_told_meds)
  )

 # count(bpq_concat,
 #       exam,
 #       bp_ever_told_high,
 #       bp_ever_told_meds,
 #       bp_meds_now) %>%
 #  View()

 bpq_check <- nhanes_data %>%
  select(SEQN = svy_id, bp_meds_check = bp_med_use) %>%
  right_join(bpq_concat) %>%
  select(bp_meds_now, bp_meds_check) %>%
  filter(bp_meds_now != bp_meds_check)

 testthat::expect_equal(nrow(bpq_check), 0)

 bpq_concat %>%
  select(SEQN, exam,
         htn_aware = bp_ever_told_high,
         bp_med_use = bp_meds_now)


}

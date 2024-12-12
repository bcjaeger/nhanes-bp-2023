#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author bcjaeger
#' @export
nhanes_load_demo <- function() {

 fnames_demo <- c(
  "2021_2023" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/DEMO_L.xpt",
  "2017_2020" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_DEMO.xpt",
  "2015_2016" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.xpt",
  "2013_2014" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DEMO_H.xpt"
 )

 nhanes_demo <- map(fnames_demo, read_xpt) %>%
  map(select,
      SEQN,
      strata = SDMVSTRA,
      psu = SDMVPSU,
      starts_with("WTMEC"),
      interview_exam_status = RIDSTATR,
      gender = RIAGENDR,
      age_years = RIDAGEYR,
      race_ethnicity = RIDRETH3,
      education = DMDEDUC2,
      inc2pvt_ratio = INDFMPIR,
      pregnant = RIDEXPRG) %>%
  map_dfr(rename_with,
          .cols = starts_with('WTMEC'),
          .fn = ~ "wts_mec",
          .id = 'exam')

 nhanes_demo_recoded <- nhanes_demo %>%
  mutate(
   interview_exam_status = recode(interview_exam_status,
                                  "1" = 'interview_only',
                                  "2" = 'interview_exam'),
   gender = recode(gender,
                   "1" = "Men",
                   "2" = "Women"),
   race_ethnicity = recode(race_ethnicity,
                           "1" =	"Hispanic",
                           "2" =	"Hispanic",
                           "3" =	"Non-Hispanic White",
                           "4" =	"Non-Hispanic Black",
                           "6" =	"Non-Hispanic Asian",
                           "7" =	"Other including more than one race"),

   education = case_when(
    education %in% c(1, 2) ~ "Less than high school",
    education %in% c(3, 4) ~ "High school graduate",
    education == 5 ~ "College graduate"
   ),

   inc2pvt_ratio = cut(inc2pvt_ratio,
                       include.lowest = TRUE,
                       right = FALSE,
                       labels = c("<1", "1 to <2", "2 to <4", "4+"),
                       breaks = c(0, 1, 2, 4, Inf)),

   pregnant = recode(pregnant,
                     "1" = "Yes",
                     "2" = "No",
                     "3" = "No")
  )


 demo_check <- nhanes_data %>%
  select(SEQN = svy_id,
         svy_weight_mec,
         svy_psu,
         svy_strata,
         demo_race,
         demo_age_years,
         demo_pregnant,
         demo_gender) %>%
  mutate(
   demo_race = fct_recode(
    demo_race,
    "Other including more than one race" = "Other"
   )
  ) %>%
  inner_join(nhanes_demo_recoded, by = "SEQN") %>%
  filter(svy_weight_mec != wts_mec |
          svy_psu != psu |
          svy_strata != strata |
          demo_race != race_ethnicity |
          demo_age_years != age_years |
          demo_pregnant != pregnant |
          demo_gender != gender)

 testthat::expect_equal(nrow(demo_check), 0)

 nhanes_demo_recoded %>%
  mutate(pregnant = if_else(gender == "Men", "No", pregnant))

}


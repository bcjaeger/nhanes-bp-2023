#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author bcjaeger
#' @export
nhanes_load_huq <- function() {

 fnames_huq <- c(
  "2021_2023" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/HUQ_L.xpt",
  "2017_2020" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_HUQ.xpt",
  "2015_2016" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HUQ_I.xpt",
  "2013_2014" = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/HUQ_H.xpt"
 )



 nhanes_huq <- map(fnames_huq, read_xpt) %>%
  map_dfr(select,
          SEQN,
          usual_place_health = HUQ030,
          .id = 'exam')

 mutate(
  nhanes_huq,
  usual_place_health = case_when(
   usual_place_health %in% c(1,3) ~ "Yes",
   usual_place_health == 2 ~ "No"
  )
 )

}





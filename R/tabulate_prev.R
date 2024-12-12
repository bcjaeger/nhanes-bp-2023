#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author bcjaeger
#' @export
#'

# design = subset(nhanes_design$htn_jnc7, htn_aware == "Yes")
# standard_weights = nhanes_design$wts_htn
# outcome = "bp_med_use"

tabulate_prev <- function(design, standard_weights, outcome) {

 variables <- c(
  "Overall" = "1",
  "Age group, years" = "age_cat",
  "Sex" = "gender",
  "Race-ethnicity" = "race_ethnicity",
  "Education" = "education",
  "Income to poverty ratio" = "inc2pvt_ratio",
  "Routine place to go for healthcare" = "usual_place_health",
  "Health insurance" = "health_ins"
 )

 formula_outcome <- as_svy_formula(outcome)

 designs <- variables %>%
  map(.f = ~ design %>%
       design_standardize(time = 'exam',
                          group = .x,
                          standard_variable = "age_cat",
                          standard_weights = standard_weights))

 # debugging
 # .x <- designs$`Race-ethnicity`
 # .y <- variables["Race-ethnicity"]

 results <- designs %>%
  map2_dfr(variables,
           .id = 'label',
           .f = ~{

            pnt_est <- svyby(formula_outcome,
                             by = as_svy_formula(c("exam", .y)),
                             design = .x,
                             FUN = svymean)

            if(.y == "1"){
             into <- c("exam", outcome)
             join_by <- "exam"
             sep = '\\:'
            } else {
             into <- c("exam", .y, outcome)
             join_by <- as.character(c("exam", .y))
             sep = '\\.|\\:'
            }

            ci <- confint(pnt_est) %>%
             as_tibble(rownames = 'splitme') %>%
             rename(ci_lower = `2.5 %`,
                    ci_upper = `97.5 %`) %>%
             separate(splitme,
                      into = into,
                      sep = sep) %>%
             pivot_wider(names_from = !!outcome,
                         values_from = starts_with("ci"))

            output <- left_join(pnt_est, ci, by = join_by) %>%
             rename_with(.fn = ~str_replace(.x,
                                            pattern = outcome,
                                            replacement = 'outcome_')) %>%
             mutate(
              suppress = suppress_catg(
               estimate = outcome_Yes,
               std_error = se.outcome_Yes,
               ci_upper = ci_upper_outcome_Yes,
               ci_lower = ci_lower_outcome_Yes,
               design = .x
              )
             )

            if(.y == "1"){
             mutate(output, variable = "Overall", level = "Overall")
            } else {
             mutate(output, variable = .y, .after = exam) %>%
              rename(level = !!.y)
            }


           })



 pvals_trend <- map_dfr(
  .x = variables,
  .f = ~ {

   subsets <- levels(design$variables[[.x]])

   if(is.null(subsets))
    return(p_trend(outcome, design = design) %>%
            mutate(level = "Overall"))

   design$variables$tmp <- design$variables[[.x]]

   subsets %>%
    set_names() %>%
    map_dfr(
     .f = function(..x) suppressWarnings(
      p_trend(outcome, design = subset(design, tmp == ..x))
     ),
     .id = 'level'
    )

  },
  .id = 'label') %>%
  distinct() %>%
  select(-variable)

 pvals_precov <- map_dfr(
  variables,
  .f = ~ {

   .design <- design %>%
    subset(exam != c("2021_2023"))

   subsets <- levels(.design$variables[[.x]])

   if(is.null(subsets))
    return(suppressWarnings(p_trend(outcome, design = .design)) %>%
            mutate(level = "Overall"))

   .design$variables$tmp <- .design$variables[[.x]]

   subsets %>%
    set_names() %>%
    map_dfr(
     .f = function(..x) suppressWarnings(
      p_trend(outcome, design = subset(.design, tmp == ..x))
     ),
     .id = 'level'
    )

  },
  .id = 'label') %>%
  distinct() %>%
  select(-variable) %>%
  rename(p_precov = p_trend)


 # debugging
 # .x <- designs %>%
 #  map(subset, exam %in% c("2017_2020", "2021_2023")) %>%
 #  .[["Race-ethnicity"]]
 #
 # .y <- "race_ethnicity"


 pvals_prepost <- map_dfr(
  variables,
  .f = ~ {

   .design <- subset(design, exam %in% c("2017_2020", "2021_2023"))

   subsets <- levels(.design$variables[[.x]])

   if(is.null(subsets))
    return(suppressWarnings(p_trend(outcome, design = .design)) %>%
            mutate(level = "Overall"))

   .design$variables$tmp <- .design$variables[[.x]]

   subsets %>%
    set_names() %>%
    map_dfr(
     .f = function(..x) suppressWarnings(
      p_trend(outcome, design = subset(.design, tmp == ..x))
     ),
     .id = 'level'
    )

  },
  .id = 'label') %>%
  distinct() %>%
  select(-variable) %>%
  rename(p_prepost = p_trend)

 suppress_data <- results %>%
  mutate(exam = str_replace(exam, "_", "-")) %>%
  select(label, exam, level, suppress) %>%
  pivot_wider(names_from = exam,
              values_from = suppress) %>%
  mutate(label = if_else(level == "Overall", NA, label)) %>%
  as_grouped_data(groups = 'label') %>%
  remove_empty(which = 'rows')

 tbl_data <- results %>%
  mutate(exam = str_replace(exam, "_", "-")) %>%
  mutate(
   across(contains("outcome"), ~ .x * 100),
   value = table_glue(
    "{outcome_Yes}\n({ci_lower_outcome_Yes}, {ci_upper_outcome_Yes })"
   )
   # useful to double check if the suppression tags are hitting the right cell
   # value = if_else(str_detect(suppress, "suppress"),
   #                 paste0(value, ";;"),
   #                 value)
  ) %>%
  select(label, exam, level, value) %>%
  pivot_wider(names_from = exam,
              values_from = value) %>%
  left_join(pvals_trend) %>%
  left_join(pvals_precov) %>%
  left_join(pvals_prepost) %>%
  mutate(label = if_else(level == "Overall", NA, label)) %>%
  as_grouped_data(groups = 'label') %>%
  remove_empty(which = 'rows')

 ft <- as_flextable(tbl_data, hide_grouplabel = TRUE) %>%
  flextable_polish() %>%
  set_header_labels(level = "Characteristic",
                    p_trend = "2013-2014 through 2021-2023",
                    p_precov = "2013-2014 through 2017-2020",
                    p_prepost = "2021-2023 versus 2017-2020") %>%
  add_header_row(values = c("Characteristic",
                            "Calendar period",
                            "P-values for change over time"),
                 colwidths = c(1, 4, 3)) %>%
  merge_v(part = "header") %>%
  footnote(
   i = ~label == "Education",
   j = 1,
   value = as_paragraph(
    "Education status was reported among participants aged 20 years and older."
   ),
   ref_symbols = "*"
  ) %>%
  footnote(
   i = ~level == "High school graduate",
   j = 1,
   value = as_paragraph(
    paste("High school graduate includes participants who reported",
          "graduating high school with or without some college education")
   ),
   ref_symbols = "†"
  ) %>%
  bg(i = ~!is.na(label), bg = 'grey90') %>%
  padding(i = ~is.na(label), part = 'body', j = 1, padding.left = 10)

 cols_to_suppress <-
  names(tbl_data)[str_which(names(tbl_data), "\\d{4}-\\d{4}")]

 for(j in cols_to_suppress){

  if(any(str_detect(suppress_data[[j]], "suppress"), na.rm = TRUE)){

   i_ci <- which(str_detect(suppress_data[[j]], "ci_width_gteq_30"))
   i_neff <- which(str_detect(suppress_data[[j]], "n_eff_lt_30"))
   i_rel <- which(str_detect(suppress_data[[j]], "relative_ci_width_gt_130"))

   if(!is_empty(i_ci)){
    ft <- ft %>%
     footnote(
      i = i_ci,
      j = j,
      part = 'body',
      ref_symbols = '‡',
      value = as_paragraph(
       "Flagged for reliability: confidence interval width > 30"
      )
     )
   }

   if(!is_empty(i_neff)){
    ft <- ft %>%
     footnote(
      i = i_neff,
      j = j,
      part = 'body',
      ref_symbols = '§',
      value = as_paragraph(
       "Flagged for reliability: effective sample size < 30"
      )
     )
   }

   if(!is_empty(i_rel)){
    ft <- ft %>%
     footnote(
      i = i_rel,
      j = j,
      ref_symbols = '¶',
      value = as_paragraph(
       "Flagged for reliability: relative confidence interval width > 130%"
      )
     )
   }

  }

 }

 ft

}

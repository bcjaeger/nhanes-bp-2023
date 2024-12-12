
source("packages.R")
source("conflicts.R")
source("globals.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

library(crew)

tar_option_set(
 controller = crew_controller_local(workers = 8)
)

tar_option_set(tidy_eval = FALSE)

manuscript_version <- 8

if(!dir.exists(glue("doc/manuscript-v{manuscript_version}"))){
  dir.create(glue("doc/manuscript-v{manuscript_version}"))
}

list(

 # data targets ----

 tar_target(nhanes_raw, command = {
  list(nhanes_load_demo(),
       nhanes_load_bp(),
       nhanes_load_bpq(),
       nhanes_load_huq(),
       nhanes_load_hiq()) %>%
   reduce(left_join, by = c("SEQN", "exam"))
 }),

 tar_target(nhanes_init, command = {

  nhanes_raw %>%
   mutate(
    age_cat = cut(
     age_years,
     breaks = c(18, 45, 65, 75, Inf),
     right = FALSE,
     include.lowest = TRUE,
     labels = c("18 to 44", "45 to 64", "65 to 74", "≥ 75")
    ),

    # all of these are based on calibrated bp
    htn_jnc7 = if_else(
     bp_sys_mean >= 140 | bp_dia_mean >= 90 | bp_med_use == "Yes",
     true = "Yes",
     false = "No"
    ),
    htn_accaha = if_else(
     bp_sys_mean >= 130 | bp_dia_mean >= 80 | bp_med_use == "Yes",
     true = "Yes",
     false = "No"
    ),
    bp_control_jnc7 = if_else(
     bp_sys_mean < 140 & bp_dia_mean < 90,
     true = "Yes",
     false = "No"
    ),
    bp_control_accaha = if_else(
     bp_sys_mean < 130 & bp_dia_mean < 80,
     true = "Yes",
     false = "No"
    ),

    htn_jnc7_uncalib = if_else(
     bp_sys_mean_uncalib>=140 | bp_dia_mean_uncalib>=90 | bp_med_use == "Yes",
     true = "Yes",
     false = "No"
    ),
    htn_accaha_uncalib = if_else(
     bp_sys_mean_uncalib>=130 | bp_dia_mean_uncalib>=80 | bp_med_use == "Yes",
     true = "Yes",
     false = "No"
    ),
    bp_control_jnc7_uncalib = if_else(
     bp_sys_mean_uncalib < 140 & bp_dia_mean_uncalib < 90,
     true = "Yes",
     false = "No"
    ),
    bp_control_accaha_uncalib = if_else(
     bp_sys_mean_uncalib < 130 & bp_dia_mean_uncalib < 80,
     true = "Yes",
     false = "No"
    ),

    health_ins = case_when(
     health_ins_none == "Yes" ~ "None",
     health_ins_medicare == "Yes" ~ "Medicare",
     health_ins_private == "Yes" ~ "Private",
     health_ins_othergovt == "Yes" ~ "Other government insurance",
     health_ins_medicaid == "Yes" ~ "Medicaid",
     is.na(health_ins_none) ~ NA_character_

    ),

    across(where(is.character), as.factor),

    health_ins = fct_relevel(health_ins,
                             "None", "Private",
                             "Medicare","Medicaid",
                             "Other government insurance"),

    education = fct_relevel(education, "Less than high school",
                            "High school graduate", "College graduate"),

    race_ethnicity = fct_relevel(race_ethnicity,
                                 "Non-Hispanic White",
                                 "Non-Hispanic Black",
                                 "Non-Hispanic Asian",
                                 "Hispanic",
                                 "Other including more than one race"),

    bp_cat_5 = case_when(
     bp_sys_mean < 120  & bp_dia_mean  < 80  ~ "<120/80 mm Hg",
     bp_sys_mean < 130  & bp_dia_mean  < 80  ~ "120-129/<80 mm Hg",
     bp_sys_mean < 140  & bp_dia_mean  < 90  ~ "130-139/80-89 mm Hg",
     bp_sys_mean < 160  & bp_dia_mean  < 100 ~ "140-159/90-99 mm Hg",
     bp_sys_mean >= 160 | bp_dia_mean >= 100 ~ "≥ 160/100 mm Hg"
    ),

    bp_cat_5 = fct_relevel(bp_cat_5,
                           "<120/80 mm Hg",
                           "120-129/<80 mm Hg",
                           "130-139/80-89 mm Hg",
                           "140-159/90-99 mm Hg",
                           "≥ 160/100 mm Hg"),

    bp_cat_4 = fct_recode(bp_cat_5,
                          "≥140/90 mm Hg" = "140-159/90-99 mm Hg",
                          "≥140/90 mm Hg" = "≥ 160/100 mm Hg"),

    bp_cat_3 = fct_recode(bp_cat_5,
                          "≥130/80 mm Hg" = "130-139/80-89 mm Hg",
                          "≥130/80 mm Hg" = "140-159/90-99 mm Hg",
                          "≥130/80 mm Hg" = "≥ 160/100 mm Hg")
   )
 }),

 tar_target(nhanes_excluded, command = {

  tidy_counts <- function(data, exclusion_label){
   total <- nrow(data)
   by_cyc <- count(data, exam)
   pivot_wider(by_cyc, values_from = n, names_from = exam) %>%
    mutate(overall = total, .before = 1) %>%
    mutate(label = exclusion_label, .before = 1)
  }

  # Completed the interview and examination
  nhanes_exclude_1 <- nhanes_init %>%
   filter(interview_exam_status == 'interview_exam')

  nhanes_exclude_2 <- nhanes_exclude_1 %>%
   filter(age_years >= 18)

  nhanes_exclude_3 <- nhanes_exclude_2 %>%
   filter(pregnant == "No" | is.na(pregnant))

  nhanes_exclude_4 <- nhanes_exclude_3 %>%
   drop_na(bp_sys_mean, bp_dia_mean)

  nhanes_exclude_5 <- nhanes_exclude_4 %>%
   filter(bp_med_use %in% c("Yes", "No"))

  counts <- bind_rows(
   tidy_counts(nhanes_init, "NHANES participants from 2013-2023"),
   tidy_counts(nhanes_exclude_1, "Completed the interview and examination"),
   tidy_counts(nhanes_exclude_2, "Age 18 years or older"),
   tidy_counts(nhanes_exclude_3, "Not pregnant at time of exam"),
   tidy_counts(nhanes_exclude_4, "At least one systolic and diastolic blood pressure measurement"),
   tidy_counts(nhanes_exclude_5, "Complete information on antihypertensive medication use")
  )

  write_csv(counts, 'doc/exclusions.csv')

  # remove empty categories in case they were created w/an exclusion
  droplevels(nhanes_exclude_5)

 }),

 tar_target(nhanes_design, command = {

  design_overall <- svydesign(ids = ~ psu,
                              strata = ~ strata,
                              weights = ~ wts_mec,
                              data = nhanes_init,
                              nest = TRUE) %>%
   subset(age_years >= 18 &
          interview_exam_status == 'interview_exam' &
          (pregnant == "No" | is.na(pregnant)) &
          !is.na(bp_sys_mean) & !is.na(bp_dia_mean) &
          bp_med_use %in% c("Yes", "No")
   )

  # see above
  design_overall$variables %<>% droplevels()

  testthat::expect_equal(
   as_tibble(design_overall$variables),
   as_tibble(nhanes_excluded)
  )

  # designs specifically for htn with jnc7 and accaha, separately
  design_htn_jnc7 <- subset(design_overall, htn_jnc7 == "Yes")
  design_htn_accaha <- subset(design_overall, htn_accaha == "Yes")

  age_weights_overall <-
   svytable(~age_cat, design = design_overall) %>%
   as.data.frame() %>%
   mutate(p = Freq / sum(Freq)) %>%
   select(age_cat, p) %>%
   deframe()

  age_weights_htn <-
   svytable(~age_cat, design = design_htn_jnc7) %>%
   as.data.frame() %>%
   mutate(p = Freq / sum(Freq)) %>%
   select(age_cat, p) %>%
   deframe()

  list(overall = design_overall,
       htn_jnc7 = design_htn_jnc7,
       htn_accaha = design_htn_accaha,
       wts_overall = age_weights_overall,
       wts_htn = age_weights_htn)

 }),

 # table targets ----

 tar_target(tbl_characteristics, command = {

  variables <- c(
   "Age group, years" = "age_cat",
   "Sex" = "gender",
   "Race-ethnicity" = "race_ethnicity",
   "Education" = "education",
   "Income to poverty ratio" = "inc2pvt_ratio",
   "Routine place to go for healthcare" = "usual_place_health",
   "Health insurance" = "health_ins"
  )

  tbl <- variables %>%
   map(tabulate_characteristics,
       by = 'exam',
       design = nhanes_design$overall,
       prop_confint = TRUE) %>%
   bind_rows(.id = 'label') %>%
   mutate(label = factor(label, levels = names(variables))) %>%
   arrange(label)

  # pvals_trend <- variables %>%
  #  map_dfr(p_trend,
  #          design = nhanes_design$overall,
  #          .id = 'label') %>%
  #  select(-variable)
  #
  # pvals_precov <- variables %>%
  #  map_dfr(p_trend,
  #          design = subset(nhanes_design$overall,
  #                         exam != "2021_2023"),
  #          .id = 'label') %>%
  #  rename(p_precov = p_trend) %>%
  #  select(-variable)
  #
  # pvals_prepost <- variables %>%
  #  map_dfr(p_trend,
  #          design = subset(nhanes_design$overall,
  #                          exam %in% c("2017_2020", "2021_2023")),
  #          .id = 'label') %>%
  #  rename(p_prepost = p_trend) %>%
  #  select(-variable)

  tb1_counts_raw <- nhanes_design$overall %>%
   getElement('variables') %>%
   count(exam) %>%
   mutate(
    n = table_glue("{exam}\n(n = {n})")
   ) %>%
   deframe()

  tbl %>%
   mutate(exam = recode(exam, !!!tb1_counts_raw),
          exam = str_replace(exam, "_", "-")) %>%
   mutate(value = if_else(is_suppressed(suppress_status),
                          true = "--",
                          false = value)) %>%
   select(label, exam, level, value) %>%
   pivot_wider(names_from = exam,
               values_from = value) %>%
   filter(level != "No" | label == "Routine place to go for healthcare") %>%
   mutate(level = if_else(level == "Yes" & label %in% c("None",
                                                        "Private",
                                                        "Medicare",
                                                        "Medicaid",
                                                        "Other government insurance"),
                          label, level),
          label = recode(label,
                         "None" = "Health insurance",
                         "Private" = "Health insurance",
                         "Medicare" = "Health insurance",
                         "Medicaid" = "Health insurance",
                         "Other government insurance" = "Health insurance")) %>%
   # left_join(pvals_trend) %>%
   # left_join(pvals_precov) %>%
   # left_join(pvals_prepost) %>%
   as_grouped_data(groups = 'label') %>%
   as_flextable(hide_grouplabel = TRUE) %>%
   flextable_polish() %>%
   set_header_labels(
    level = "Characteristic"
    # p_trend = "2013-2014 through 2021-2023",
    # p_precov = "2013-2014 through 2017-2020",
    # p_prepost = "2021-2023 versus 2017-2020"
   ) %>%
   add_header_row(
    values = c(
     "Characteristic",
     "Calendar period"
     # "P-values for change over time"
    ),
    colwidths = c(
     1,
     4
     # 3
    )
   ) %>%
   merge_v(part = 'header') %>%
   bg(i = ~!is.na(label), bg = 'grey90') %>%
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
   padding(i = ~is.na(label), part = 'body', j = 1, padding.left = 10)


 }),

 tar_target(tbl_bp_means, command = {

  variables <- c(
   "Systolic BP, mm Hg" = "bp_sys_mean",
   "Diastolic BP, mm Hg" = "bp_dia_mean",
   "BP category, %" = "bp_cat_4"
  )

  tbl <- variables %>%
   map(tabulate_characteristics,
       by = 'exam',
       design = nhanes_design$overall,
       prop_confint = TRUE) %>%
   bind_rows(.id = 'label') %>%
   mutate(label = factor(label, levels = names(variables))) %>%
   arrange(label)

  pvals_trend <- variables %>%
   map_dfr(p_trend,
           design = nhanes_design$overall,
           age_adjust = FALSE,
           .id = 'label') %>%
   select(-variable) %>%
   mutate(level = if_else(is.na(level), label, level),
          label = if_else(label == level, NA, label))

  pvals_precov <- variables %>%
   map_dfr(p_trend,
           design = subset(nhanes_design$overall,
                           exam != "2021_2023"),
           age_adjust = FALSE,
           .id = 'label')  %>%
   rename(p_precov = p_trend) %>%
   select(-variable) %>%
   mutate(level = if_else(is.na(level), label, level),
          label = if_else(label == level, NA, label))

  pvals_prepost <- variables %>%
   map_dfr(p_trend,
           design = subset(nhanes_design$overall,
                           exam %in% c("2017_2020", "2021_2023")),
           age_adjust = FALSE,
           .id = 'label') %>%
   rename(p_prepost = p_trend) %>%
   select(-variable) %>%
   mutate(level = if_else(is.na(level), label, level),
          label = if_else(label == level, NA, label))

  tbl %>%
   mutate(exam = str_replace(exam, "_", "-")) %>%
   mutate(value = if_else(is_suppressed(suppress_status),
                          true = "--",
                          false = value),
          level = if_else(ctns, label, level),
          label = if_else(ctns, NA, label)) %>%
   select(label, exam, level, value) %>%
   pivot_wider(names_from = exam,
               values_from = value) %>%
   left_join(pvals_trend) %>%
   left_join(pvals_precov) %>%
   left_join(pvals_prepost) %>%
   as_grouped_data(groups = 'label') %>%
   remove_empty(which = 'rows') %>%
   as_flextable(hide_grouplabel = TRUE) %>%
   flextable_polish() %>%
   set_header_labels(level = "Characteristic",
                     p_trend = "2013-2014 through 2021-2023",
                     p_precov = "2013-2014 through 2017-2020",
                     p_prepost = "2021-2023 versus 2017-2020") %>%
   add_header_row(values = c("Characteristic",
                             "Calendar period",
                             "P-values for change over time"),
                  colwidths = c(1, 4, 3)) %>%
   merge_v(part = 'header') %>%
   bg(i = ~!is.na(label), bg = 'grey90') %>%
   compose(i = ~str_detect(label, 'BP category'),
           value = as_paragraph("BP category, %")) %>%
   padding(i = ~is.na(label), part = 'body', j = 1, padding.left = 10)

 }),

 tar_target(tbl_htn_prev, command = {

  outcomes <- c("htn_jnc7",
                "htn_accaha",
                "htn_jnc7_uncalib",
                "htn_accaha_uncalib") %>%
   set_names(str_remove(., "^htn_"))

  map(
   .x = outcomes,
   .f = ~ tabulate_prev(design = nhanes_design$overall,
                        standard_weights = nhanes_design$wts_overall,
                        outcome = .x)
  )

 }),

 tar_target(tbl_htn_aware_prev, command = {

  jnc7 <- tabulate_prev(design = nhanes_design$htn_jnc7,
                        standard_weights = nhanes_design$wts_htn,
                        outcome = "htn_aware")

  accaha <- tabulate_prev(design = nhanes_design$htn_accaha,
                          standard_weights = nhanes_design$wts_htn,
                          outcome = "htn_aware")

  list(jnc7 = jnc7, accaha = accaha)


 }),

 tar_target(tbl_htn_aware_meds_prev, command = {

  jnc7 <- tabulate_prev(design = subset(nhanes_design$htn_jnc7, htn_aware == "Yes"),
                standard_weights = nhanes_design$wts_htn,
                outcome = "bp_med_use")

  accaha <- tabulate_prev(design = subset(nhanes_design$htn_accaha, htn_aware == "Yes"),
                        standard_weights = nhanes_design$wts_htn,
                        outcome = "bp_med_use")

  list(jnc7 = jnc7, accaha = accaha)


 }),

 tar_target(tbl_bp_control_prev, command = {

  designs <- nhanes_design[c("htn_jnc7", "htn_accaha")] %>%
   enframe(value = 'design') %>%
   mutate(name = str_remove(name, "^htn_"))

  map_guide <- c("bp_control_jnc7",
                 "bp_control_accaha",
                 "bp_control_jnc7_uncalib",
                 "bp_control_accaha_uncalib") %>%
   set_names(str_remove(., "^bp_control_")) %>%
   enframe() %>%
   mutate(label = name,
          name = str_remove(name, '_uncalib$')) %>%
   left_join(designs, by = 'name')

  map2(
   .x = map_guide$value,
   .y = map_guide$design,
   .f = ~ tabulate_prev(design = .y,
                        standard_weights = nhanes_design$wts_htn,
                        outcome = .x)
  ) %>%
   set_names(map_guide$label)




 }),

 tar_target(tbl_bp_control_meds_prev, command = {

  designs <- nhanes_design[c("htn_jnc7", "htn_accaha")] %>%
   enframe(value = 'design') %>%
   mutate(name = str_remove(name, "^htn_"))

  map_guide <- c("bp_control_jnc7",
                 "bp_control_accaha",
                 "bp_control_jnc7_uncalib",
                 "bp_control_accaha_uncalib") %>%
   set_names(str_remove(., "^bp_control_")) %>%
   enframe() %>%
   mutate(label = name,
          name = str_remove(name, '_uncalib$')) %>%
   left_join(designs, by = 'name')

  map2(
   .x = map_guide$value,
   .y = map_guide$design,
   .f = ~ tabulate_prev(design = subset(.y, bp_med_use == "Yes"),
                        standard_weights = nhanes_design$wts_htn,
                        outcome = .x)
  ) %>%
   set_names(map_guide$label)


 }),

 tar_target(tbl_prevalence_ratios, command = {

  design <- subset(nhanes_design$htn_jnc7, exam == '2021_2023')

  outcomes <- c('htn_aware', "bp_med_use",
                "bp_control_jnc7", "bp_control_jnc7")

  subset <- c("htn", "htn", "htn", "htn_med_use")

  for(i in unique(outcomes)){
   design$variables[[i]] %<>% as.numeric() %>% subtract(1)
  }

  design_meds <- subset(design, bp_med_use == 1)

  design_grid <- list(design, design, design, design_meds)

  variables <- c(
   "Age group, years" = "age_cat",
   "Sex" = "gender",
   "Race-ethnicity" = "race_ethnicity",
   "Education" = "education",
   "Income to poverty ratio" = "inc2pvt_ratio",
   "Routine place to go for healthcare" = "usual_place_health",
   "Health insurance" = "health_ins"
   # "Private health insurance" = "health_ins_private",
   # "Medicare health insurance" = "health_ins_medicare",
   # "Medicaid health insurance" = "health_ins_medicaid",
   # "Other government health insurance" = "health_ins_othergovt"
  )

  ref_levels <- variables %>%
   set_names(variables) %>%
   map_chr(~levels(nhanes_design$overall$variables[[.x]])[1]) %>%
   enframe(name = 'variable', value = 'term')

  results <- tibble(outcome = outcomes,
                    subset = subset,
                    design = design_grid) %>%
   cross_join(tibble(variable = variables)) %>%
   mutate(
    prev_ratios = pmap(
     .l = list(variable, outcome, design),
     .f = function(variable, outcome, design){

      m1 <- c("age_cat", "gender", "race_ethnicity")

      m2 <- c(m1,
              "education",
              "inc2pvt_ratio",
              "usual_place_health",
              "health_ins")

      formulas <- list(m1 = m1, m2 = m2) %>%
       map(setdiff, variable) %>%
       map(glue_collapse, " + ") %>%
       map(~paste(outcome, .x, sep = '~')) %>%
       map(~paste(.x, variable, sep = " + ")) %>%
       map(as.formula)

      map_dfr(formulas, .f = ~ {


       fit <- svyglm(formula = .x,
                     family = quasipoisson(),
                     design = design)

       df_resid <- fit$df.residual

       if(df_resid < 0){
        df_resid <- degf(nhanes_design$htn_jnc7)
       }

       ci <- confint(fit, ddf = df_resid) %>%
        as_tibble(rownames = 'term') %>%
        rename(lwr = `2.5 %`,
               upr = `97.5 %`)

       summary(fit) %>%
        getElement("coefficients") %>%
        as_tibble(rownames = 'term') %>%
        select(term, est = Estimate) %>%
        left_join(ci, by = 'term') %>%
        filter(str_detect(term, variable))

      },
      .id = 'model')

     }
    ))

  tbl_results <- unnest(results, prev_ratios) %>%
   mutate(term = str_remove(term, variable),
          across(.cols = c(est, lwr, upr),
                 .fns = exp)) %>%
   mutate(tbv = table_glue("{est}\n({lwr}, {upr})"), .keep = 'unused')

  variable_recoder <- names(variables) %>%
   set_names(variables)

  tbl_awareness_meds <- tbl_results %>%
   filter(outcome %in% c("htn_aware", "bp_med_use")) %>%
   select(-design, -subset) %>%
   pivot_wider(names_from = c(outcome, model), values_from = tbv) %>%
   split(.$variable) %>%
   map(~ bind_rows(filter(ref_levels, variable == .x$variable[1]), .x)) %>%
   .[variables] %>%
   bind_rows() %>%
   mutate(across(everything(), ~ if_else(is.na(.x), "1 (reference)", .x))) %>%
   mutate(variable = recode(variable, !!!variable_recoder)) %>%
   as_grouped_data(groups = 'variable') %>%
   as_flextable(hide_grouplabel = TRUE) %>%
   flextable_polish() %>%
   footnote(
    i = ~variable == "Education",
    j = 1,
    value = as_paragraph(
     "Education status was reported among participants aged 20 years and older."
    ),
    ref_symbols = "*"
   ) %>%
   footnote(
    i = ~term == "High school graduate",
    j = 1,
    value = as_paragraph(
     paste("High school graduate includes participants who reported",
           "graduating high school with or without some college education")
    ),
    ref_symbols = "†"
   ) %>%
   bg(i = ~!is.na(variable), bg = 'grey90') %>%
   padding(i = ~is.na(variable), part = 'body', j = 1, padding.left = 10) %>%
   set_header_labels(
    term = "Characteristic",
    htn_aware_m1 = "PR (95% CI) Model 1",
    htn_aware_m2 = "PR (95% CI) Model 2",
    bp_med_use_m1 = "PR (95% CI) Model 1",
    bp_med_use_m2 = "PR (95% CI) Model 2"
   ) %>%
   add_header_row(values = c("Characteristic", "Hypertension awareness",
                             "Taking antihypertensive medications"),
                  colwidths = c(1, 2, 2)) %>%
   merge_v(part = 'header')

  tbl_bp_control <- tbl_results %>%
   filter(outcome %in% c("bp_control_jnc7")) %>%
   mutate(outcome = paste(outcome, subset, sep = '_'), .keep='unused') %>%
   select(-design) %>%
   pivot_wider(names_from = c(outcome, model), values_from = tbv) %>%
   split(.$variable) %>%
   map(~ bind_rows(filter(ref_levels, variable == .x$variable[1]), .x)) %>%
   .[variables] %>%
   bind_rows() %>%
   mutate(across(everything(), ~ if_else(is.na(.x), "1 (reference)", .x))) %>%
   mutate(variable = recode(variable, !!!variable_recoder)) %>%
   as_grouped_data(groups = 'variable') %>%
   as_flextable(hide_grouplabel = TRUE) %>%
   flextable_polish() %>%
   footnote(
    i = ~variable == "Education",
    j = 1,
    value = as_paragraph(
     "Education status was reported among participants aged 20 years and older."
    ),
    ref_symbols = "*"
   ) %>%
   footnote(
    i = ~term == "High school graduate",
    j = 1,
    value = as_paragraph(
     paste("High school graduate includes participants who reported",
           "graduating high school with or without some college education")
    ),
    ref_symbols = "†"
   ) %>%
   bg(i = ~!is.na(variable), bg = 'grey90') %>%
   padding(i = ~is.na(variable), part = 'body', j = 1, padding.left = 10) %>%
   set_header_labels(
    term = "Characteristic",
    bp_control_jnc7_htn_m1 = "PR (95% CI) Model 1",
    bp_control_jnc7_htn_m2 = "PR (95% CI) Model 2",
    bp_control_jnc7_htn_med_use_m1 = "PR (95% CI) Model 1",
    bp_control_jnc7_htn_med_use_m2 = "PR (95% CI) Model 2"
   ) %>%
   add_header_row(values = c("Characteristic",
                             "Among all US adults with hypertension",
                             "Among US adults taking antihypertensive medication"),
                  colwidths = c(1, 2, 2)) %>%
   merge_v(part = 'header')

  list(awareness_meds = tbl_awareness_meds,
       bp_control = tbl_bp_control)


 }),

 tar_target(tbl_prevalence_ratios_by_cycle, command = {

  design <- nhanes_design$htn_jnc7

  outcomes <- c("bp_control_jnc7", "bp_control_jnc7")

  subset <- c("htn", "htn_med_use")

  for(i in unique(outcomes)){
   design$variables[[i]] %<>% as.numeric() %>% subtract(1)
  }

  design$variables %<>% mutate(
   across(where(is.factor),
          ~factor(.x,
                  levels = levels(.x),
                  labels = paste("..", levels(.x), sep = '')))
  )

  design_meds <- subset(design, bp_med_use == "..Yes")

  design_grid <- list(design, design_meds)

  variables <- c(
   "Age group, years" = "age_cat",
   "Sex" = "gender",
   "Race-ethnicity" = "race_ethnicity",
   "Education" = "education",
   "Income to poverty ratio" = "inc2pvt_ratio",
   "Routine place to go for healthcare" = "usual_place_health",
   "Health insurance" = "health_ins"
  )

  formula_rhs <- paste(variables, collapse = ' + ')

  ref_levels <- variables %>%
   set_names(variables) %>%
   map_chr(~levels(nhanes_design$overall$variables[[.x]])[1]) %>%
   enframe(name = 'variable', value = 'term')

  results <- tibble(outcome = outcomes,
                    subset = subset,
                    design = design_grid) %>%
   cross_join(tibble(exam = levels(design$variables$exam))) %>%
   mutate(
    prev_ratios = pmap(
     .l = list(outcome, design, exam),
     .f = function(outcome, design, .exam){

      formula <- as.formula(paste(outcome, formula_rhs, sep = " ~ "))

      fit <- try(svyglm(formula = formula,
                    family = quasipoisson(),
                    design = design,
                    subset = exam == .exam))

      if(inherits(fit, 'try-error')) browser()

      df_resid <- fit$df.residual

      if(df_resid < 0){
       df_resid <- degf(nhanes_design$htn_jnc7)
      }

      ci <- confint(fit, ddf = df_resid) %>%
       as_tibble(rownames = 'term') %>%
       rename(lwr = `2.5 %`,
              upr = `97.5 %`)

      summary(fit) %>%
       getElement("coefficients") %>%
       as_tibble(rownames = 'term') %>%
       select(term, est = Estimate) %>%
       left_join(ci, by = 'term')

     }
    )
   )

  tbl_results <- unnest(results, prev_ratios) %>%
   select(-design) %>%
   filter(term != "(Intercept)") %>%
   separate(term, into = c("variable", "term"), sep = '\\.\\.') %>%
   mutate(across(.cols = c(est, lwr, upr), .fns = exp),
          exam = str_remove(exam, "\\.\\."),
          exam = str_replace(exam, "_", "-")) %>%
   mutate(tbv = table_glue("{est}\n({lwr}, {upr})"), .keep = 'unused')

  variable_recoder <- names(variables) %>%
   set_names(variables)

  # Prevalence ratios for blood pressure control by
  # calendar period among all adults with hypertension.
  tbls <- tbl_results %>%
   split(.$subset) %>%
   map(
    ~ .x %>%
     select(-subset, -outcome) %>%
     pivot_wider(names_from = exam, values_from = tbv) %>%
     split(.$variable) %>%
     map(~ bind_rows(filter(ref_levels, variable == .x$variable[1]), .x)) %>%
     .[variables] %>%
     bind_rows() %>%
     mutate(across(everything(), ~ if_else(is.na(.x), "1 (reference)", .x))) %>%
     mutate(variable = recode(variable, !!!variable_recoder)) %>%
     as_grouped_data(groups = 'variable') %>%
     as_flextable(hide_grouplabel = TRUE) %>%
     flextable_polish() %>%
     footnote(
      i = ~variable == "Education",
      j = 1,
      value = as_paragraph(
       "Education status was reported among participants aged 20 years and older."
      ),
      ref_symbols = "*"
     ) %>%
     footnote(
      i = ~term == "High school graduate",
      j = 1,
      value = as_paragraph(
       paste("High school graduate includes participants who reported",
             "graduating high school with or without some college education")
      ),
      ref_symbols = "†"
     ) %>%
     bg(i = ~!is.na(variable), bg = 'grey90') %>%
     padding(i = ~is.na(variable), part = 'body', j = 1, padding.left = 10) %>%
     set_header_labels(
      term = "Characteristic"
     ) %>%
     add_header_row(values = c("Characteristic", "Calendar period"),
                    colwidths = c(1, 4)) %>%
     merge_v(part = 'header')
   )

  tbls


 }),

 tar_target(tbl_prevalance_ratio_diffs, command = {

  design <- nhanes_design$htn_jnc7 %>%
   subset(race_ethnicity == "Non-Hispanic Black") %>%
   subset(exam %in% c("2017_2020", "2021_2023"))

  design$variables$bp_control_jnc7 %<>% as.numeric() %>% subtract(1)

  design_meds <- subset(design, bp_med_use == "Yes")

  data_tbl <- map_dfr(
   .x = list(htn = design, meds = design_meds),
   .id = 'subset',
   .f = ~ {

    m1 <- svyglm(formula = bp_control_jnc7 ~ exam + age_cat + gender,
                 design = .x, family = quasipoisson)

    m2 <- update(m1, formula = .~. + education + inc2pvt_ratio +
                  usual_place_health + health_ins)

    cis <- list(m1 = m1, m2 = m2) %>%
     map(confint) %>%
     map_dfr(~as_tibble(.x, rownames = 'term'), .id = 'model') %>%
     rename(lwr = `2.5 %`, upr = `97.5 %`)

    rspec <- round_spec() %>%
     round_using_decimal(2)

    list(m1 = m1, m2 = m2) %>%
     map(~summary(.x)$coefficients) %>%
     map_dfr(~as_tibble(.x, rownames = 'term'), .id = 'model') %>%
     filter(str_detect(term, '^exam')) %>%
     left_join(cis, by = c("model", "term")) %>%
     transmute(model,
               perc_diff = (Estimate[model == 'm1'] - Estimate[model == 'm2']) /
                Estimate[model == 'm1'],
               exam = str_remove(term, "^exam"),
               est = exp(Estimate),
               lwr = exp(lwr),
               upr = exp(upr)) %>%
     mutate(tbl_value = table_glue("{est} ({lwr}, {upr})", rspec = rspec),
            perc_diff = table_value(100 * perc_diff),
            model = recode(model,
                           m1 = 'Age-sex adjusted prevalence ratio (95% CI)',
                           m2 = "Full adjusted prevalence ratio (95% CI)"),
            .keep = 'unused') %>%
     select(-exam) %>%
     pivot_longer(cols = -c(model)) %>%
     mutate(
      model = if_else(
       name == 'perc_diff',
       "Percent of increase in BP control explained",
       model
      )) %>%
     distinct() %>%
     mutate(
      model = factor(
       model,
       levels = c(
        "Age-sex adjusted prevalence ratio (95% CI)",
        "Full adjusted prevalence ratio (95% CI)",
        "Percent of increase in BP control explained"
       )
      )
     ) %>%
     arrange(model)
   }
  )

  data_tbl %>%
   select(-name) %>%
   pivot_wider(names_from =subset, values_from = value) %>%
   flextable() %>%
   set_header_labels(model = "",
                     htn = "All US adults with hypertension",
                     meds = "US adults with hypertension taking antihypertensive medication") %>%
   flextable_polish() %>%
   add_footer_lines(as_paragraph("Full-adjusted model includes age, sex, education, income-to-poverty ratio, routine place to receive healthcare and type of health insurance."))


 }),

 # report targets ----

 tar_render(manuscript,
            path = here::here("doc/manuscript.Rmd"),
            output_file = paste0("manuscript", "-v", manuscript_version, "/",
                                 "manuscript-", basename(here()),
                                 "-v", manuscript_version,
                                 ".docx"))
 # end of targets ----

) %>%
 tar_hook_before(
  hook = {source("conflicts.R")},
  names = everything()
)

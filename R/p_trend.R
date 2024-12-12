

p_trend <- function(variable, design, age_adjust = NULL){

 if(is.character(design$variables[[variable]])){
  stop("variable must be numeric or factor")
 }

 lvls <- levels(design$variables[[variable]])

 design$variables %<>% mutate(
  exam_numeric = recode(
   exam,
   "2013_2014" = 0,
   "2015_2016" = 2,
   "2017_2020" = 4.6,
   "2021_2023" = 8.7
  ),
  exam_numeric = exam_numeric - min(exam_numeric)
 )

 # this is true if there are >1 age groups in the data
 age_adjust <- age_adjust %||% sum(table(design$variables$age_cat) > 0) > 1

 if(is.null(lvls)){

  formulas <- list(
   if(age_adjust){
    as.formula(paste(variable, "exam_numeric", sep = "~"))
   } else {
    as.formula(paste(variable, "exam_numeric + age_cat", sep = "~"))
   }

  )

  family <- gaussian()

 } else {

  cntrl <- if(age_adjust)
   "exam_numeric + age_cat"
  else
   "exam_numeric"

  formulas <- lvls %>%
   enframe() %>%
   mutate(name = variable) %>%
   glue_data("I({name} == \"{value}\") ~ {cntrl}") %>%
   map(as.formula)

  family <- quasibinomial()

 }

 pvals <- formulas %>%
  map_dbl(
   function(f) {

    svyglm(formula = f, design = design, family = family) %>%
     summary() %>%
     getElement("coefficients") %>%
     .["exam_numeric", "Pr(>|t|)"]
   }

  )

 tibble(variable = variable,
        level = lvls,
        p_trend = table_pvalue(pvals,
                               drop_leading_zero = FALSE,
                               decimals_outer = 3L,
                               decimals_inner = 3L))

}

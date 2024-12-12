

design_standardize <- function(design,
                               time = 'exam',
                               group = NULL,
                               standard_variable = 'age_cat',
                               standard_weights){

 if(group == standard_variable) return(design)

 by_vars <- c(time, group)

 over <- by_vars %>%
  setdiff(c(standard_variable, "1")) %>%
  as_svy_formula()

 excluding.missing <- NULL

 if(!is.null(group) && group != "1") excluding.missing <- as_svy_formula(group)

 svystandardize(by = as_svy_formula(standard_variable),
                over = over,
                design = design,
                population = standard_weights,
                excluding.missing = excluding.missing)

}

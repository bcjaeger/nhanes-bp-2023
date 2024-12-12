

suppress_catg <- function(estimate,
                          std_error,
                          ci_upper,
                          ci_lower,
                          design,
                          divby = 1){

  result <- vector(mode = 'character', length = length(estimate))

  n_obs <- nrow(design$variables)
  n_psu <- length(unique(design$cluster[[1]]))
  n_strata <- length(unique(design$strata[[1]]))

  n_df <- n_psu - n_strata

  # When applicable for complex surveys, if the sample size and confidence
  # interval criteria are met for presentation and the degrees of freedom
  # are fewer than 8, then the proportion should be flagged for statistical
  # review by the clearance official. This review may result in either the
  # presentation or the suppression of the proportion.

  if(n_df < 8){
    result[] <- "review_df_lt_8..."
  }


  p  <- estimate / divby
  q  <- 1 - p
  se <- std_error / divby

  if(any(p < .Machine$double.eps)){

    result[p < .Machine$double.eps] %<>% paste0("review_zero_events...")

  }

  if(any(q < .Machine$double.eps)){

    result[p < .Machine$double.eps] %<>% paste0("review_all_events...")

  }

  ci_width <- (ci_upper - ci_lower) / divby

  if( any(is.nan(ci_width)) ) ci_width[is.nan(ci_width)] <- Inf

  # Large absolute confidence interval width
  # If the absolute confidence interval width is greater than or
  # equal to 0.30, then the proportion should be suppressed.

  if(any(ci_width >= 0.30)){

    result[ci_width >= 0.30]  %<>% paste0("suppress_ci_width_gteq_30...")

  }

  n_eff = ifelse(
    test = (0 < p & p < 1),
    yes = pmin((p * (1 - p)) / (se ^ 2), n_obs),
    no =  n_obs
  )

  if(any(is.na(n_eff))) n_eff[is.na(n_eff)] <- n_obs

  # Sample size (n_eff)
  # Estimated proportions should be based on a minimum denominator sample size
  # and effective denominator sample size (when applicable) of 30. Estimates
  # with either a denominator sample size or an effective denominator sample
  # size (when applicable) less than 30 should be suppressed.

  if(any(n_eff < 30)){

    result[n_eff < 30] %<>% paste("suppress_n_eff_lt_30...")

  }

  # Korn and Graubard CI relative width for p
  ci_width_relative_p = ifelse(p > 0, 100*(ci_width / p), NA)

  # Korn and Graubard CI relative width for q
  ci_width_relative_q = ifelse(q > 0, 100*(ci_width / q), NA)


  # Relative confidence interval width
  # If the absolute confidence interval width is between 0.05 and 0.30 and the
  # relative confidence interval width is more than 130%, then the proportion
  # should be suppressed.
  # If the absolute confidence interval width is between 0.05 and 0.30 and the
  # relative confidence interval width is less than or equal to 130%, then the
  # proportion can be presented if the degrees of freedom criterion below is met.

  p_unreliable <- ci_width_relative_p > 130 & ci_width > 0.05 & ci_width < 0.3

  if(any(p_unreliable)){

    result[p_unreliable] %<>% paste0("suppress_relative_ci_width_gt_130...")

  }

  # Complementary proportions
  # If all criteria are met for presenting the proportion but not for its
  # complement, then the proportion should be shown. A footnote indicating that
  # the complement of the proportion may be unreliable should be provided.

  # Determine if estimate should be flagged as having an unreliable
  # complement; Complementary proportions are reliable unless Relative
  # CI width is greater than 130%;
  q_unreliable <- ci_width_relative_q > 130 & ci_width > 0.05 & ci_width < 0.3

  if(any(q_unreliable)){

    result[q_unreliable] %<>% paste0("review_unreliable_complement...")

  }

  result[result == ''] <- "okay..."

  result <- str_sub(result, start = 0, end = -4)

  result

}

suppress_ctns <- function(estimate,
                          std_error,
                          design){

  n_obs <- rep(nrow(design$variables), times = length(estimate))

  result <- vector(mode = 'character', length = length(estimate))

  # rse: relative standard error
  rse <- std_error / estimate

  if( any(is.nan(rse)) ) rse[is.nan(rse)] <- -Inf

  if(any(rse >= 0.3)){

    result[rse >= 0.3] %<>% paste("suppress_rse_gteq_0.3...")

  }

  if(any(n_obs) < 30){

    result[n_obs < 30] %<>% paste("suppress_nobs_lt_30...")

  }


  if(any(is.infinite(rse))){

    result[is.infinite(rse)] %<>% paste("suppress_inf_rse...")

  }

  result[result == ''] <- "okay..."

  result <- str_sub(result, start = 0, end = -4)

  result

}

is_suppressed <- function(x){
  str_detect(x, pattern = 'suppress')
}

#' t_test_sim
#'
#' @param mu_true 
#' @param n_subj 
#' @param sigma_true 
#' @param mu_null 
#' @param alpha 
#'
#' @returns
#' @export
#'
#' @examples
t_test_sim = function(mu_true, n_subj = 30, sigma_true = 5, mu_null = 0, alpha = 0.05){
  
  # generate the data
  x = rnorm(n = n_subj, mean = mu_true, sd = sigma_true)
  
  # perform the one-sample t-test
  test_output = t.test(x, mu = mu_null, conf.level = alpha)
  
  # clean output and calculate results
  test_output_cleaned = broom::tidy(test_output) |>
    # select the point estimate (mu_hat) and p-value
    select(mu_hat = estimate, p_value = p.value) |>
    mutate(
      # determine if H0 was rejected
      rejected_H0 = (p_value < alpha)
    )
  
  test_output_cleaned
}
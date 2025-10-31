#' prop_test
#'
#' @param dataset 
#'
#' @returns
#' @export
#'
#' @examples
prop_test = function(dataset) {
  
  n_success = pull(dataset, unsolved_homicides)
  n_total = pull(dataset, total_homicides)
  
  test_output = prop.test(x = n_success, n = n_total)
  
  test_output_cleaned = broom::tidy(test_output) |> 
    select(
      p_hat = estimate, 
      conf_low = conf.low, 
      conf_high = conf.high
    )
  
  test_output_cleaned
  
}
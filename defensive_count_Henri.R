count_them <- function(supposedly_a_count) {
  # check it
  assert_numeric(supposedly_a_count, lower = 0, len = 1, any.missing = F, finite = T)
  
  if (!checkmate::test_count(supposedly_a_count)) {
    warning(
      "rounding ", supposedly_a_count,
      " to the nearest integer."
    )
    supposedly_a_count <- round(supposedly_a_count)
  }
  as.integer(supposedly_a_count)
}




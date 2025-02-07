#'
#'  Contiguous subvector with the largest sum
#'
#'  Finds the contiguous subvector with the
#'  largest sum among all the contiguous subvectors of `v`.
#'
#'  Taken from https://tomaztsql.wordpress.com/2022/03/28/little-useless-useful-r-functions-kadanes-algorithm/
#'
#' @param v A numeric vector
#' @return A one-row data frame with columns
#' * `Sum` - sum of the subvector's elements
#' * `Start` - starting index of the subvector
#' * `End` - ending index of the subvector
#' @export
#'
kadane = function(v) {
  max_so_far = -Inf
  max_ending_here = 0
  subarray_start = 0
  subarray_end = 0
  int_s = 0

  for (i in 1:length(v)) {
    max_ending_here = max_ending_here  + v[i]
    if (max_so_far < max_ending_here ){
      max_so_far = max_ending_here
      subarray_start = int_s
      subarray_end = i
    }
    if (max_ending_here < 0) {
      max_ending_here = 0
      int_s =  i + 1
    }
  }

  ## cat("Sum is: ", max_so_far, " with starting position: ", subarray_start, " and ending: ", subarray_end)

  data.frame(Sum = max_so_far, Start = subarray_start, End = subarray_end)
}

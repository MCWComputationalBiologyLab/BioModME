# custom_round_df
#
# This function rounds numeric columns in a dataframe based on specified parameters.
# The primary goal is to provide custom rounding options, with added flexibility for
# the first column. It can also convert values to scientific notation if they round to zero.
# Non-numeric columns are returned untouched.
#
# Args:
#   df: The input dataframe.
#   digits: Number of decimal places to which numeric values (except the first column, unless specified) should be rounded.
#           Default is 2.
#   zero_as_plain: If TRUE (default), values that round to zero are returned as 0.
#                  Otherwise, they are returned as 0.00 or in the format specified by the other parameters.
#   to_sci: If TRUE (default), values that round close enough to zero will be represented in scientific notation.
#   ignore_first_col: If TRUE, the first column is ignored and returned without any modifications.
#                     Default is FALSE.
#   first_col_digits: Number of decimal places to which the first column should be rounded.
#                     If not specified, it defaults to the value of 'digits'.
#
# Returns:
#   A dataframe with rounded numeric columns based on the specified parameters.

custom_round_df <- function(df, 
                            digits = 2, 
                            zero_as_plain = TRUE, 
                            to_sci = TRUE, 
                            ignore_first_col = FALSE, 
                            first_col_digits = NULL) {
  
  # If first_col_digits is not provided, use the default digits value
  if (is.null(first_col_digits)) {
    first_col_digits <- digits
  }
  
  # Internal function for custom rounding
  custom_round <- function(x, custom_digits = digits) {
    
    # Check if value is zero and zero_as_plain is TRUE
    if(zero_as_plain && x == 0) {
      return(0)
    }
    
    rounded_x <- round(x, custom_digits)
    
    # Check if the rounded value is close enough to zero for scientific notation
    if(to_sci && abs(rounded_x) < 10^(-custom_digits)) {
      return(formatC(x, format = "e", digits = custom_digits))
    } else {
      return(rounded_x)
    }
  }
  
  # Apply the custom rounding function to numeric columns
  df_rounded <- as.data.frame(lapply(seq_along(df), function(idx) {
    col <- df[[idx]]
    if (ignore_first_col && idx == 1) {
      return(col)
    }
    if (is.numeric(col)) {
      # Apply different rounding for the first column
      if (idx == 1) {
        return(sapply(col, function(val) custom_round(val, first_col_digits)))
      } else {
        return(sapply(col, custom_round))
      }
    } else {
      return(col)
    }
  }))
  
  names(df_rounded) <- names(df)  # Assign column names
  
  return(df_rounded)
}

library(tidyr)
library(dplyr)
library(tidyselect)


#' @title Pivot annual data from wide to long
#' @description `pivot_wb`` takes in a dataset with columns documenting annual data in wide format and condenses into longer format,
#' decreasing the number of columns while increasing the number of rows.
#' 
#' @param df A dataframe object to pivot.
#' @param varname A string to name the new column to be created storing the data originally from each year variable.
#' @param col_prefix An optional prefix string used to identify relevant year column variables, whose prefix will be removed. Defaults to "X".
#' @returns A tibble dataframe of the original dataframe in long format.
#' @examples
#' # Using provided dataset matmor
#' matmor
#' matmor |> pivot_wb(varname = "matmor", col_prefix = "X")
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#' @importFrom tidyselect starts_with
#' @export
pivot_wb <- function(df, varname, col_prefix = "X"){
  df_long <- df |> 
    tidyr::pivot_longer(cols = tidyselect::starts_with(col_prefix),
                 names_to = "year",
                 names_prefix = col_prefix, # removes X from year column
                 values_to = varname) |> 
    dplyr::mutate(year = as.numeric(year)) |> # change year to numeric
    dplyr::select(iso, year, varname)
}


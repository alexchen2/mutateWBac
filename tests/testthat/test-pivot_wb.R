check_suggests <- function() {
  skip_if_not_installed("tidyr")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyselect")
}

test_that("Basic functionality of pivot_wb works", {
  check_suggests()

  matmor0 <- matmor %>%
    head() %>%
    pivot_wb(varname = "mat_mortality")
  
  # Check column names of new wider format table
  expect_equal(colnames(matmor0), c("iso", "year", "mat_mortality"))
  
  # Check if certain values were transformed properly from longer to wider format
  expect_equal(
    matmor0 %>% 
      dplyr::filter(iso == "AFG", year == 2000) %>%
      dplyr::select(mat_mortality) %>%
      as.numeric, 
    matmor %>% 
      dplyr::filter(iso == "AFG") %>%
      dplyr::select(X2000) %>%
      as.numeric
  )

  expect_equal(
    matmor0 %>% 
      dplyr::filter(iso == "ARG", year == 2005) %>%
      dplyr::select(mat_mortality) %>%
      as.numeric, 
    matmor %>% 
      dplyr::filter(iso == "ARG") %>%
      dplyr::select(X2005) %>%
      as.numeric
  )
})

test_that("Custom prefixes passed through pivot_wb are detected", {
  check_suggests()
  
  matmor_preA <- matmor %>% 
    select(iso, A2000 = X2000, A2001 = X2001, A2002 = X2002)

  matmor0_preA <- matmor_preA %>%
    head() %>%
    pivot_wb(
      varname = "mat_mortality", 
      col_prefix = "A"
    )

  # Check if custom prefixed year columns were transformed properly
  expect_equal(colnames(matmor0_preA), c("iso", "year", "mat_mortality"))
  
  # Check if values persisted through pivot with custom prefixed columns
  expect_equal(
    matmor0_preA %>% 
      dplyr::filter(iso == "AFG", year == 2000) %>%
      dplyr::select(mat_mortality) %>%
      as.numeric, 
    matmor_preA %>% 
      dplyr::filter(iso == "AFG") %>%
      dplyr::select(A2000) %>%
      as.numeric
  )

  expect_equal(
    matmor0_preA %>% 
      dplyr::filter(iso == "ARG", year == 2002) %>%
      dplyr::select(mat_mortality) %>%
      as.numeric, 
    matmor_preA %>% 
      dplyr::filter(iso == "ARG") %>%
      dplyr::select(A2002) %>%
      as.numeric
  )
})

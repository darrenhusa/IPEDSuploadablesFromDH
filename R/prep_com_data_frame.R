#' Some initial recoding for Completions
#'
#' @param df a dataframe of student level data or cip information
#'
#' @importFrom dplyr case_when mutate select
#' @importFrom tidyr separate
#'
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe ready for the make_com scripts
#' @export
#'

prep_com_data_frame <- function(df) {
  colnames(df) <- stringr::str_to_upper(colnames(df))

  # cips could be 6-digit characters: if so, add the period
  if (sum(cip_is_valid(df$MAJORCIP)) == nrow(df)) {
    df <- df |>
      dplyr::mutate(MAJORCIP = add_period_to_cip(df$MAJORCIP))
  }

  # if cips end in .0000, the file read-in may lose the decimal and period
  # need to add it back before we can proceed
  df$MAJORCIP <- ensure_cip_has_period(df$MAJORCIP)

  # now check that all cips have a period... if not, throw a warning
  if (cip_does_not_have_a_period(df$MAJORCIP)) {
    stop("Cip Codes are not in an accepted format. Review setup requirements for Completions")
  }

  # if they do have a period, only proceed if the format isn't finished
  df <- df |>
    separate_cip_into_components(df$MAJORCIP) |>
    dplyr::mutate(
      Two = add_leading_zero_to_cip_lhs(df$Two),
      Four = adjust_cip_rhs_format(df$Four),
      MAJORCIP = join_cip_components_by_period(df$Two, df$Four)
    ) |>
    dplyr::select(-"Two", -"Four") |>
    dplyr::mutate(
      UNITID = ensure_data_is_character_type(df$UNITID),
      DEGREELEVEL = ensure_data_is_character_type(df$DEGREELEVEL)
    )

  if ("STUDENTID" %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(STUDENTID = ensure_data_is_character_type(df$STUDENTID))
  }

  df
}

# private functions

# is_valid is a signal that the function returns a boolean: TRUE or FALSE
# TRUE = 1
# FALSE = 0
# reason sum() == nrow(df) works, check all cases of the variable df$x
# needs to match the number of rows in the dataframe.
cip_is_valid <- function(x) {
  grepl(x, pattern = "^[0-9]{6}$")
}

add_period_to_cip <- function(x) {
  gsub(
    pattern = "(^[0-9]{2})([0-9]{4}$)",
    replacement = "\\1\\.\\2",
    x
  )
}

# use when cip code is in two parts separated by a period
# and ends in four zero characters.
ensure_cip_has_period <- function(x) {
  gsub(pattern = "(^[0-9]{1,2}$)", replacement = "\\1\\.0000", x)
}

cip_has_a_period <- function(x) {
  grepl(x, pattern = "\\.")
}

cip_does_not_have_a_period <- function(x) {
  !cip_has_a_period(x)
}

separate_cip_into_components <- function(x) {
  tidyr::separate(
    col = x,
    into = c("Two", "Four"),
    sep = "\\."
  )
}

add_leading_zero_to_cip_lhs <- function(x) {
  dplyr::case_when(
    nchar(x) == 1 ~ paste0("0", x),
    TRUE ~ x
  )
}

adjust_cip_rhs_format <- function(x) {
  dplyr::case_when(
    nchar(x) == 0 ~ paste0(x, "0000"),
    nchar(x) == 1 ~ paste0(x, "000"),
    nchar(x) == 2 ~ paste0(x, "00"),
    nchar(x) == 3 ~ paste0(x, "0"),
    TRUE ~ x
  )
}

join_cip_components_by_period <- function(p1, p2) {
  paste0(p1, ".", p2)
}

ensure_data_is_character_type <- function(x) {
  as.character(x)
}

hr_df <- data.frame(unitid = 123456,
                    empid = c(1:16),
                    gender = rep(1:2, 8),
                    raceethnicity = rep(1:4, 4),
                    occcategory3 = c(1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 18, 20, 22, 24),
                    ftpt = c('P', rep('F', 15)),
                    months = c(99, rep(9, 15)),
                    currentemployee = 1)
hr_df

#cips could be 6-digit characters: if so, add the period
#if(sum(grepl(df$MAJORCIP, pattern = "^[0-9]{6}$")) == nrow(df)){

#becomes

#if(sum(cip_code_is_valid(df$MAJORCIP)) == nrow(df)){


# valid cip format = 6 digits with no period
major.cip <- 130000

# too long
major.cip <- 123456789
# too short
major.cip = 100
# includes non-numeric characters
major.cip = '12AB'
# includes the period
major.cip = '22.0000'

grepl(major.cip, pattern = "^[0-9]{6}$")

cip_code_is_valid <- function(x) {
  grepl(x, pattern = "^[0-9]{6}$")
}

# call the function
cip_code_is_valid(major.cip)

cip_code_is_valid(520000)

library(tidyr)

# not working

# where x here is a dataframe column
#separate_cip_code_into_component_parts <- function(x) {
#  tidyr::separate(col = x,
#                  into = c("Two", "Four"),
#                  sep = "\\."
#  )
#}

#result = separate_cip_code_into_component_parts('13.0000')

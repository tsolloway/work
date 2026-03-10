#' example_data_ice_cream
#'
#' @description Synthetic survey data for 1000 respondents rating 30 ice cream
#'   flavors. Binary item columns indicate selection (1) or not (0). Includes
#'   subgroup columns (Total, Gen_Z, Millennials, Gen_X), a weight column, and
#'   respondent UUIDs. Useful for testing TURF analysis workflows.
#'
#' @format A tibble with 1000 rows and 36 columns:
#' \describe{
#'   \item{resp_id}{UUID respondent identifier}
#'   \item{weight}{Numeric survey weight (0.5 to 1.5)}
#'   \item{Total}{Always 1 (total sample flag)}
#'   \item{Gen_Z, Millennials, Gen_X}{Binary subgroup membership}
#'   \item{ic_vanilla, ic_chocolate, ...}{Binary ice cream flavor selections (30 items)}
#' }
"example_data_ice_cream"

#' example_data_ice_cream_dictionary
#'
#' @description Variable dictionary for [example_data_ice_cream]. Maps variable
#'   names to human-readable labels for the 30 ice cream flavor items.
#'
#' @format A tibble with 30 rows and 2 columns:
#' \describe{
#'   \item{variable}{Variable name matching column in `example_data_ice_cream`}
#'   \item{label}{Human-readable flavor label}
#' }
"example_data_ice_cream_dictionary"

#' example_data_brand
#'
#' @description Simulated consumer brand perception survey data for 2000
#'   respondents rating 30 brand attributes on a 5-point Likert scale. Attributes
#'   are organized into 6 thematic clusters (quality, innovation, trust, value,
#'   emotional connection, social responsibility) plus 2 outcome variables.
#'   Includes demographic subgroup columns. Designed for testing Bayesian Network
#'   analysis workflows.
#'
#' @format A tibble with 2000 rows and 39 columns:
#' \describe{
#'   \item{resp_id}{UUID respondent identifier}
#'   \item{Brand}{Brand name (6 brands: Apex, Vero, Kinetic, Solace, Drift, Ember)}
#'   \item{Brand_id}{Numeric brand identifier (1-6)}
#'   \item{Total}{Always 1 (total sample flag)}
#'   \item{Gen_Z, Millennials, Gen_X}{Binary subgroup membership}
#'   \item{bp_high_quality, bp_reliable, ...}{Brand perception attributes (30 items, 1-5 scale)}
#'   \item{ltr}{Likelihood to Recommend (1-5 scale)}
#'   \item{overall_satisfaction}{Overall Brand Satisfaction (1-5 scale)}
#' }
"example_data_brand"

#' example_data_brand_dictionary
#'
#' @description Variable dictionary for [example_data_brand]. Maps variable
#'   names to human-readable labels for the 30 brand perception attributes
#'   and 2 outcome variables.
#'
#' @format A tibble with 32 rows and 2 columns:
#' \describe{
#'   \item{var}{Variable name matching column in `example_data_brand`}
#'   \item{label}{Human-readable display label}
#' }
"example_data_brand_dictionary"

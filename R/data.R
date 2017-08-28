#' IPCA items and its weights
#'
#' A dataset containing the IPCA items, their respective weights and codes in tibble format. Items and codes are also provided in ts data structure.
#'
#'
#' @format A list with five attributes:
#' \describe{
#'   \item{ipca}{dataframe with ipca items}
#'   \item{weights}{dataframe with weights items}
#'   \item{ipca_ts}{ts with ipca items}
#'   \item{weights_ts}{ts with weights items}
#'   \item{cod}{Items' codes}
#' }
#' @source \url{https://sidra.ibge.gov.br}
"ipca_item"


#' IPCA subitems and its weights
#'
#' A dataset containing the IPCA items, their respective weights and codes in tibble format. Subitems and codes are also provided in ts data structure.
#'
#' @format A list with six attributes:
#' \describe{
#'   \item{ipca}{dataframe with ipca subitems}
#'   \item{weights}{dataframe with weights subitems}
#'   \item{ipca_ts}{ts with ipca subitems}
#'   \item{weights_ts}{ts with weights subitems}
#'   \item{cod}{Subitems' codes}
#'   \item{ipca_index}{The full index}
#' }
#' @source \url{https://sidra.ibge.gov.br}
"ipca_sub"

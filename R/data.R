#' Learning Community Core Data
#'
#' @title dat
#' @description A core dataset from DEA1500 Learning Community App and several Qualtrics Survey including 6 lists: appdata.group, appdata.log, appdata.user, appdata.survey, appdata.post, appdata.comment
#' @format appdata.log: A data frame with 10066 rows and 4 variables:
#' \describe{
#'   \item{\code{day}}{date Event trigger date}
#'   \item{\code{netid}}{character User id}
#'   \item{\code{event}}{integer Log event}
#'   \item{\code{freq}}{integer Event frequency}
#'}
#' @format appdata.user: A data frame with 456 rows and 4 variables:
#' \describe{
#'   \item{\code{is_superuser}}{integer Whether is superuser}
#'   \item{\code{netid}}{character User id}
#'   \item{\code{username}}{character User name}
#'   \item{\code{link}}{character User profile URL}
#'}
#' @format appdata.survey: A data frame with 963 rows and 5 variables:
#' \describe{
#'   \item{\code{StartDate}}{date Survey submission date}
#'   \item{\code{usernetid}}{character User id}
#'   \item{\code{eng}}{character Engagement type}
#'   \item{\code{result}}{double Engagement result calculated by averaging Likert questions}
#'   \item{\code{time}}{character Survey wave}
#'}
#' @format appdata.post: A data frame with 1679 rows and 3 variables:
#' \describe{
#'   \item{\code{reason}}{character Post description}
#'   \item{\code{day}}{date Post date}
#'   \item{\code{netid}}{character User id}
#'}
#' @format appdata.comment: A data frame with 2522 rows and 3 variables:
#' \describe{
#'   \item{\code{content}}{character Comment description}
#'   \item{\code{day}}{date Comment date}
#'   \item{\code{netid}}{character User id}
#'}
#' @format appdata.group: A list with 4 vectors:
#' \describe{
#'   \item{\code{ELnetid}}{character Early leaver id}
#'   \item{\code{Lnetid}}{character Leaver id}
#'   \item{\code{LLnetid}}{character Late leaver id}
#'   \item{\code{Pnetid}}{character Participant id}
#'}
#' @source <https://dea1500.xyz>
"dat"

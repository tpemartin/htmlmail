#' Create an htmlmail instance
#'
#' @description must first run gm_auth_configure(path = "support/gmailAPI.json") with respect to the json file from gmail API credential setup (see gmailr).
#' @param htmlpath the path of the html content for the email, especially created from mjml app.
#'
#' @return An environment, with create_draft(to, from, subject) method, and send(to, from, subject) method using gmail
#' @export
#'
#' @examples none
initiate_htmlmail <- function(htmlpath)
{
  require(gmailr)
  require(magrittr)
  mjml <- new.env()
  mjml$html <- mjml_import(htmlpath)
  mjml$mimeBody <-
    gm_mime() %>%
    gm_html_body(mjml$html)

  mjml$create_draft <- function(to, from, subject){
    mjml_createDraft(to, from, subject, mjml$mimeBody)
  }

  mjml$send <- function(to, from, subject){
    mjml_send(to, from, subject, mjml$mimeBody)
  }

  return(mjml)
}

mjml_import <- function(htmlpath) {
  lines <- xfun::read_utf8(htmlpath)
  return(paste0(lines, collapse = "\n"))
}
create_mime <- function(to, from, subject, mimeBody){
  mimeBody %>%
    gm_subject(subject) %>%
    gm_to(to) %>%
    gm_from(from)
}
# Verify it looks correct
mjml_createDraft <- function(to, from, subject, mimeBody){
  create_mime(to, from, subject, mimeBody) %>%
    gmailr::gm_create_draft()
}
mjml_send <- function(to, from, subject, mimeBody){
  create_mime(to, from, subject, mimeBody) %>%
    gmailr::gm_send_message()
}

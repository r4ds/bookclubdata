# These functions mostly just wrap googledrive/googlesheets4 functions.

# nocov start
.service_account_json <- function() {
  intToUtf8(
    base64enc::base64decode(
      Sys.getenv("R4DS_CLUB_GOOGLE_SERVICE_ACCOUNT")
    )
  )
}

.googledrive_authorize <- function() {
  googledrive::drive_auth(
    path = .service_account_json()
  )
}

.googlesheets_authorize <- function() {
  googlesheets4::gs4_auth(
    path = .service_account_json()
  )
}

.googledrive_timestamp <- function(file_id) {
  .googledrive_authorize()
  req <- googledrive::request_generate(
    endpoint = "drive.files.get",
    params = list(fileId = file_id, fields = "modifiedTime")
  )
  res <- googledrive::do_request(req)
  return(lubridate::ymd_hms(res$modifiedTime))
}

.googlesheet_read <- function(ss, sheet, ...) {
  .googlesheets_authorize()
  googlesheets4::local_gs4_quiet()
  cli::cli_inform(
    "Reading from {ss}, sheet {sheet}."
  )
  googlesheets4::read_sheet(ss, sheet, ...)
}

.googledrive_download <- function(file, path) {
  .googledrive_authorize()
  googledrive::local_drive_quiet()
  cli::cli_inform(
    "Downloading googledrive file {file}."
  )
  googledrive::drive_download(googledrive::as_id(file), path)
}

.googledrive_upload_public <- function(path, name) {
  .googledrive_authorize()
  googledrive::local_drive_quiet()
  res <- googledrive::drive_upload(path, name = name)
  googledrive::drive_share_anyone(res)
  return(res$id)
}

.googledrive_update <- function(id, path, ...) {
  .googledrive_authorize()
  googledrive::local_drive_quiet()
  cli::cli_inform("Updating googledrive file {id}.")
  googledrive::drive_update(
    file = googledrive::as_id(id),
    media = path,
    ...
  )
}
# nocov end

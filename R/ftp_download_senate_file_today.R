# ftp_download_senate_file_today -----------------------------------------------
ftp_download_senate_file_today <- function(dest_folder, user_pwd, dbg = TRUE)
{
  url <- sprintf("ftp://%s@193.23.163.140/ExportFlusshygiene.csv", user_pwd)

  today <- format(as.Date(Sys.time()), "%y%m%d")

  destfile <- file.path(dest_folder, sprintf("TW_SW_%s.txt", today))

  download_file(url, destfile)
}

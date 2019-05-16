# download_files_from_ftp_server -----------------------------------------------
download_files_from_ftp_server <- function(
  file_names, target_dir, user_pwd, dbg = TRUE
)
{
  if (! length(file_names)) {

    cat("No files given.\n")
    return()
  }

  url <- sprintf("ftp://%s@ftp.kompetenz-wasser.de", user_pwd)

  kwb.utils::catAndRun(
    dbg = dbg,
    messageText = "Downloading files to temporary directory...",
    expr = for (file_name in file_names) {
      destfile <- file.path(tempdir(), file_name)
      download_file(file.path(url, file_name), destfile, dbg)
    }
  )

  # Copy the files from the temporary directory to the target directory
  kwb.utils::catAndRun(
    dbg = dbg,
    messageText = paste("Copying files to", target_dir),
    expr = file.copy(
      from = file.path(tempdir(), file_names),
      to = file.path(target_dir, file_names)
      #to = file.path(target_dir, paste0(extract_date_string(file_names), ".txt"))
    )
  )
}

# extract_date_string ----------------------------------------------------------
extract_date_string <- function(x)
{
  start <- nchar("Regenschreiber_") + 1

  substr(x, start, start + 5)
}

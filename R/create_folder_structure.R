# create_folder_structure ------------------------------------------------------
create_folder_structure <- function(root)
{
  # Create download folder if necessary
  downloads <- kwb.utils::createDirectory(file.path(root, "downloads"))
  downloads_bwb <- kwb.utils::createDirectory(file.path(downloads, "bwb"))
  downloads_senate <- kwb.utils::createDirectory(file.path(downloads, "senate"))

  # Create database folder if necessary
  db <- kwb.utils::createDirectory(file.path(root, "database"))

  list(
    downloads = downloads,
    downloads_bwb = downloads_bwb,
    downloads_senate = downloads_senate,
    db = db
  )
}

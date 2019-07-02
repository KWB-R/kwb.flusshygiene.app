# create_folder_structure ------------------------------------------------------
create_folder_structure <- function(root, dbg = FALSE)
{
  # Helper function
  create_dir <- function(x) kwb.utils::createDirectory(x, dbg = dbg)

  # Create download folder if necessary
  downloads <- create_dir(file.path(root, "downloads"))
  downloads_bwb <- create_dir(file.path(downloads, "bwb"))
  downloads_senate <- create_dir(file.path(downloads, "senate"))

  # Create database folder if necessary
  db <- create_dir(file.path(root, "database"))

  # Create prediction folder if necessary
  predictions <- create_dir(file.path(root, "predictions"))

  list(
    downloads = downloads,
    downloads_bwb = downloads_bwb,
    downloads_senate = downloads_senate,
    db = db,
    predictions = predictions
  )
}

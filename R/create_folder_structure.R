# create_folder_structure ------------------------------------------------------
create_folder_structure <- function(root, dbg = FALSE)
{
  # Helper function
  create_dir <- function(...) {
    kwb.utils::createDirectory(file.path(...), dbg = dbg)
  }
    
  # Create download folder if necessary
  downloads <- create_dir(root, "downloads")
  downloads_bwb <- create_dir(downloads, "bwb")
  downloads_senate <- create_dir(downloads, "senate")

  # Create database folder if necessary
  db <- create_dir(root, "database")

  # Create prediction folder if necessary
  predictions <- create_dir(root, "predictions")

  list(
    downloads = downloads,
    downloads_bwb = downloads_bwb,
    downloads_senate = downloads_senate,
    db = db,
    predictions = predictions
  )
}

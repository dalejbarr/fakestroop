#' Save stroop data to disk
#'
#' Save Stroop data to disk with varying levels of data-wrangling difficulty.
#'
#' @param stroop Source data object created using [make_stroop()].
#' @param path Name of the path to write the data.
#' @param difficulty Whether the data wrangling task should be `"easy"` or `"hard"`.
#' @param overwrite Whether to overwrite an existing path.
#' @param quiet Whether to suppress messages when files are saved.
#' @returns Returns `NULL` invisibly (called for its side effect only).
#' @importFrom utils write.csv
#' @export
save_stroop <- function(stroop, path, difficulty = "hard", overwrite = FALSE, quiet = FALSE) {
  diff_opts <- c("easy", "medium", "hard")
  if (!(difficulty %in% diff_opts)) {
    stop("'difficulty' must be one of: '",
         paste(diff_opts, collapse = "', '"), "'")
  }
  
  if (dir.exists(path)) {
    if (!overwrite) {
      stop("output directory '", path, "' exists and overwrite = FALSE")
    } else {
      unlink(path, TRUE, TRUE)
    }
  }
  dir.create(path, FALSE)

  ## easy = idealized dataset
  if (difficulty == "easy") {
    fname <- file.path(path, "stroop-data.csv")
    sw <- wrangle_stroop(stroop)
    names(sw) <- c("id", "eng_lang", "congruent", "incongruent")
    sw |>
      write.csv(fname, row.names = FALSE)
    if (!quiet) {
      message("wrote ", nrow(sw), " rows to ", fname)
    }
  } else {
    ## medium or hard
    demo <- if (difficulty == "medium") {
              ## no typos or NAs
              ff <- stroop[["demo"]][!is.na(stroop[["demo"]][["eng_lang"]]),
                                     c("id", "el2")]
              names(ff) <- c("id", "eng_lang")
              ff
            } else {
              ## typos and NAs
              stroop[["demo"]][,c("id", "age", "eng_lang")]
            }
    rownames(demo) <- NULL

    write.csv(demo,
              file = file.path(path, "demographics.csv"),
              row.names = FALSE, na = "")
    if (!quiet) {
      message("wrote demographic data to '",
              file.path(path, "demographics.csv"), "'")
    }

    if (difficulty == "hard") {
      transcript <- stroop[["trials"]][, c("id", "trial", "response_obs")]
      names(transcript)[3] <- "response"
      write.csv(transcript, file = file.path(path, "transcript.csv"),
                row.names = FALSE)
      if (!quiet) {
        message("wrote transcript to '",
                file.path(path, "transcript.csv"), "'")
      }
      sid <- names(stroop[["raw"]])
      lapply(sid, function(.x) {
        fname <- file.path(path, paste0(.x, ".csv"))
        write.csv(stroop[["raw"]][[.x]],
                  file = fname, row.names = FALSE)
      })
      if (!quiet) {
        message("wrote ", length(sid), " files with trial data to '",
                file.path(path, paste0("S",
                                       paste(rep("X", nchar(sid[1]) - 1),
                                             collapse = ""))),
                ".csv'")
      }
    } else {
      trials <- stroop[["trials"]][, c("sub_id", "word", "ink_colour",
                                       "response", "RT")]
      names(trials) <- sub("sub_id", "id", names(trials)) |>
        sub("word", "stimword", x = _)
      fname <- file.path(path, "trials.csv")
      write.csv(trials, file = fname, row.names = FALSE)
      if (!quiet) {
        message("wrote ", nrow(trials), " trials to ", fname)
      }
    }
  }
  
  invisible(NULL)
}

#' Save stroop data to zipfile
#'
#' Save Stroop data to zip file with varying levels of data-wrangling difficulty.
#'
#' @param stroop Source data object created using [make_stroop()].
#' @param zip_filename Name of the output zipfile.
#' @param difficulty Whether the data wrangling task should be `"easy"` or `"hard"` (passed to `save_stroop()`).
#' @param overwrite Whether to overwrite an existing file.
#' @param quiet Whether to suppress messages when files are saved.
#' @returns Returns `NULL` invisibly (called for its side effect only).
#' @importFrom utils write.csv
#' @importFrom utils zip
#' @export
zip_stroop <- function(stroop, zip_filename, difficulty, overwrite = FALSE,
                       quiet = FALSE) {
  if (file.exists(zip_filename) && !overwrite) {
    stop("zip file '", zip_filename, "' exists and overwrite = FALSE")
  }
  if (file.exists(zip_filename)) file.remove(zip_filename)
  
  tf <- tempfile()
  save_stroop(stroop, tf, difficulty, overwrite = TRUE, quiet = TRUE)
  zip(zip_filename, tf, flags = "-rj")
  if (!quiet) {
    message("wrote stroop data to '", zip_filename, "'")
  }

  invisible(NULL)
}

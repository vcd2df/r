  get_vars <- function(fptr) {
    line <- readLines(fptr, n = 1)
    # vars will be the names of variables and their encodings
    vars <- list()
    while (!grepl("\\$enddefinitions", line)) {
      if (grepl("var", line)) {
        # get the name and encoding and save it, if novel
        # we assume same named vars if different modules are redundant
        parts <- strsplit(line, " ")[[1]]
        if (!parts[4] %in% vars) {
          vars[[parts[5]]] <- parts[4]
        }
      }
      line <- readLines(fptr, n = 1)
    }
    return(vars)
  }

  tick <- function(fptr, t_n) {
    line <- readLines(fptr, n = 1)
    # basically, we read until we get an empty line or another timestamp
    while (length(line) > 0 && substring(line, 1, 1) != "#") {
      # lines either store a bit or a word, we parse differently
      if (grepl(" ", line)) { # word
        parts <- strsplit(substring(line, 2), " ")[[1]]
        val <- parts[1]
        var <- trimws(parts[2])
      } else { # bit
        val <- substring(line, 1, 1)
        var <- trimws(substring(line, 2))
      }
      # once parsed, verify its a unique register then store it
      if (var %in% names(t_n)) {
        # special case - just log non-numerics as -1
        # mostly happens during initialization
        if (grepl("^[0-9]+$", val)) {
          t_n[var] <- as.integer(strtoi(val, base = 2))
        } else {
          t_n[var] <- -1
        }
      }
      line <- readLines(fptr, n = 1)
    }
    # not sure how R works, but basically either we read another time stamp or a nothing
    # have to signal this to the calling function
    # ?TODO - refactor this? don't actually know enough R to handle idiomatically
    if (length(line) == 0) {
      return(list(done=TRUE))
    } else {
      return(list(time=line, vals=t_n))
    }
  }

  vcd2df <- function(f_name) {
      # Basically, do some hacks to read line-by-line, and skip to the good part (vars)
      fptr <- file(f_name, "r")
      line <- readLines(fptr, n = 1)
      while (!grepl("\\$scope", line)) {
          line <- readLines(fptr, n = 1)
      }

      # Read the variables and perform a few transposes
      # We need ids to rename the rows latter, but easier to use aliases atm.
      ids <- get_vars(fptr)
      # We make a df to store a time point
      t_n <- setNames(rep(-1, length(ids)), ids)
      df <- data.frame(t_n)
      
      while (!grepl("\\$dumpvars", readLines(fptr, n = 1))) {
        # Keep reading until $dumpvars is found
        # ?TODO - grab the timescale here then transform the ticks?
        # I don't think that ever matters (relative time is fine)
      }

      # Now we have the variables, we can read the dumpvars
      # We know there's a #0 somewhere, so...
      time <- '#0'
      while (TRUE) {
        list <- tick(fptr, t_n)
        if (length(list) == 2) {
          # We have to update or single tick df and our result df, I guess
          t_n <- list$vals
          df$vals <- list$vals
          # We name the new column by the old time, then update the time
          names(df)[ncol(df)] <- time
          time <- list$time
        } else {
          # We change from encodings back to reg names, then return
          close(fptr)
          rownames(df) <- names(ids)
          return(df[, -1])
        }
      }
  }

  if (!interactive()) {
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) == 1) {
      df <- vcd2df(args[1])
      name <- strsplit(basename(args[1]), "\\.")[[1]][1]
      saveRDS(df, paste0(name, ".rds"))
    } else {
      cat("Usage: Rscript vcd_df.R <filename>.vcd\n")
    }
  }

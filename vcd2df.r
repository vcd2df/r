#' Read a value change dump (VCD) file and convert it to a data frame.
#' 
#' @param f_name The path of a VCD (value change dump) file.
#' @returns A data frame with the values of the variables at each time point.
#' @examples
#' vcd2df("example.vcd")
#' vcd2df("testbench.vcd")
vcd2df <- function(f_name) {
  # Basically, do some hacks to read line-by-line, and skip to the good part (vars)
  fptr <- file(f_name, "r")
  line <- readLines(fptr, n = 1)
  while (!grepl("\\$scope", line)) {
      line <- readLines(fptr, n = 1)
  }

  # Read the variables and perform a few transposes
  # We need vars to rename the rows latter, but easier to use aliases atm.
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
  # We make a df to store a time point
  t_n <- setNames(rep(-1, length(vars)), vars)
  df <- data.frame(t_n)
  
  while (!grepl("\\$dumpvars", readLines(fptr, n = 1))) {
    # Keep reading until $dumpvars is found
  }

  # Now we have the variables, we can read the dumpvars
  # We know there's a #0 somewhere, so...
  time <- '#0'
  while (TRUE) {
    while (length(line) > 0 && substring(line, 1, 1) != "#") {
      # lines either store a bit or a word, we parse differently
      if (grepl(" ", line)) { 
        # word
        parts <- strsplit(substring(line, 2), " ")[[1]]
        val <- parts[1]
        var <- trimws(parts[2])
      } else { 
        # bit
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
    # We have to update or single tick df and our result df, I guess
    df$vals <- t_n
    # We name the new column by the old time, then update the time
    names(df)[ncol(df)] <- time
    time <- line
    if (length(line) != 0) {
      # If we got another timestamp, we proceed.
      line <- readLines(fptr, n = 1)
    } else {
      # Or we change from encodings back to reg names, then return
      close(fptr)
      rownames(df) <- names(vars)
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

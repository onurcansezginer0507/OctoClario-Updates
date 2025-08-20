# Helper function to generate well names (A1, A2, ..., H12) (KEEP THIS)
generate_well_names <- function() {
  rows <- LETTERS[1:8]
  cols <- sprintf("%02d", 1:12)
  wells <- c()
  for (row in rows) {
    for (col in cols) {
      wells <- c(wells, paste0(row, as.integer(col)))
    }
  }
  return(wells)
}
all_wells <- generate_well_names()
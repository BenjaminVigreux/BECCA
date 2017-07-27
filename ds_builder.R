ds.builder <- function(rows, cols, dataset, perc = c(TRUE, FALSE)) {
  
  # Function to build descriptive statistics table.
  
  # rows <- table containing variables to be displayed by row.
  # cols <- table containing variables to be displayed by column.
  # dataset <- dataset from which to retrieve values.
  
  # Issues: cannot handle multiple type of cont variable on same dimension.
  
  if (missing(perc))
    perc <- FALSE
  
  # Get row and column variable names
  types <- c("Triads", "Dyads", "Stones", "Questions", "Descriptors")
  row.vars <- as.vector(as.matrix(rows[,match(types, colnames(rows), nomatch = 0)]))
  row.vars <- row.vars[!is.na(row.vars)]
  col.vars <- as.vector(as.matrix(cols[,match(types, colnames(cols), nomatch = 0)]))
  col.vars <- col.vars[!is.na(col.vars)]
  
  # Get continous variable ranges
  minmax <- c("Triads", "T Min", "T Max", "Dyads", "D Min", "D Max", "Stones", "S Min", "S Max")
  row.ranges <- NULL
  col.ranges <- NULL
  if (any(match(minmax, colnames(rows), nomatch = 0)))
    row.ranges <- rows[, match(minmax, colnames(rows), nomatch = 0)]
  if (any(match(minmax, colnames(cols), nomatch = 0)))
    col.ranges <- rows[, match(minmax, colnames(cols), nomatch = 0)]
  
  # Create DS Table
  nr <- length(row.vars)
  nc <- length(col.vars)
  ds.table <- matrix(NA, nrow = nr, ncol = nc)
  rownames(ds.table) <- row.vars
  colnames(ds.table) <- col.vars
  
  for (r in 1:dim(ds.table)[1]) {
    for (c in 1:dim(ds.table)[2]) {
      
      row <- dataset[, row.vars[r]]
      col <- dataset[, col.vars[c]]
      
      # Convert continuous variables to binomial ("Does observation fall within the range?")
      if (length(grep("^[TDS][0-9]", row.vars[r])))
        row <- ((row >= row.ranges[r, 2]) & (row <= row.ranges[r, 3]))
      if (length(grep("^[TDS][0-9]", col.vars[c])))
        col <- ((col >= col.ranges[c, 2]) & (col <= col.ranges[c, 3]))
      
      # Populate table
      tab <- table(row, col)
      if (dim(tab)[1] == 2)
        if (dim(tab)[2] == 2) {
          ds.table[r, c] <- tab[2, 2]
        } else {
          ds.table[r, c] <- tab[2, 1]
        }
      if (dim(tab)[1] == 1)
        if (dim(tab)[2] == 2) {
          ds.table[r, c] <- tab[1, 2]
        } else {
          ds.table[r, c] <- tab
        }
    }
  }
  
  # Convert to percentages
  if (perc) {
    p.table <- ds.table
    # Cell by cell
    for (r in 1:dim(p.table)[1]) {
      for (c in 1:dim(p.table)[2]) {
        
        # Columns
        col <- dataset[, colnames(p.table)[c]]
        # Continuous variables
        if (length(grep("^[TDS][0-9]", colnames(p.table)[c]))) {
          # Use remainder to get row number.
          nc <- match(colnames(p.table)[c], cols) %% dim(cols)[1]
          # Use quotient to get column number.
          nc <- c(nc, (match(colnames(p.table)[c], cols) %/% dim(cols)[1]) + 1)
          if (nc[1] == dim(cols)[1])
            nc[2] <- nc[2] - 1
          c.var.range <- c(cols[nc[1],nc[2]+1], cols[nc[1],nc[2]+2])
          col <- ((col >= c.var.range[1]) & (col <= c.var.range[2]))
        }
        p.table[r, c] <- p.table[r, c]/sum(col, na.rm = TRUE)
      }
    }
    p.table <- apply(p.table, 1:2, function(p.table)
      paste0(format(round(p.table * 100, 2), 2), "%"))
    return(p.table)
  } else {
    return(ds.table)
  }
}
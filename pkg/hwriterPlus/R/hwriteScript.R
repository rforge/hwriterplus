hwriteScript <- function(output, page = NULL, fontSize = "10pt",
                         trim = TRUE, ...,
                         link = NULL, name = NULL, heading = NULL,
                         center = NULL, br = NULL, div = NULL) {

  ## Purpose: Addition to hwriter to allow easy printing of R script
  ## ----------------------------------------------------------------------
  ## Arguments: Same as for hwriteOutput, plus a trim argument
  ## ----------------------------------------------------------------------
  ## Author: David Scott, Date: 22 August, 2012

  ## default arguments
  if (is.null(br)) br <- FALSE
  if (is.null(center)) center <- FALSE
  if (is.null(div)) div <- FALSE
  args <- list(...)

  ## Trim off first and last 2 lines of output from script
  if (trim) {
      nOut <- length(output)
      output <- output[-c(1,nOut-1,nOut)]
  }

  ## Add line endings
  output <- paste(output, collapse = "\n")
  ### Add tags for preformatting
  output <- paste("<pre style = 'font-size:", fontSize, "'>",
                  output, "</pre>", sep = "")

  hwrite(output, page = page, ..., link = link, name = name,
         heading = heading, center = center, br = br, div = div)

}


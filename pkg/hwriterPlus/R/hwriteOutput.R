hwriteOutput <- function(output, page = NULL, fontSize = "10pt", ...,
                         link = NULL, name = NULL, heading = NULL,
                         center = NULL, br = NULL, div = NULL) {

  ## Purpose: Addition to hwriter to allow easy printing of R output
  ## ----------------------------------------------------------------------
  ## Arguments: Same as for hwriteString, plus fontSize argument
  ## ----------------------------------------------------------------------
  ## Author: David Scott, Date: 24 Oct 2010, 12:23

  ## default arguments
  if (is.null(br)) br <- FALSE
  if (is.null(center)) center <- FALSE
  if (is.null(div)) div <- FALSE
  args <- list(...)

  ## Add line endings
  output <- paste(output, collapse = "\n")
  ### Add tags for preformatting
  output <- paste("<pre style = 'font-size:", fontSize, "'>",
                  output, "</pre>", sep = "")

  hwrite(output, page = page, ..., link = link, name = name,
         heading = heading, center = center, br = br, div = div)

}


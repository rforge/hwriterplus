hwriteSVG <- function(image.url, page = NULL,
                      width = 500, height = 500,
                      attributes = "border = '0'", ...) {

    ## Base string
    str <- "<!--[if !IE]>-->
             <object data = @imageData type = 'image/svg+xml'
	             width = @imageWidth height = @imageHeight
                     id = @imageID @imageAttributes> <!--<![endif]-->
            <!--[if lt IE 9]>
              <object src = @imageData classid = 'image/svg+xml'
	              width = @imageWidth height = @imageHeight
                      id = @imageID @imageAttributes> <![endif]-->
            <!--[if gte IE 9]>
              <object data = @imageData type = 'image/svg+xml'
	              width = @imageWidth height = @imageHeight
                      id = @imageID @imageAttributes> <![endif]-->
            </object>"

    ## Substitute values in the string
    imageData <- paste("'", image.url, "'", sep = "")
    str <- gsub("@imageData", imageData, str)
    imageWidth <- paste("'", width, "'", sep = "")
    str <- gsub("@imageWidth", imageWidth, str)
    imageHeight <- paste("'", height, "'", sep = "")
    str <- gsub("@imageHeight", imageHeight, str)
    str <- gsub("@imageAttributes", attributes, str)

  ## final
  hwrite(str, page, ...)
}

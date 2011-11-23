newPage <- function(filename, dirname = NULL, title = filename,
                    doctype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n",
                    xmlns = "xmlns='http://www.w3.org/1999/xhtml'",
                    link.javascript = NULL, link.css = NULL, css = NULL,
                    head = NULL, charset = "utf-8", lang = "en",
                    head.attributes = NULL, body.attributes = NULL)
{
    if (!is.null(dirname)) {
        if (!file.exists(dirname))
            dir.create(dirname, rec = TRUE, showWar = FALSE)
        filename <- file.path(dirname, filename)
    }
    page <- file(filename, "wt")
    meta <- hmakeTag("meta", NULL, `http-equiv` = "Content-Type",
                     content = paste("text/html; charset = ", charset,
                     sep = ""),
                     newline = FALSE)
    if (!is.null(link.javascript))
        link.javascript <- paste(hmakeTag("script", language = "JavaScript",
                                          src = link.javascript),
                                 collapse = "\n")
    if (!is.null(link.css))
        link.css <- paste(hmakeTag("link", rel = "stylesheet",
                                   type = "text/css", href = link.css),
                          collapse = "\n")
    if (!is.null(css))
        css <- paste(hmakeTag("style", css), collapse = "\n")
    numbering <- hmakeTag("script", "nequations=0;", newline = TRUE)
    head <- paste(meta, hmakeTag("title", title), head, link.javascript,
                  link.css, css, numbering, sep = "\n")
    head <- do.call(hmakeTag, c(list("head", head, newline = TRUE),
                                head.attributes))
    bodyStart <- do.call(hmakeTag, c(list("body", NULL), body.attributes))
    bodyStart <- substr(bodyStart, 1, regexpr("</body>", bodyStart) - 1)
    hwrite(paste(doctype, "<html ", xmlns, " xml:lang = '", lang, "' lang = '",
                 lang, "'>", head, bodyStart, sep = ""), page)
    page
}

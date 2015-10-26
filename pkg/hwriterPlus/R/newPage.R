### Set up new environment for equation list and equation number counter



newPage <- function(filename, dirname = NULL, title = filename,
                    doctype = "<!DOCTYPE html>\n",
                    link.javascript = NULL, link.css = NULL, css = NULL,
                    head = NULL, head.attributes = NULL,
                    body.attributes = NULL)
{
    ## today <- format(strptime(date(), "%a %b %d %H:%M:%S %Y"), "%B %d, %Y")

    ## Create equation numbers and list for labels
    ## Original version used global assignment
    hwriterEquation <<- 0
    hwriterEquationList <<- character(0)
    ## Set up new environment for equation list and equation number counter
    ## .hwriterGlobalEnv <- new.env()
    ## assign("hwriterEquation", 0, .hwriterGlobalEnv)
    ## assign("hwriterEquationList", character(0), .hwriterGlobalEnv)
    hwriteLatex <- function(ltx, page = NULL,
                            table.attributes = NULL,
                            tr.attributes = NULL,
                            td.attributes = NULL, ...)
        {
            ## cat is used to write to files,
            ##deal with output to standard output
            if (is.null(page)) page = ""
            ## Note: no append argument as it could ONLY be happened to work...
            ## count: add a (#)
            ## label: add before: Equation (#): label
            if (! is(ltx, "latex")) ltx <- as.latex(ltx)
            if (ltx$inline){
                ## inline: directly there in the text
                ##(can't be counted or labeled)
                cat(paste("\\(", ltx$alt,"\\)", sep = ""),
                    file = page, append = TRUE, sep = " ")
            } else {
                ## not inline: own living space
                ## (will be within a table to center it)
                if (ltx$count) {
                    ## update counter hwriterEquation
                    hwriterEquation <<- hwriterEquation + 1
                    ## eqnNum <- get("hwriterEquation", .hwriterGlobalEnv)
                    eqnNum <- eqnNum + 1
                    ## assign("hwriterEquation", eqnNum, .hwriterGlobalEnv)
                    ## deal with label
                    if (is.null(ltx$label)){
                        hwriterEquationList[hwriterEquation] <<-
                            paste("eq:", hwriterEquation, sep = "")
                        ## eqnLabel <- paste("eq:", eqnNum, sep = "")
                    } else {
                        hwriterEquationList[hwriterEquation] <<-
                            paste("eq:", ltx$label, sep = "")
                        ## eqnLabel <- paste("eq:", ltx$label, sep = "")
                    }
                    ## eqnList <- get("hwriterEquationList", .hwriterGlobalEnv)
                    ## eqnList[eqnNum] <- eqnLabel
                    ## assign("hwriterEquationList", eqnList, .hwriterGlobalEnv)
                    ## write out equation as table with equation number
                    if (is.null(table.attributes)){
                        table.attributes <- "border = '0' width = '90%'"
                    }
                    if (is.null(td.attributes)){
                        td.attributes <- c("width = '50'",
                                           "align = 'center'",
                                           "align = 'right' width = '50'")

                    }
                    cat(paste("\n<br /><center><table ",
                              table.attributes, "><tr ",
                              tr.attributes, "><td ",
                              td.attributes[1], ">&nbsp;</td><td ",
                              td.attributes[2], ">\\[",
                              ltx$alt, "\\]</td><td ",
                              td.attributes[3], " id = '",
                              eqnLabel,
                              "'>(", eqnNum,
                              ")</td></tr></table></center><br />",
                              sep = ""),
                        file = page, append = TRUE)
                } else {
                    if (is.null(table.attributes)){
                        table.attributes <- "border = '0'"
                    }
                    cat(paste("<br /><center><table ",
                              table.attributes, "><tr ",
                              tr.attributes, "><td align = 'center'>\\[",
                              ltx$alt, "\\]</td></tr></table></center><br />",
                              sep = ""),
                        file = page, append = TRUE)
                }
            }
        }

    if (!is.null(dirname)) {
        if (!file.exists(dirname))
            dir.create(dirname, recursive = TRUE, showWarnings = FALSE)
        filename <- file.path(dirname, filename)
    }
    page <- file(filename, "wt")
    if (!is.null(link.javascript))
        link.javascript <- paste(hmakeTag("script", type = "text/javascript",
                                          src = link.javascript),
                                 collapse = "\n")
    if (!is.null(link.css))
        link.css <- paste(hmakeTag("link", rel = "stylesheet",
                                   type = "text/css", href = link.css),
                          collapse = "\n")
    if (!is.null(css))
        css <- paste(hmakeTag("style", css), collapse = "\n")
    head <- paste(hmakeTag("title", title), head, link.javascript,
                  link.css, css, sep = "\n")
    head <- do.call(hmakeTag, c(list("head", head, newline = TRUE),
                                head.attributes))
    bodyStart <- do.call(hmakeTag, c(list("body", NULL), body.attributes))
    bodyStart <- substr(bodyStart, 1, regexpr("</body>", bodyStart) - 1)
    hwrite(paste(doctype, "<html ", ">", head, bodyStart, sep = ""), page)

    list(p = page, hwriteLaTeX = hwriteLaTeX)
}

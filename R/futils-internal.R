.First <- function () {
    if (interactive()) {
        invisible({
            lapply(c("utils", "colorout"), function(x) suppressMessages(library(x, 
                character.only = TRUE)))
        })
        options(menu.graphics = FALSE)
        options(width = 158)
        options(setWidthOnResize = TRUE)
        setHook(packageEvent("grDevices", "onLoad"), function(...) grDevices::X11.options(bg = "beige"))
        loadhistory(file = "~/.Rhistory")
    }
    source("~/code/r/lib.r")
}

.Last <- function () {
    if (interactive()) {
        savehistory("~/.Rhistory")
        message(".Last ")
    }
}

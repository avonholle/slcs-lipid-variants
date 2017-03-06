# make-markdown-file.R
# make a markdown file
# -----------------------

library("rmarkdown")
render("temp.rmd", run_pandoc = F)

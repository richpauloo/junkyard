library(magick)

st <- Sys.time()

#report <- image_read_pdf("https://cran.r-project.org/web/packages/pdftools/pdftools.pdf")
report <- image_read_pdf("C:/Users/rpauloo/Documents/Github/junkyard/mueller/mueller-report-searchable.pdf")

# add page numbers
for(i in 1:length(report)){
  report[i] <- image_annotate(report[i], paste0("page ", i, "/", length(report)), size = 200, gravity = "northwest", color = "pink")
}

image_write(report, "C:/Users/rpauloo/Documents/Github/junkyard/mueller/mueller_pg.gif", format = "gif")

Sys.time() - st

beepr::beep(8)

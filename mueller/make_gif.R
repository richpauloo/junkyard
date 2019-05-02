library(magick)

st <- Sys.time()

report <- image_read_pdf("C:/Users/rpauloo/Desktop/mueller/mueller-report-searchable.pdf")

# add page numbers
for(i in 1:length(report)){
  report[i] <- image_annotate(report[i], paste0("page ", i, "/", length(report)), size = 200, gravity = "southwest", color = "pink")
}


anim <- image_animate(report)
image_write(anim, "C:/Users/rpauloo/Desktop/mueller/mueller_pg.gif")


Sys.time() - st

beepr::beep(8)

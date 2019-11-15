library(moveVis)
library(move)
library(raster)
library(ggplot2)

data("move_data")


unique(timestamps(move_data))
timeLag(move_data, unit = "mins")

# align movement timestamps
move_data <- align_move(move_data, res = 240, digit = 0, unit = "secs")

# make frames
frames <- frames_spatial(move_data, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5)


length(frames) # number of frames
frames[[100]] # display one of the frames


animate_frames(frames, out_file = "~/example_1.gif")

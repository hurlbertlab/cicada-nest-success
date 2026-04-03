##########################
#
# Nestwatch files are too big to be stored in github regular storage
# so they need to be unzipped for local use in this repository
#
##########################

inuse_nestwatch <- "data/nestwatchV3/"
unzip <- list.files(path = inuse_nestwatch, pattern = ".zip$", full.names = TRUE)

unzip(zipfile = unzip, 
      overwrite = TRUE,
      exdir = inuse_nestwatch)

#done! now you can run everything else in the repo that relies on the nestwatch files.
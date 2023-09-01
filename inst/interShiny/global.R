all_objects=ls()
data.frames_only <- all_objects[sapply(mget(all_objects), class) == "data.frame"]
data.frames_only=as.vector(data.frames_only)
library(tidyverse)
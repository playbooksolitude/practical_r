#23-1016 mon 08:03

#
library(tidyverse)
mpg |> 
  group_by(displ) |> 
  reframe(mean_cyl = mean(cyl)) |> 
  #with(mean_cyl) |> 
  #summary()
  mutate(log = ifelse(mean_cyl > 6, 1, 0)) |> 
  ggplot(aes(x = displ, y = log)) +
  geom_point(position = position_jitter(width = .05, 
                                        height = .05)) +
  geom_smooth()


# flight ------------------------
library(nycflights13)
flights |> colnames()
flights |> 
  group_by(origin) |> 
  reframe(dest, distance)

#??????
flights |> 
  drop_na() |> 
  group_by(dest) |> 
  reframe(mean_delay = mean(dep_delay), 
          mean_distance = mean(distance)) |> 
  summary()

#????????? ??????
flights |> 
  drop_na() |> 
  group_by(dest) |> 
  reframe(mean_delay = mean(dep_delay), 
          mean_distance = mean(distance)) |> 
  ggplot(aes(x = mean_distance, y = mean_delay)) +
  geom_point(position = position_jitter(.5, .5))

flights |> 
  drop_na() |> 
  group_by(dest) |> 
  reframe(mean_delay = mean(dep_delay), 
          mean_distance = mean(distance)) |> 
  mutate(log = ifelse(mean_delay > 14.81, 1, 0)) |> 
  ggplot(aes(x = mean_distance, y = log)) +
  geom_point(position = position_jitter(width = .05,.05)) +
  geom_smooth(se = F) +
  annotate(geom = "text", x = 4000,
           y = 0.75, label = "?????? ?????? = 47", 
           size = 7) +
  annotate(geom = "text", x = 4000,
           y = 0.65, label = "?????? ?????? = 57", 
           size = 7)


flights |> 
  drop_na() |> 
  group_by(dest) |> 
  reframe(mean_delay = mean(dep_delay), 
          mean_distance = mean(distance)) |> 
  mutate(log = ifelse(mean_delay > 14.81, 1, 0)) |> 
  count(log)


# ??????
#????????? ??????  ##????????? ????????? ?????? ???????????? ????????????
flights |> 
  drop_na() |> 
  group_by(dest) |> 
  reframe(mean_delay = mean(dep_delay), 
          mean_distance = mean(distance)) |> 
  ggplot(aes(x = mean_distance, y = mean_delay)) +
  geom_point(position = position_jitter(.5, .5)) +
  geom_smooth(method = "lm", se = F) 

#
flights |> 
  drop_na() |> 
  group_by(dest) |> 
  reframe(mean_delay = mean(dep_delay), 
          mean_distance = mean(distance)) -> temp_lm 
lm(temp_lm$mean_delay, temp_lm$mean_distance)

lm(formula = mean_delay ~ mean_distance, 
   data = temp_lm) |> summary()






library(tidyverse)
library(zoo)
library(optotools)
library(ez)
library(broom)
library(stargazer)
library(dplyr)
library(jtools)
library(signal)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
source('stat_smooth_fun_pval.R')

# FUNCTIONS ---------------------------------------------------------------

filter <- dplyr::filter

span <- function(vec)
{
  diff(range(vec))
}

get_reaching <- function(is_moving){
  rle_result <- rle(is_moving)
  
  l <- rle_result$lengths
  v <- rle_result$values
  
  result_df <- tibble(l, v)
  
  largest <- result_df %>% 
    mutate(i = row_number()) %>% 
    filter(v) %>% 
    filter(l == max(l)) %>% 
    filter(row_number() == 1) %>% 
    pull(i)
  
  cumsum_l <- c(0, cumsum(l))
  start_reach <- 1 + cumsum_l[largest]
  end_reach <- cumsum_l[largest + 1]
  
  vec_len <- length(is_moving)
  
  is_reaching <- rep(FALSE, vec_len)
  is_reaching[start_reach:end_reach] <- TRUE
  
  return(is_reaching)
  
}

any_of <- function(x){
  if (unique(x) > 1){
    warning("column contains more than 1 unique value")
  }
  return(first(x))
}

lowpass_filter_func <- function(x){
  lowpass_filt <- signal::butter(8, 10/nyq, type = "low")
  signal::filtfilt(lowpass_filt, x)
}


mga_graph <- function(df_subset){
  
  tn <- first(df_subset$trial_num)
  ppid <- first(df_subset$ppid)
  dist <- first(df_subset$distance)
  width <- first(df_subset$width)
  devic <- first(df_subset$device)
  
  ttl <- sprintf("T: %s\nPPID: %s\nDIST: %scm  \nWIDTH:%s \nDEVICE:%s", tn, ppid, dist, width, devic)
  fname <- sprintf("%s_T%s_%s_%s_%s.png", ppid, tn, dist, width, devic)
  
  ggplot(df_subset, aes(x = time, y = ga, color = device)) + 
    geom_path(size = 1.2) + 
    geom_text(data = df_subset %>% select(mga_t, mga, device) %>% distinct(),
              aes(x = mga_t,
                  y = mga,
                  label = sprintf("S: %.3f, \nT: %.3f", mga, mga_t)),
              size = 4, 
              #nudge_x = 0.2,
              #nudge_y = 3,
              position =position_jitterdodge(dodge.width = 0.3,jitter.width = 0.2)) +
    labs(x = "Time (s)", y = "Grip Aperture (mm)", title = ttl) +
    coord_cartesian(ylim = c(0, 150)) +
    theme_bw()+
    ggsave(paste("mga_graphs_fil", fname, sep = "/"), type = "cairo-png")
}

vel_graph <- function(df_subset){
  
  tn <- first(df_subset$trial_num)
  ppid <- first(df_subset$ppid)
  dist <- first(df_subset$distance)
  width <- first(df_subset$width)
  devic <- first(df_subset$device)
  
  ttl <- sprintf("T: %s\nPPID: %s\nDIST: %scm  \nWIDTH:%s \nDEVICE:%s", tn, ppid, dist, width, devic)
  fname <- sprintf("%s_T%s_%s_%s_%s.png", ppid, tn, dist, width, devic)
  
  ggplot(df_subset, aes(x = time, y = wrist_vel, color = device)) + 
    geom_path(size = 1.2) + 
    geom_text(data = df_subset %>% select(vel_t, max_vel, device) %>% distinct(),
              aes(x = vel_t,
                  y = max_vel,
                  label = sprintf("S: %.3f, \nT: %.3f", max_vel, vel_t)),
              size = 4, 
              position =position_jitterdodge(dodge.width = 0.5,jitter.width = 0.2)) +
    labs(x = "Time (s)", y = "Velocity (cm/s)", title = ttl) +
    coord_cartesian(ylim = c(0, 1800)) + 
    theme_bw()+
    ggsave(paste("vel_graphs_fil", fname, sep = "/"), type = "cairo-png")
}

raincloud_theme <- theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 16),
  legend.position = "right",
  plot.title = element_text(lineheight = .8, face = "bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
  axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"))

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

read_and_filter <- function(file){
  
  movement <- read_ndi(file) 
  dt <- 1/ movement$freq
  
  df <- as.data.frame(movement) %>% 
    mutate_all(na.approx, na.rm = FALSE) %>% # lerp
    mutate(frame = 0:(movement$numframes-1))
  
  # pad data
  paddata <- 100

  data <- tibble(frame = (-paddata):(movement$numframes+paddata-1))
  
  data <- data %>%
    left_join(y = df, by = "frame") %>%
    fill(-frame, .direction = "down") %>% # padded NAs replaced with first/last position
    fill(-frame, .direction = "up")
  
  # filter  
  data <- data %>% 
    mutate_at(vars(-frame), function(x) filtfilt(filt = bf, x = x)) %>% 
    dplyr::filter(frame >= 0, frame < movement$numframes) %>% 
    mutate(time = frame * dt) %>% 
    select(-frame)
  
  return(data)
}

read_and_filter_big <- function(file){
  
  movement <- read_csv(file) 
  dt <- 1/ 60
  numframes = length(movement$m0_x)
  df <- as.data.frame(movement) %>% 
    mutate_at(vars(m0_x:m2_z), funs(ifelse(. == -999, NA, .))) %>% 
    mutate_at(vars(m0_x:m2_z), na.approx, na.rm = FALSE) %>% # lerp
    mutate(frame = 0:(numframes-1))
  # pad data
  paddata <- 100
  
  data <- tibble(frame = (-paddata):(numframes+paddata-1))
  
  data <- data %>%
    left_join(y = df, by = "frame") %>%
    fill(-frame, .direction = "down") %>% # padded NAs replaced with first/last position
    fill(-frame, .direction = "up")
  
  # filter  
  data <- data %>% 
    mutate_at(vars(-frame), function(x) filtfilt(filt = bf, x = x)) %>% 
    dplyr::filter(frame >= 0, frame < numframes) %>% 
    mutate(time = frame * dt) %>% 
    select(-frame)
  
  return(data)
}

read_and_time <- function(file){
  
  movement <- read_csv(file) 
  dt <- 1/ 60
  numframes = length(movement$m0_x)
  df <- as.data.frame(movement) %>% 
    mutate(frame = 0:(numframes-1))

  data <- df %>% 
    mutate(time = frame * dt) %>% 
    select(-frame)
  
  return(data)
}
# CONSTANTS ---------------------------------------------------------------
delta_t <- 0.0166666666667
fs <- 60
nyq <- fs/2
bf <- butter(1, 10/nyq , type="low")
# IMPORT ------------------------------------------------------------------
device1_file_filt <- tibble(files = list.files(recursive = TRUE, pattern = "^3d_fil(.*)csv$"))

device1_filt <- device1_file_filt %>% 
  mutate(data = map(files, read_and_time)) %>% 
  unnest(data, .drop = FALSE) %>% 
  separate(files, c("device", "ppid", "records","width_distance", "file_num"), sep = "/",extra='drop') %>% 
  select(-records) %>% 
  separate(width_distance, c("width","distance"),sep = "_") %>% 
  separate(file_num, c("rec","file_num"),sep = "_") %>% 
  select(-rec) %>% 
  mutate(distance=as.integer(distance),
         file_num = as.integer(file_num)) %>%
  group_by(ppid, width, distance) %>% 
  mutate(trial_num =  file_num - min(file_num)) %>% 
  select(-file_num) %>% 
  select(device,ppid,width,distance,trial_num,everything())%>% 
  data.frame()


opto_files <- tibble(files = list.files("OPTOTRAK",
                                        full.names = TRUE,
                                        recursive = TRUE, pattern = "^C#(.*)ROO$"))

optotrak <- opto_files %>% 
  mutate(move_data = map(files, read_and_filter), device = "OPTOTRAK") %>% 
  mutate(file_num = as.integer(substr(files, 12, 14))) %>% 
  left_join(read_csv("filenum.csv"), by = "file_num") %>% 
  unnest(move_data, .drop = FALSE) %>%
  rename_(.dots=setNames(names(.), tolower(gsub("1", "0", names(.))))) %>% 
  rename_(.dots=setNames(names(.), tolower(gsub("2", "1", names(.))))) %>% 
  rename_(.dots=setNames(names(.), tolower(gsub("3", "2", names(.))))) %>% 
  group_by(ppid, width, distance) %>% 
  mutate(trial_num =  file_num - min(file_num)) %>% 
  select(-files,-file_num) %>% 
  select(device,ppid,width,distance,trial_num,everything())

prehension_raw <- bind_rows(device1_filt, optotrak)

device1_filt %>%
  select(-device) %>% 
  left_join(optotrak %>% select(-device), by = c('ppid', 'distance', 'width', 'trial_num', 'time'), suffix = c(".bk", ".op")) %>% 
  write_csv("prehension_raw.csv")

# PROCESS -----------------------------------------------------------------

# CPD_data_output ---------------------------------------------------------

big_movement <- device1_filt %>%
  group_by(ppid, width, distance, trial_num, device) %>% 
  mutate(time = row_number() * delta_t,
         wrist_vel = c(NA, sqrt((diff(m2_x)^2 + diff(m2_y)^2 + diff(m2_z)^2))/diff(time))) %>% 
  dplyr::filter(!is.na(wrist_vel)) %>% 
  mutate(is_moving = wrist_vel >= 50,
         is_reaching = get_reaching(is_moving),
         movement_time = span(time[is_reaching]))%>%
  dplyr::filter(is_reaching) %>%
  select(-is_moving,-is_reaching,-movement_time,-wrist_vel,-time) %>% 
  mutate(time = row_number() * delta_t)

op_movement <- optotrak %>%
  group_by(ppid, width, distance, trial_num, device) %>% 
  mutate(time = row_number() * delta_t,
         wrist_vel = c(NA, sqrt((diff(m2_x)^2 + diff(m2_y)^2 + diff(m2_z)^2))/diff(time))) %>% 
  dplyr::filter(!is.na(wrist_vel)) %>% 
  mutate(is_moving = wrist_vel >= 50,
         is_reaching = get_reaching(is_moving),
         movement_time = span(time[is_reaching]))%>%
  dplyr::filter(is_reaching) %>%
  select(-is_moving,-is_reaching,-movement_time,-wrist_vel)%>% 
  mutate(time = row_number() * delta_t)

# Prehension data ---------------------------------------------------------

prehension_raw <- bind_rows(big_movement, op_movement)

#getting ppid, width, distance, trial num, and analysis
prehension_movement <-  prehension_raw %>%
  group_by(ppid, width, distance, trial_num, device) %>% 
  mutate(time = row_number() * delta_t,
         ga = ((m0_x - m1_x)^2 + (m0_y - m1_y)^2 + (m0_z - m1_z)^2)^0.5,
         mga = max(ga),
         mga_t = time[which.max(ga)],
         wrist_vel = c(NA, sqrt((diff(m2_x)^2 + diff(m2_y)^2 + diff(m2_z)^2))/diff(time)),
         max_vel = max(wrist_vel, na.rm = TRUE),
         vel_t = time[which.max(wrist_vel)],
         movement_time = span(time)) %>% 
  dplyr::filter(!is.na(wrist_vel)) %>% 
  write_csv('prehension_movement.csv')


prehension_movement %>% 
  group_by(ppid, trial_num, width, distance) %>% 
  do(mga_graph(.)) 
  
prehension_movement %>% 
  group_by(ppid, trial_num, width, distance) %>% 
  #filter(is_reaching) %>% 
  #mutate(time = time - min(time)) %>% 
  do(vel_graph(.)) 
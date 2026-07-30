rm(list=ls())
library(R.matlab)
source('./RL_plotting_utils.R')
data_root <- './data/lefebvre2017/'


# Experiment 1 ------------------------------------------------------------
fns <- Sys.glob(file.path(data_root, 'data_exp1/exp1_*.mat'))
dat <- NULL
for(fn in fns) {
  exp1_columns <- c('subjects', 'trials', 'condition', 'time1', 'time2', 'time3', 'choice_direction', 'reward', 'rt')
  data_ <- data.frame(readMat(fn)$data)
  colnames(data_) <- exp1_columns
  data_$subjects <- readMat(fn)$sub[1,1]
  dat <- rbind(dat, data_)
}

dat$subjects <- as.factor(dat$subjects)
dat$condition <-factor(dat$condition, levels = 1:4,
                       labels = c("25/25", "75/25", "25/75", "75/75"))
cond_map <- data.frame(condition = c("25/25", "75/25", "25/75", "75/75"),
                       s_left  = c("s1", "s3", "s5", "s7"),
                       s_right = c("s2", "s4", "s6", "s8"),
                       p_left  = c(0.25, 0.75, 0.25, 0.75),
                       p_right = c(0.25, 0.25, 0.75, 0.75),
                       stringsAsFactors = FALSE)
dat <- merge(dat, cond_map, by = "condition", all.x = TRUE)
dat$R <- factor(dat$choice_direction, levels=c(-1,1), labels=c('left', 'right'))
dat$rt <- dat$rt/1000
dat <- dat[order(dat$subjects,dat$trials),c('subjects', 'trials', 'condition',
                                            's_left', 's_right', 'p_left', 'p_right',
                                            'R', 'rt', 'reward')]
dat <- dat[!is.na(dat$R),]
dat <- dat[dat$rt>.15,]
dat$exposure <- get_exposure(dat)
save(dat, file=file.path(data_root, 'lefebvre_exp1.RData'))



# Experiment 2 ------------------------------------------------------------
fns <- Sys.glob(file.path(data_root, 'data_exp2/exp2_*.mat'))
dat <- NULL
for(fn in fns) {
  exp1_columns <- c('subjects', 'trials', 'condition', 'time1', 'choice_direction', 'reward', 'rt', 'money')
  data_ <- data.frame(readMat(fn)$data)
  colnames(data_) <- exp1_columns
  data_$subjects <- readMat(fn)$sub[1,1]
  dat <- rbind(dat, data_)
}

dat$subjects <- as.factor(dat$subjects)
dat$condition <-factor(dat$condition, levels = 1:4,
                       labels = c("25/25", "75/25", "25/75", "75/75"))
cond_map <- data.frame(condition = c("25/25", "75/25", "25/75", "75/75"),
                       s_left  = c("s1", "s3", "s5", "s7"),
                       s_right = c("s2", "s4", "s6", "s8"),
                       p_left  = c(0.25, 0.75, 0.25, 0.75),
                       p_right = c(0.25, 0.25, 0.75, 0.75),
                       stringsAsFactors = FALSE)
dat <- merge(dat, cond_map, by = "condition", all.x = TRUE)
dat$R <- factor(dat$choice_direction, levels=c(-1,1), labels=c('left', 'right'))
dat$rt <- dat$rt/1000
dat <- dat[order(dat$subjects,dat$trials),c('subjects', 'trials', 'condition',
                                            's_left', 's_right', 'p_left', 'p_right',
                                            'R', 'rt', 'reward', 'money')]
dat <- dat[!is.na(dat$R),]
dat <- dat[dat$rt>.15,]
dat$exposure <- get_exposure(dat)
save(dat, file=file.path(data_root, 'lefebvre_exp2.RData'))

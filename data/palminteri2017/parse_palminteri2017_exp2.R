library(R.matlab)
data_root <- './data/palminteri2017'

dat <- NULL
for(subject_n in 21:40) {
  session_dat <- NULL
  for(session_n in 1:2) {
    fn <- file.path(data_root, paste0('Online Scripts/data/Test', subject_n, '_Session', session_n, '.mat'))
    mat   <- readMat(fn)
    data_ <- data.frame(mat$data)
    gainA <- mat$gain.A  # 4 x 24: reward if left chosen
    gainB <- mat$gain.B  # 4 x 24: reward if right chosen
    
    colnames(data_) <- c('subject', 'session', 'trial', 'condition',
                         'col5',  # no idea?
                         'choice_lr', 'rt',
                         'outcome_factual', 'outcome_counterfactual',
                         # not sure about these?
                         'rt_confirmation?', 'timestamp_start', 'timestamp_end') #?
    data_$outcome_counterfactual <- data_$outcome_counterfactual*2-1
    data_$outcome_factual <- data_$outcome_factual*2-1
    data_$subject <- subject_n
    
    # Trial number within condition
    data_$trial_in_condition <- ave(data_$condition, data_$condition, FUN = seq_along)
    
    # Extract gain.A and gain.B per trial -- recode to -1, 1
    data_$reward_left <- mapply(function(cond, t) gainB[cond, t],
                                data_$condition, data_$trial_in_condition)*2-1
    data_$reward_right <- mapply(function(cond, t) gainA[cond, t],
                                 data_$condition, data_$trial_in_condition)*2-1
    
    # Drop col8 (redundant with outcome_factual) and timing columns if not needed
    data_ <- data_[, !colnames(data_) %in% c('col5', 'rt_confirmation', 'timestamp_start', 'timestamp_end')]
    
    # map conditions
    cond_map <- data.frame(condition = c(1, 2, 3, 4),
                           s_left  = paste0("s", c(1,3,5,7)+8*(session_n-1)),
                           s_right = paste0("s", c(2,4,6,8)+8*(session_n-1)),
                           stringsAsFactors = FALSE)
    data_ <- merge(data_, cond_map, by.x='condition', by.y='condition')
    
    data_$block <- session_n
    session_dat <- rbind(session_dat, data_)
  }
  dat <- rbind(dat, session_dat)
}

# Factors
dat$subjects   <- factor(dat$subject)
dat$session   <- factor(dat$session)
dat$condition <- factor(dat$condition)
dat$trials <- dat$trial

# RT in seconds
dat$rt <- dat$rt / 1000

# Reward probabilities
dat$p_left  <- NA
dat$p_right <- NA

dat[dat$condition == 1, c('p_left', 'p_right')] <- list(0.50, 0.50)
dat[dat$condition == 2, c('p_left', 'p_right')] <- list(0.25, 0.75)
dat[dat$condition == 3, c('p_left', 'p_right')] <- list(0.25, 0.75)

# Condition 4: reversal at trial 13
dat[dat$condition == 4 & dat$trial_in_condition <= 12, c('p_left', 'p_right')] <- list(0.17, 0.83)
dat[dat$condition == 4 & dat$trial_in_condition >= 13, c('p_left', 'p_right')] <- list(0.83, 0.17)

dat$condition_label <- factor(dat$condition,
                              levels = 1:4,
                              labels = c('Symmetric', 'Asymmetric', 'Asymmetric', 'Reversal'))

dat$exposure <- dat$trial_in_condition
## The matlab comment suggests 0 = left, 1 = right - but that almost has to be wrong!
dat$R <- factor(dat$choice_lr, levels=c(0, 1), labels=c('left', 'right'))
dat <- dat[order(dat$subjects, dat$block, dat$trials),]

## Couple of consistency checks
# checks
aggregate(reward_left==1~p_left*condition, dat, mean)
aggregate(reward_right==1~p_right*condition, dat, mean)

dat$factual_check <- ifelse(dat$choice_lr == 0, dat$reward_left, dat$reward_right)
mean(dat$factual_check == dat$outcome_factual) # Should be 1

dat <- dat[,c('subjects', 'block', 'condition', 'condition_label', 'R', 'rt', 's_left', 's_right', 'p_left', 'p_right', 'reward_left', 'reward_right', 'exposure')]
dat

save(dat, file='./data/palminteri2017_exp2.RData')

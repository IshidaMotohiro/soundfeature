library(tidyverse)
library(rlist)
library(seewave)
library(tuneR)

## �w�肵���f�B���N�g������S�Ă�wav�t�@�C���̃p�X���擾
path = "F:/User/Samples2/the drum club - kit 004 THE MEGA BUNDLE"
files_path <- path %>% list.files(pattern = "wav$", recursive=T, include.dirs=FALSE)
files_path <- str_c(path, files_path, sep = "/")
files_path %>% length()

## wav�t�@�C����ǂݍ���
for(i in length(files_path)){files_path[i] %>% readWave()}

dat <- lapply(files_path, readWave)

## �g�`���v���b�g
dID <- 2131
files_path[dID]

tbl <- as_tibble(dat[[dID]]@left, 1:length(dat[[dID]]@left))
tbl <- tbl %>% mutate(time = (1:length(dat[[dID]]@left) / dat[[dID]]@samp.rate))
tbl %>% ggplot(mapping = aes(x = time ,y = tbl$value)) + geom_line()

## �X�y�N�g���O�������v���b�g
spec_win <- dat[[dID]]@left %>% spectro(f = dat[[dID]]@samp.rate ,wl = dat[[dID]]@samp.rate / 441)

## seewave::spec�Ŏ��g���̓����ʂ��Z�o
proptest <- specprop(seewave::spec(dat[[dID]], f = dat[[dID]]@samp.rate, str = TRUE , plot = FALSE))
proptest
dat1 <- dat %>% head(100)

prop <- lapply(dat1,
               function(x){specprop(seewave::spec(x), f = x@samp.rate, str = TRUE, plot = FALSE)}
               )

prop_df <- 
  cbind(
    1:length(prop),
    prop %>% list.map(mean) %>% unlist() %>% as_data_frame(),
    prop %>% list.map(median) %>% unlist() %>% as_data_frame(),
    files_path[1:length(prop)]
  )

colnames(prop_df) <- c("dID", "mean", "median", "filepath")

prop_df %>% ggplot(mapping = aes(x = mean, y = median)) + geom_point()

prop_df %>% arrange(mean)
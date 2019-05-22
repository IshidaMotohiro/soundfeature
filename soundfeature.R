library(tidyverse)
library(rlist)
library(seewave)
library(tuneR)

## 指定したディレクトリから全てのwavファイルのパスを取得
path = ""
files_path <- path %>% list.files(pattern = "wav$", recursive=T, include.dirs=FALSE)
files_path <- str_c(path, files_path, sep = "/")
files_path %>% length()

## wavファイルを読み込む
## wavファイルの総サンプル数が1以下のものは読み込まない
i <- 1
dat <- NULL

for(i in 1:length(files_path)){
  tmp <-
    files_path[i] %>% readWave(header = TRUE)
  
  if (tmp$samples > 1){
    tmp2 <- files_path[i] %>% readWave(header = FALSE)
    dat <- c(dat, list(tmp2))
  }
}


# naIndex <- NULL
# dat <- lapply(files_path[1750:1760],
#               function(x){
#                 tmp <- readWave(x)
#                 tmp2 <- readWave(x, header = TRUE)
#                 if (tmp2$samples <= 1){
#                   tmp <- NA
#                   list.remove(x)
#                 }
#                 return(tmp)
#                 }
#               )
# dat %>% list.exclude(is.na() == TRUE)


## 波形をプロットしてみる
dID <- 1
files_path[dID] %>% readWave()

tbl <- as_tibble(dat[[dID]]@left, 1:length(dat[[dID]]@left))
tbl <- tbl %>% mutate(time = (1:length(dat[[dID]]@left) / dat[[dID]]@samp.rate))
tbl %>% ggplot(mapping = aes(x = time ,y = tbl$value)) + geom_line()

## スペクトログラムをプロットしてみる
spec_win <- dat[[dID]]@left %>% spectro(f = dat[[dID]]@samp.rate)

## seewave::specpropで周波数スペクトルを算出
freq_spec_test <- specprop(seewave::spec(dat[[dID]], f = dat[[dID]]@samp.rate, str = TRUE, plot = FALSE))
freq_spec_test
# dat1 <- dat %>% head(10)

freq_spec <- lapply(dat,
               function(x){try(specprop(spec(x), f = x@samp.rate))}
               )

## 周波数スペクトルのリストからmeanとmedianを抽出してデータフレームに変換
## 【4列目のfilepathはwaveファイルを読み込む際の破損データ除去処理の関係でズレているので要修正】
freq_spec_df <- 
  cbind(
    1:length(freq_spec),
    freq_spec %>% list.map(mean) %>% unlist() %>% as_tibble(),
    freq_spec %>% list.map(median) %>% unlist() %>% as_tibble(),
    files_path[1:length(freq_spec)]
  )

colnames(freq_spec_df) <- c("dID", "mean", "median", "filepath")

## あまり意味のないプロット
freq_spec_df %>% ggplot(mapping = aes(x = mean, y = median)) + geom_point()

freq_spec_df %>% arrange(mean)

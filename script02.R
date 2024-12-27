# Encoding SJIS
#ラッシュモデル後章スクリプト：目次
#分析結果確認環境:R2.15.2(32bit/2013/02/17)

#0.必要なパッケージの読み込み

#1.LLTM 応用例1 : 分数の引き算に関する分析(本文2.2節)
 #1.1:データの読み込みと確認
 #1.2:計画行列の作成
 #1.3:LLTMの実行
 #1.4:LLTMとラッシュモデルとの推定値の比較
 #1.5:その他にできること（本文未掲載）

#2.LLTM 応用例2 : 怒りの反応に関する調査（2値）(本文2.3節)
 #2.1:データの読み込みと確認
 #2.2:計画行列の作成
 #2.3:LLTMの実行
 #2.4:LLTMとラッシュモデルとの推定値の比較
 #2.5:その他にできること（本文未掲載）

#3.LRSM 応用例3 : 怒りの反応に関する調査（3値）(本文2.5節)
 #3.1:データの読み込みと確認
 #3.2:計画行列の作成
 #3.3:LRSMの実行
 #3.4:LRSMとRSMとの推定値の比較
 #3.5:その他にできること（本文未掲載）

#4.LPCM 応用例4 : 怒りの反応に関する調査（2値・3値）(本文2.7節)
 #4.1:データの読み込みと確認
 #4.2:計画行列の作成
 #4.3:LPCMの実行
 #4.4:LPCMとPCMとの推定値の比較
 #4.5:その他にできること（本文未掲載）



#0.必要なパッケージの読み込み ------------------------------------------------------
library(eRm)       #分析に必要な関数が入っているパッケージ"eRm"の読み込みです。
#-----------------------------------------------------------------------------------



### 1. LLTM 応用例1 : 分数の引き算に関する分析(本文2.2節)  ####---------------------

#1.1:データの指定と確認
#元データはpackage"CDM"中のサンプルデータ"fraction.subtraction.data"です。
dat<-read.csv("fraction.subtraction.data.csv",sep=",", header=T)
#内容の確認(データの内容，データの行数(生徒数)，列数(問題数))
head(dat);nrow(dat);ncol(dat)


#1.2:計画行列の作成(本文(2.8)式)
#元データはpackage"CDM"中の"fraction.subtraction.qmatrix"です。
WW <- matrix(c(
	0,0,0,1,0,1,1,0,
	0,0,0,1,0,0,1,0,
	0,0,0,1,0,0,1,0,
	0,1,1,0,1,0,1,0,
	0,1,0,1,0,0,1,1,
	0,0,0,0,0,0,1,0,
	1,1,0,0,0,0,1,0,
	0,0,0,0,0,0,1,0,
	0,1,0,0,0,0,0,0,
	0,1,0,0,1,0,1,1,
	0,1,0,0,1,0,1,0,
	0,0,0,0,0,0,1,1,
	0,1,0,1,1,0,1,0,
	0,1,0,0,0,0,1,0,
	1,0,0,0,0,0,1,0,
	0,1,0,0,0,0,1,0,
	0,1,0,0,1,0,1,0,
	0,1,0,0,1,1,1,0,
	1,1,1,0,1,0,1,0,
	0,1,1,0,1,0,1,0
		 ),nrow=20,ncol=8,byrow=T)
#計画行列の列名に基本母数名をあてる
colnames(WW) <- paste("alpha",1:8,sep="")
#作成した計画行列の確認
WW


#1.3:LLTMの実行（結果は容易度を表すため，困難度と符号が逆となる点に注意）
#関数LLTM()の引数
#X:分析に使用する0,1の2値データを指定する引数
#W:関数LLTMに指定する計画行列，指定しない場合は，通常のラッシュモデルとなるよう自動で計画行列が生成される
#se:Se=Tとした場合，各推定値の標準誤差が計算される
lltmres <- LLTM(X=dat,W=WW,se=T)
#結果の表示(本文 表2.3, 2.4)
summary(lltmres)

#1.4:LLTMとラッシュモデルとの推定値の比較(本文 図2.1:左上)
rmres    <- RM(dat)　#ラッシュモデルで項目母数を推定する
rmbeta   <- rmres$betapar #ラッシュモデルの推定値の取り出し
lltmbeta <- scale(lltmres$betapar,scale=F) #LLTMの結果の中心化
#両者の結果の描画
par(ps=16)
plot(rmbeta,lltmbeta, col="black", lwd=2,lty=1,
     xlab="Rasch model's beta",ylab="LLTM model's beta",
     xlim=c(-3,3),ylim=c(-3,3))
abline(0,1,col="black", lwd=2,lty=1)
#両者の推定値の相関
cor(rmbeta,lltmbeta)


#1.5:その他にできること（本文未掲載）
#被験者母数の推定(正解した数に対応する被験者母数の推定値を表示します)
perparLLTM <- person.parameter(lltmres)
#情報量規準の算出
IC(perparLLTM)






### 2. LLTM 応用例2 : 怒りの反応に関する調査（2値）(本文2.3節) ####--------------------

#2.1:データの読み込みと確認
#元データはpackage"lme4"中のサンプルデータ"VerbAgg"です。
dat2<-read.csv("VerbAggdata2.csv",sep=",", header=T)
head(dat2);nrow(dat2);ncol(dat2)
#作成したデータの項目名の確認
colnames(dat2)



#2.2:計画行列の作成(本文(2.10)式)
WW <- matrix(c(
		0,0,0,
		0,1,0,
		0,0,1,
		1,0,0,
		1,1,0,
		1,0,1
		 ),nrow=6,ncol=3,byrow=T)
#計画行列の列名に基本母数名をあてる
colnames(WW) <- paste("alpha",1:3,sep="")
#作成した計画行列の確認
WW



#2.3:LLTMの実行（結果は容易度を表すため，困難度と符号が逆となる点に注意）
lltmres2 <- LLTM(dat2[,c(1,2,3,7,8,9)],W=WW,sum0=T,se=T)
#結果の表示(本文 表2.7, 2.8)
summary(lltmres2)


#2.4:LLTMとラッシュモデルとの推定値の比較(本文 図2.1:右上)
rmres2    <- RM(dat2[,c(1,2,3,7,8,9)])　#ラッシュモデルで項目母数を推定する
rmbeta2   <- rmres2$betapar #ラッシュモデルの推定値の取り出し
lltmbeta2 <- scale(lltmres2$betapar,scale=F) #LLTMの結果の中心化
#両者の結果の描画
par(ps=16)
plot(rmbeta2,lltmbeta2, col="black", lwd=2,lty=1,
     xlab="Rasch model's beta",ylab="LLTM model's beta",
     xlim=c(-3,3),ylim=c(-3,3))
abline(0,1,col="black", lwd=2,lty=1)
#両者の推定値の相関
cor(rmbeta2,lltmbeta2)


#2.5:その他にできること（本文未掲載）
#被験者母数の推定
perparLLTM2 <- person.parameter(lltmres2)
#情報量規準の算出
IC(perparLLTM2)







### 3. LRSM 応用例3 : 怒りの反応に関する調査（3値）(本文2.5節) ####--------------------
#3.1:データの読み込みと確認
dat3<-read.csv("VerbAggdata3.csv",sep=",", header=T)
#データの確認
head(dat3);nrow(dat3);ncol(dat3)
#データの項目名の確認
colnames(dat3)



#3.2:計画行列の作成(package"eRm"の関数LRSM用の計画行列の指定のルールについては，別紙の参照をお願いします。)
WW <- matrix(c(
		0,0,0,0,
		0,0,0,1,
		0,1,0,0,
		0,2,0,1,
		0,0,1,0,
		0,0,2,1,
		1,0,0,0,
		2,0,0,1,
		1,1,0,0,
		2,2,0,1,
		1,0,1,0,
		2,0,2,1
		 ),nrow=12,ncol=4,byrow=T)
#計画行列の列名に基本母数名とカテゴリ係数名をあてる
colnames(WW) <- c(paste("alpha",1:3,sep=""),"omega2")
#作成した計画行列の確認
WW



#3.3:LRSMの実行（結果は容易度を表すため，困難度と符号が逆となる点に注意）
#関数LRSM()の引数
#X:分析に使用するデータを指定する引数
#W:関数LRSMに指定する計画行列，指定しない場合は，RSMとなるよう自動で計画行列が生成される
#se:Se=Tとした場合，各推定値の標準誤差が計算される
lrsmres <- LRSM(X=dat3[,c(4,5,6,10,11,12)],W=WW,se=T)
#結果の表示(本文 表2.9, 2.10)
summary(lrsmres)



#3.4:LRSMとRSMとの推定値の比較(本文 図2.1:左下)
rsmres   <- RSM(dat3[,c(4,5,6,10,11,12)]) #RSMで項目母数を推定する
rsmbeta  <- rsmres$betapar #RSMの推定値の取り出し
lrsmbeta <- scale(lrsmres$betapar,scale=F) #LRSMの結果の中心化
#両者の結果の描画
par(ps=16)
plot(rsmbeta,lrsmbeta, col="black", lwd=2,lty=1,
     xlab="RSM model's beta",ylab="LRSM model's beta",
     xlim=c(-3,3),ylim=c(-3,3))
abline(0,1,col="black", lwd=2,lty=1)
#両者の推定値の相関
cor(rsmbeta,lrsmbeta)




#3.5:その他にできること（本文未掲載）
#被験者母数の推定
perparLRSM <- person.parameter(lrsmres)
#情報量規準の算出
IC(perparLRSM)










### 4. LPCM 応用例4 : 怒りの反応に関する調査（2値・3値）(本文2.7節) ####----------------
#4.1:データの読み込みと確認
dat4<-read.csv("VerbAggdata4.csv",sep=",", header=T)
#データの確認
head(dat4)
#データの項目名の確認
colnames(dat4)




#4.2:計画行列の作成(package"eRm"の関数LPCM用の計画行列の指定のルールについては，別紙の参照をお願いします。)
WW <- matrix(c(
		0,0,0,0,0,0,
		0,1,0,0,0,0,
		0,0,1,0,0,0,
		1,0,0,0,0,0,
		2,0,0,1,0,0,
		1,1,0,0,0,0,
		2,2,0,0,1,0,
		1,0,1,0,0,0,
		2,0,2,0,0,1
		 ),nrow=9,ncol=6,byrow=T)
#計画行列の列名に基本母数名と閾値母数名をあてる
colnames(WW) <- c(paste("alpha",1:3,sep=""),d192,d202,d212)
#作成した計画行列の確認
WW



#4.3:LPCMの実行（結果は容易度を表すため，困難度と符号が逆となる点に注意）
#関数LPCM()の引数
#X:分析に使用するデータを指定する引数
#W:関数LPCMに指定する計画行列，指定しない場合は，PCMとなるよう自動で計画行列が生成される
#se:Se=Tとした場合，各推定値の標準誤差が計算される
lpcmres <- LPCM(dat4,W=WW,se=T)
#結果の表示(本文 表2.11, 2.12)
summary(lpcmres)



#4.4:LPCMとPCMとの推定値の比較(本文 図2.1:右下)
pcmres   <- PCM(dat4)　#PCMで項目母数を推定する
pcmbeta  <- pcmres$betapar  #PCMの推定値の取り出し
lpcmbeta <- scale(lpcmres$betapar,scale=F) #LPCMの結果の中心化
#両者の結果の描画
par(ps=16)
plot(pcmbeta,lpcmbeta, col="black", lwd=2,lty=1,
     xlab="PCM model's beta",ylab="LPCM model's beta",
     xlim=c(-3,3),ylim=c(-3,3))
abline(0,1,col="black", lwd=2,lty=1)
#両者の推定値の相関
cor(pcmbeta,lpcmbeta)



#4.5:その他にできること（本文未掲載）
#被験者母数の推定
perparLPCM <- person.parameter(lpcmres)
#情報量規準の算出
IC(perparLPCM)








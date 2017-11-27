library(ggplot2)
library(dplyr)
library(Rcpp)
library(GGally)
library(DT)
#library(markdown)
library(ggcorrplot)
library(reshape2)
library(broom)
library(robustbase)
library(car)

#loadfonts(quiet = TRUE)

# Define UI for application that draws a scatterplot
#small category label database
cate_lab <- read.table("scate_list.tsv",sep="\t",header=T,fileEncoding="utf-8")
fin.1<-"scate_weekly_click.csv"
fin.2<-"scate_weekly_cv.csv"
fin.3<-"scate_weekly_clickuu.csv"
fin.4<-"scate_weekly_cvuu.csv"
fin.5<-"scate_weekly_media.csv"

df_cl <- read.csv(fin.1,header=T,sep=",")
df_cv <- read.csv(fin.2,header=T,sep=",")
df_cluu <- read.csv(fin.3,header=T,sep=",")
df_cvuu <- read.csv(fin.4,header=T,sep=",")
df_m <- read.csv(fin.5,header=T,sep=",")

#- merge KPI file and create dataset
df <- left_join(x=df_cl,y=df_cv,by=c("category_low_id"="category_low_id","week_num"="week_num","region"="region"))
df <- left_join(x=df,y=df_cluu,by=c("category_low_id"="category_low_id","week_num"="week_num","region"="region"))
df <- left_join(x=df,y=df_cvuu,by=c("category_low_id"="category_low_id","week_num"="week_num","region"="region"))
df <- left_join(x=df,y=df_m,by=c("category_low_id"="category_low_id","week_num"="week_num","region"="region"))

#- 2017/1/1(=week_num = 52となってしまう週は削除する)
df <- df %>% dplyr::filter(week_num != 52)
#df <- df %>% dplyr::rename(cl_cnt=cl_rec_cnt)
#df <- df %>% dplyr::rename(cl_uu=cl_u)

#-- convert NA to 0
df[is.na(df)]<-0
#--calculate cvr
df <- df %>% dplyr::mutate(cvr=cv_cnt/cl_cnt)

#-- get max week_num from df
max_weeknum <- max(df$week_num)

#--read data of kpi min category
# min category label
mcate_lab <- read.table("mcate_list.csv",sep=",",header=T,fileEncoding="utf-8")
fin.6<-"mcate_weekly_click.csv"
fin.7<-"mcate_weekly_cv.csv"
fin.8<-"mcate_weekly_clickuu.csv"
fin.9<-"mcate_weekly_cvuu.csv"
fin.10<-"mcate_weekly_media.csv"

mdf_cl <- read.csv(fin.6,header=T,sep=",")
mdf_cv <- read.csv(fin.7,header=T,sep=",")
mdf_cluu <- read.csv(fin.8,header=T,sep=",")
mdf_cvuu <- read.csv(fin.9,header=T,sep=",")
mdf_m <- read.csv(fin.10,header=T,sep=",")

#- merge KPI dataset
dfm <- left_join(x=mdf_cl,y=mdf_cv,by=c("category_min_id"="category_min_id","week_num"="week_num","region"="region"))
dfm <- left_join(x=dfm,y=mdf_cluu,by=c("category_min_id"="category_min_id","week_num"="week_num","region"="region"))
dfm <- left_join(x=dfm,y=mdf_cvuu,by=c("category_min_id"="category_min_id","week_num"="week_num","region"="region"))
dfm <- left_join(x=dfm,y=mdf_m,by=c("category_min_id"="category_min_id","week_num"="week_num","region"="region"))

#--listup existing min categories in database
exist_mincate_list<-unique(dfm$category_min_id)
exist_mincate_df<-as.data.frame(unique(dfm$category_min_id))
colnames(exist_mincate_df)<-c("ex_mc")

#merge category label
mcate_exist <- inner_join(x=mcate_lab,y=exist_mincate_df,by=c("category_min_id"="ex_mc"))

mcate_exist <- mcate_exist %>% dplyr::mutate(choise_label=paste(as.character(category_min_id)
                                                                ,category_min_name,sep="_"))
#- 2017/1/1(=week_num = 52となってしまう週は削除する)
dfm <- dfm %>% dplyr::filter(week_num != 52)
#-- convert NA to 0
dfm[is.na(dfm)]<-0
#-- calculate CVR
dfm <- dfm %>% dplyr::mutate(cvr=cv_cnt/cl_cnt)    


#small category list(all)
cateID_list<-cate_lab$category_low_id

#--listup existing small categories in database
#--crate Niigata list

region_df_niigata<- df %>% dplyr::filter(region =="Niigata") 
exist_scate_df_niigata <- as.data.frame(unique(region_df_niigata$category_low_id))

colnames(exist_scate_df_niigata)<-c("ex_sc")
  
#merge category label(small category name)
scate_exist_niigata <- inner_join(x=cate_lab,y=exist_scate_df_niigata,by=c("category_low_id"="ex_sc"))
  
scate_exist_niigata <- scate_exist_niigata %>% dplyr::mutate(choise_label=paste(as.character(category_low_id)
                                                                ,category_low_name,sep="_"))
  
#--crate Tokyo scate list

region_df_tokyo<- df %>% dplyr::filter(region =="Tokyo") 
exist_scate_df_tokyo <- as.data.frame(unique(region_df_tokyo$category_low_id))

colnames(exist_scate_df_tokyo)<-c("ex_sc")

#merge category label(small category name)
scate_exist_tokyo <- inner_join(x=cate_lab,y=exist_scate_df_tokyo,by=c("category_low_id"="ex_sc"))

scate_exist_tokyo <- scate_exist_niigata %>% dplyr::mutate(choise_label=paste(as.character(category_low_id)
                                                                                ,category_low_name,sep="_"))


#以下、server.Rに記載
#region_scate<- reactive({
#  #regionでフィルタしたdfを返す。
#  region_df<-switch(input$var_region_s,
#                    "Niigata"= scate_exist_niigata,
#                    "Tokyo"=scate_exist_tokyo)
#  return(region_df)
#})



#min category list(all)
#mincateID_list <- mcate_lab$category_min_id
#存在するカテゴリのリスト

#回帰分析の結果から、必要なパラメータをデータフレームに取得する関数
get_parameters<- function(res_rg){
  #summaryを取ったときに"Warning message:In summary.lm(obj) : essentially perfect fit: summary may be unreliable"とwarning
  message_str <- "simpleWarning in summary.lm(res_rg2): essentially perfect fit: summary may be unreliable\n"

  #lm.summaryがワーニングとなるかどうかを評価して、ワーニングだった場合は、ダミーのresponseデータを返す
  testerror <- tryCatch(summary(res_rg),
                      warning=function(w){
                        if(paste(w)==message_str){
                          print(paste(w)); 
                          "NaN"
                        }
                      }
              )
  
  if(class(testerror)=="summary.lm"){
    sum_res <- testerror
    #偏回帰係数の推定値、t値,t値の上側確率を取得
    out <- tidy(sum_res)[2,c("estimate","statistic","p.value")]
    #カラム名を変更
    colnames(out) <- c("estimate","t_statistic","t_pval")
    #調整済R2乗値,F値,F値の上側確率を取得
    out2 <- glance(sum_res)[1,c("adj.r.squared","statistic","p.value")]
    #カラム名を変更
    colnames(out2)<-c("r2","f_statistic","f_pval")
    #--- res_rgから直接偏回帰係数の信頼区間を取得
    #偏回帰係数の推定血の95%信頼区間を取得
    ci.value <- confint(res_rg)
    #信頼区間下限
    low_value <- ci.value[2,1]
    #信頼区間上限
    upper_value <- ci.value[2,2]
    out3 <- cbind(as.data.frame(low_value),as.data.frame(upper_value))
    colnames(out3) <- c("low_ci","upper_ci")
    #y切片を取得する
    out4 <- as.data.frame(tidy(sum_res)[1,]$estimate)
    #カラム名変更
    colnames(out4)<-c("intercept")
    
    #データフレームに繋げる
    out_total <- cbind(out,out4,out3,out2)
  }
  if(class(testerror) != "summary.lm"){
    out_total<-as.data.frame(matrix("NA",nrow=1,ncol=9))
    colnames(out_total)<-c("estimate","t_statistic","t_pval","intercept","low_ci","upper_ci","r2","f_statistic","f_pval")
  }
  return(out_total)
}


#ロバスト回帰分析の結果から、必要なパラメータをデータフレームに取得する関数
get_rob_parameters<- function(res_rg){
  #tryCatch()をメインに使ったことで、解がwarning(信頼できない)とメッセージがでるようなデータの場合は、ロバスト回帰の結果ではなく、
  # "NaN"をリターンさせるようにした。
  # 引数がlmrobオブジェクトでなければ、全てNAのデータオブジェクトを返すとした。
  if(class(res_rg)=="lmrob"){
    sum_res <-summary(res_rg)
              
    if(is.na(res_rg$coefficients[2]) | res_rg$scale==0){
      out_total<-as.data.frame(matrix("NA",nrow=1,ncol=9))
      colnames(out_total)<-c("estimate","t_statistic","t_pval","intercept","low_ci","upper_ci","r2","Chisq","model_pval")
    }
    else{
      #偏回帰係数の推定値、t値,t値の上側確率を取得
      out <- as.data.frame(sum_res$coefficients)[2,c("Estimate","t value","Pr(>|t|)")]
      #カラム名を変更
      colnames(out) <- c("estimate","t_statistic","t_pval")
      #調整済R2乗値,F値,F値の上側確率を取得
      out2_1 <- as.data.frame(sum_res$adj.r.squared)
      #カラム名を変更
      colnames(out2_1)<-c("r2")
      
      #anova_analysisを呼び出して、分散分析を行う。
      out2_2 <-anova_analysis(res_rg)
      out2<-cbind(out2_1,out2_2)
      #--- res_rgから直接偏回帰係数の信頼区間を取得
      #偏回帰係数の推定値の95%信頼区間を取得
      ci.value <- confint(res_rg)
      #信頼区間下限
      low_value <- ci.value[2,1]
      #信頼区間上限
      upper_value <- ci.value[2,2]
      out3 <- cbind(as.data.frame(low_value),as.data.frame(upper_value))
      colnames(out3) <- c("low_ci","upper_ci")
      #y切片を取得する
      out4 <- as.data.frame(sum_res$coefficients[1,c("Estimate")])
      #カラム名変更
      colnames(out4)<-c("intercept")
      
      #データフレームに繋げる
      out_total <- cbind(out,out4,out3,out2)
    }
  }
  # 受けとったオブジェクトが、"simpleWarning in lmrob.S(x, y, control = control, mf = mf): S-estimated scale == 0:  Probably exact fit; check your data\n"
  # のワーニングでデータの原因によりパラメータが求まらないような場合、"Nan"を受けとるので、リターンする内容を全て"NA"で返す
  else if (class(res_rg) != "lmrob"){
    out_total<-as.data.frame(matrix("NA",nrow=1,ncol=9))
    colnames(out_total)<-c("estimate","t_statistic","t_pval","intercept","low_ci","upper_ci","r2","Chisq","model_pval")
  }
  
  return(out_total)
  
}

anova_analysis<-function(reg){
# robustbase::lmrobの結果を受けて、切片だけのモデルと当該モデルの差があるかを評価するχ2検定を行う
#--modelの適合度検定
  #説明変数の偏回帰係数がNAの場合（=説明変数の分散が0の場合)Anovaはできないので、
  #また、同時に、説明変数の偏回帰係数が0のときは、以下のErrorが生じるので、避けた
  #Error in solve.default(vcov.hyp) : 
  #  Lapack routine dgesv: system is exactly singular: U[1,1] = 0 
    
    if(!is.na(reg$coefficients[2]) && reg$coefficients[1]!=0){
      anova.res <-as.data.frame(car::Anova(reg))
      teststat<-as.data.frame(anova.res[1,c("Chisq","Pr(>Chisq)")])
      
    } else{
      tmp<-matrix("NA",nrow=1,ncol=2)
      teststat<-as.data.frame(tmp)
    }
    colnames(teststat)<-c("Chisq","model_pval")
  return(teststat)
}

#g<-function(x){
#  e<-try(anova_analysis(x),silent=FALSE)
#  if(class(e)=="try-error"){
#    tmp<-matrix("NA",nrow=1,ncol=2)
#    xx<-as.data.frame(tmp)
#    colnames(xx)<-c("Chisq","model_pval")
#  }else{
#    xx<-anova_analysis(x)
#  }
#  return(xx)
#}


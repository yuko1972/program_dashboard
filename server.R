#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source('global.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  #--low category: scatterPlot display
  #return low_category_code from input variable
  sm_id_input <- reactive({
    tmp_id <- subset(scate_exist,choise_label==input$var_sm)
    tmp_id$category_low_id
    #tmp_id : integer
  })
  
  #小カテ＿散布図ページで選択された小カテゴリのプログラム名リストをUIに出力する。
  #これは、htmlOutput("select_program")で使った関数
  #output$select_program <- renderUI({
  #  selectInput("program_Name","プログラムを選んで下さい。",
  #              choices = merchant_scate_list()
  #              )
  #})
  
  #散布図_小カテページの小カテが更新されると、同時にSelectInputのchoiceの内容を変える。
  # observe({
  #   x<- input$var_sm
  #   set_m <- merchant_scate_list()
  #   
  #   if( is.null(x))
  #     x <- character(0)
  #   
  #   scateid<- sm_id_input()
  #   updateSelectInput(session,"select_program_up",
  #                     label=paste("プログラムを選択して下さい",scateid,sep=":"),
  #                     choices= set_m,
  #                     selected = tail(set_m,1)
  #                     )
  # })
  
  #scatterplotページのmerchant_リストの更新を行う。小カテゴリが変化した時。
  observeEvent(input$var_sm,{
    #sm_id_input()は、var_smを読んで、category_low_idを返す関数
    #scateid<- sm_id_input()
    set_m <- merchant_scate_list()
    updateSelectInput(session,"select_program_up",
                         label="プログラムを選択して下さい",
                         choices= set_m,
                         selected = tail(set_m,1)
                         )
  })
  
  #merchant_リストの更新は、地域が変更された時にも行う。
  observeEvent(input$var_region_s,{
    #sm_id_input()は、var_smを読んで、category_low_idを返す関数
    #scateid<- sm_id_input()
    set_m <- merchant_scate_list()
    updateSelectInput(session,"select_program_up",
                      label="プログラムを選択して下さい",
                      choices= set_m,
                      selected = tail(set_m,1)
                      )
  })
  
 
  #小カテと地域によって決定する存在するマーチャント集合を作成する関数(scatterplotページ用)
  merchant_scate_list <- reactive({
    #入力された小カテIDラベル付
    scate_id <- sm_id_input()

    #merchant_labを入力されたscaet_idに限定する。
    merchant_list <- merchant_lab %>% dplyr::filter(category_low_id == scate_id)
    
    #実際にデータdf存在するマーチャントのリストにする。
    exist_mc_list <- df %>%dplyr::filter(category_low_id == scate_id) %>% dplyr::filter(region == input$var_region_s) %>% distinct(merchant_site_id)

    exist_mc_list <- left_join(x=exist_mc_list,y=merchant_list,by="merchant_site_id")
    program_set <- exist_mc_list$program_name
    return(program_set)
    
  })
  
  output$program_name_selected <-renderPrint({
    #program_name_selected()
    #cat(paste("htmlプログラム名",input$program_Name, sep=":"))
    mc_id <-unlist(strsplit(input$select_program_up,"_"))[1]
#    cat(paste("プログラム名",substring(input$select_program_up,1,6), sep=":"))
    cat(paste("プログラム名",input$select_program_up, sep=":"))
    cat("\n")
    fff<-sm_id_input()
    cat(paste("小カテ名",input$var_sm,sep=":"))
    cat("\n")
#    cat(paste("小カテID",fff,sep=":"))
#    cat("\n")
#    cat(paste("プログラム名",merchant_id_input(), sep=":"))
  })
  
  #scatterplotページで選択されたID_マーチャント名を受取、merchatn_site_idだけにして返す関数
  #これ関数にする必要無いかも。
  #関数にするなら、引数を受けてIDだけを返すようにしたら
  merchant_id_input <- reactive({
    mc_id <-unlist(strsplit(input$select_program_up,"_"))[1]
    return(mc_id)
  })
  

  # ScatterplotのUi入力データをみて絞り込みする関数
  define_data_scp <- reactive({
    #小カテゴリか極小カテゴリかによるデータフレーム選択
      input_cate <- sm_id_input()
      sl_df <- df %>% dplyr::filter(category_low_id ==input_cate)
    #UIのregion変数を読んで、地域を新潟、東京のいずれかに絞る。
      sl_df <- sl_df %>% dplyr::filter(region == input$var_region_s)
      
    #マーチャントを絞る。
      input_merchant<- merchant_id_input()
      sl_df <- sl_df %>% dplyr::filter(merchant_site_id == input_merchant)
      
#    #チェックボックスで「週番号12を除く」にチェックがあれば、レコードを除く
    if(input$checkbox_1 == TRUE){    
      #week_num=12が異常値なのでこれを全て削除
      sl_df <- sl_df %>% dplyr::filter(week_num != 12)
    }
    return(sl_df)
  })
  
  #scatterplotのページでマーチャントが変わるごとにデータを差し替える関数。
  testdef<- eventReactive(input$select_program_up,{
    merchant_df <-define_data_scp()
  })
  
  # corrmatrixのUi入力データをみて絞り込みする関数
  define_data_corr <- reactive({
    #小カテID数値を取得
    input_cate <- as.integer(unlist(strsplit(input$var_sm_cr,"_"))[1])
    
    sdf <- df %>% dplyr::filter(category_low_id ==input_cate)
    #UIのregion変数を読んで、地域を新潟、東京のいずれかに絞る。
    sdf <- sdf %>% dplyr::filter(region == input$var_region_cr)
    
    #マーチャントを絞る。
    input_merchant<- as.integer(unlist(strsplit(input$var_merchant_cr,"_"))[1])
    sdf <- sdf %>% dplyr::filter(merchant_site_id == input_merchant)
    
    #    #チェックボックスで「週番号12を除く」にチェックがあれば、レコードを除く
    if(input$checkbox_3 == TRUE){    
      #week_num=12が異常値なのでこれを全て削除
      sdf <- sdf %>% dplyr::filter(week_num != 12)
    }
    return(sdf)
  })
  
  #corrmatrixページ用のマーチャント名が変わる毎に変わるデータセット定義
  datadef<- eventReactive(input$var_merchant_cr,{
    merchant_df <-define_data_corr()
    return(merchant_df)
  })
  
  
  output$testdata<-renderTable({
    #ここでイベントリアクティブな関数testdefを取ってきて使う
    #データを定義する。
    #merchant_df <-define_data_sp()
    merchant_df<- testdef()
    #selected_dfが0レコードだった場合、メッセージをoutput$Scatterplotに表示する。
    validate(
      need(nrow(merchant_df) >0,"このカテゴリのデータ数が0レコードのため、散布図を表示できません。")
    )
    merchant_df
  })
  
#corrmatrix datatable--確認用なので、必要無い。
  output$view_corrdata<-renderTable({
    #データを定義する。
    merchant_df<-datadef()
    #selected_dfが0レコードだった場合、メッセージをoutput$Scatterplotに表示する。
    validate(
      need(nrow(merchant_df) >0,"このカテゴリのデータ数が0レコードのため、散布図を表示できません。")
    )
    merchant_df
  })
  
  
  output$scatterPlot <- renderPlot({
    catename <- input$var_sm
    mid <-unlist(strsplit(input$select_program_up,"_"))[1]
    
    #sdf <-define_data_sp()
    sdf<- testdef()
    #selected_dfが0レコードだった場合、メッセージをoutput$Scatterplotに表示する。
    validate(
      need(nrow(sdf) >0,"このカテゴリのデータ数が0レコードのため、散布図を表示できません。")
    )
    
    #font_A <- "IPAMincho"
    #最直近の週は赤でプロットする。
    latest_week <- max(sdf$week_num)
    sdf <- sdf %>% dplyr::mutate(latest="FALSE")
    sdf[sdf$week_num==latest_week,"latest"]<- "TRUE"
    
    
    p <-ggplot(sdf,aes(y=get(input$var_y), x=get(input$var_x),colour=latest))+geom_point()+ylab(input$var_y)+xlab(input$var_x)
    p <- p+ggtitle(paste("散布図",catename,"地域",input$var_region_s," ","プログラム",mid,sep=":"))
    p <- p + scale_color_manual(values= c("TRUE"="red","FALSE"="blue"))
    #p<-p+theme(text = element_text(family = font_A))
    #p <-p + theme_bw(base_family="HiraMaruProN-W3")
    p <- p + theme_bw(base_family="IPAPMincho")
    p <- p + theme(axis.text.x = element_text(size=12),
                   axis.text.y = element_text(size=12))
    p <- p + scale_y_continuous(labels = comma)
    p <- p + scale_x_continuous(labels = comma)
    #p<-p + theme_bw(base_family="IPAexMincho")
    plot(p)
    
  })
  
  #--low category: correlation matrix page
  #この関数も必要ないんじゃない？inputにあるvar_sm_crの文字列の数値部分を取ってくるだけだから。
  #sm_id_input_c <- reactive({
  #  return(as.integer(unlist(strsplit(input$var_sm_cr,"_"))[1]))
  #})

  #-- make merchant program list for scate_corrmatrix page
  merchant_scate_list_corr <- reactive({
    #corrmatrixページで入力された小カテIDラベル付
    #scate_id <- sm_id_input_c()
    scate_id <- as.integer(unlist(strsplit(input$var_sm_cr,"_"))[1])
    
    #merchant_labを入力されたscaet_idに限定する。
    merchant_list <- merchant_lab %>% dplyr::filter(category_low_id == scate_id)
    
    #実際にデータdf存在するマーチャントのリストにする。
    exist_mc_list <- df %>%dplyr::filter(category_low_id == scate_id) %>% dplyr::filter(region == input$var_region_cr) %>% distinct(merchant_site_id)
    
    exist_mc_list <- left_join(x=exist_mc_list,y=merchant_list,by="merchant_site_id")
    program_set <- exist_mc_list$program_name
    return(program_set)
    
  })
  
  #corrmatrixページの小カテを読む
  observeEvent(input$var_sm_cr,{
    set_m <- merchant_scate_list_corr()
    updateSelectInput(session,"var_merchant_cr",
                      label="プログラムを選択して下さい",
                      choices= set_m,
                      selected = tail(set_m,1)
    )
  })
  
  #merchant_リストの更新は、地域が変更された時にも行う。
  observeEvent(input$var_region_cr,{
    set_m <- merchant_scate_list_corr()
    updateSelectInput(session,"var_merchant_cr",
                      label="プログラムを選択して下さい",
                      choices= set_m,
                      selected = tail(set_m,1)
    )
  })
  


  #show correlation matrix between kpis
  output$corrmatrix<- renderPlot({
    #reactive function sm_id_input
    #input_cate<- sm_id_input_c()
    #selected_df <-df %>% dplyr::filter(category_low_id == input_cate)
    # 地域の選択変数：var_region_cr
    #selected_df <- selected_df %>% dplyr::filter(region == input$var_region_cr)
    
    #mid <- merchant_id_input_cr()
    #マーチャントのフィルタ
    #---
    selected_df<- datadef()
    # errorトラップ
    validate(
      need(nrow(selected_df) >0,"対象カテゴリのレコード数が0のため計算できません。")
    )
    
    if( input$checkbox_3 == TRUE){
    #week_num=12が異常値なのでこれを全て削除
      selected_df <- selected_df %>% dplyr::filter(week_num != 12)
    }
    
    corr<-round(cor(selected_df[5:10]),3)
    #p_value
    p.mat<- cor_pmat(selected_df[5:10])
    p<-ggcorrplot(corr,lab=TRUE,lab_size=4,
                  type="lower",p.mat=p.mat,insig="pch",title="Correlation matrix between KPI")
    plot(p) 
  })
  
  #return small category name that was selected in cormatrix tab
  output$selected_cate_name<-renderText({
    paste("小カテゴリ名",input$var_sm_cr," 地域",input$var_region_cr,"プログラム",input$var_mc_cr,sep=":")
  })

#---------------------------------------------------------------------------------------------------------------------
  #--low category: Simulation Page
  sm_id_input_sim <- reactive({
    tmp_id <- subset(scate_exist,choise_label==input$cate_sim)
    tmp_id$category_low_id
  })
  
  #-- min category: Simulation Page
  min_id_input_sim <- reactive({
    tmp_id <- subset(mcate_exist,choise_label==input$cate_sim_m)
    tmp_id$category_min_id
  })
  
  
  # SimulationページのUI入力データをみて、データを絞り込む関数。
  define_data <- reactive({
    #小カテゴリか極小カテゴリかによるデータフレーム選択
    if(input$radio_s == 1){
      input_cate <- sm_id_input_sim()
      selected_df <-df %>% dplyr::filter(category_low_id ==input_cate)
    }
    else {#極小カテゴリのときのデータフレーム
      input_cate <- min_id_input_sim()
      selected_df <-dfm %>% dplyr::filter(category_min_id ==input_cate)
    }
    
    #チェックボックスで「週番号12を除く」にチェックがあれば、レコードを除く
    if(input$checkbox_5 == TRUE){    
      #week_num=12が異常値なのでこれを全て削除
      selected_df <- selected_df %>% dplyr::filter(week_num != 12)
    }
    #UIのregion変数を読んで、地域を新潟、東京のいずれかに絞る。
    #小カテ、極小カテ共通
    selected_df <- selected_df %>% dplyr::filter(region == input$var_region_sim)
    return(selected_df)
    
  })
  
  
  #return Simulation Page output-- 
  # regression model:1)media effect to click
  # 2) click effect to media
  # reactive({})にしなくても、中身のsm_id_input_sim()などがreactiveだから、書き換わっている？
 # 引数がある関数は、reactive({})とは書けないか？
    #calc_reg <-reactive({
  calc_reg <- function(obj_var){
    
    #Simulationページのinput情報からデータをフィルタして定義する。
    selected_df <- define_data()
    
    #shiny用、データ数が0だった時のエラーメッセージ
    validate(need( nrow(selected_df)>0,"データが0行です。計算できません。" )
             )
    
    if(nrow(selected_df) ==0){
      stop()
    }
    
    
    #obj_var ==1 説明変数がmedia_cnt,目的変数がcl_cntのモデル
    #obj_var ==2 説明変数がcl_cnt,目的変数がcv_cntのモデル
    if(obj_var == 1){
      y_var <- "cl_cnt"
      x_var <- "media_cnt"
    }else if (obj_var ==2){
      y_var <- "cv_cnt"
      x_var <- "cl_cnt"
    }
    #単回帰モデル、対数変換モデルの実行
    
    #-----------------------------------------
    # original model
    #-----------------------------------------
    res1 <- lm(data=selected_df,get(y_var)~ get(x_var))
    parm_df.1 <- get_parameters(res1)
    
    #モデルの当てはめ結果(ベクトル)
    est_df.1<- fitted(res1)
    

    #モデル名を追加
    parm_df.1$model_type <- "normal"
    #最直近の期待値
    parm_df.1$last_y <- est_df.1[length(est_df.1)]

    
    
    
    #-----------------------------------------
    # both_log model
    # y= ax^b
    # ln(y)=ln(a)+b*ln(x)
    # log(0)=INFを避けるために、0.01を加算
    #-----------------------------------------
    res2 <- lm(data=selected_df,I(log(get(y_var)+0.01))~ I(log(get(x_var)+0.01)))
    parm_df.2 <- get_parameters(res2)
    
    #モデルの当てはめ結果(ベクトル)
    est_df.2<- fitted(res2)
    #モデル名を追加
    parm_df.2$model_type <- "double_log"
    #最直近の期待値
    parm_df.2$last_y <- est_df.2[length(est_df.2)]

    
    #-----------------------------------------
    # semi_log model(log convert only click(y))
    # 指数モデル
    #$$$$ y=a*e^(bx) by(toyota)
    # a^ = exp(a)
    # y = exp(a)*exp(bx)
    #-----------------------------------------
    res3 <- lm(data=selected_df,I(log(get(y_var)+0.01))~ get(x_var))
    parm_df.3 <- get_parameters(res3)
    
    #モデルの当てはめ結果(ベクトル)
    est_df.3<- fitted(res3)
    
    #モデル名を追加
    parm_df.3$model_type <- "y_log"
    #最直近の期待値
    parm_df.3$last_y <- est_df.3[length(est_df.3)]

    
    #-----------------------------------------
    # semi_log model(log convert media(x))
    # y = a + b*ln(x)
    #-----------------------------------------
    res4 <- lm(data=selected_df,get(y_var)~ I(log(get(x_var)+0.01)))
    parm_df.4 <- get_parameters(res4)
    
    #モデルの当てはめ結果(ベクトル)
    est_df.4<- fitted(res4)
    
    #モデル名を追加
    parm_df.4$model_type <- "x_log"
    #最直近の期待値
    parm_df.4$last_y <- est_df.4[length(est_df.4)]
    

        
    #-----------------------------------------
    # 通常回帰モデルの結果をレコードにマージする
    #-----------------------------------------
    parm_all <- rbind(parm_df.1,parm_df.2,parm_df.3,parm_df.4)
    
    #r2(adjusted)の最高のモデルを特定する
    parm_good_model <- parm_all %>% dplyr::arrange(desc(r2)) %>% head(1)
    

    #-----------------------------------------
    # Robust(MM推定) original model
    #-----------------------------------------
    #lmrob.controlオブジェクトを先に設定しておく。
    parm1<-lmrob.control()
    #parm1$seed=1.
    parm1$maxit.scale=500
    parm1$max.it=500
    parm1$k.max=500
    parm1$setting="KS2011"
    
    res5 <- lmrob(data=selected_df,get(y_var)~ get(x_var),control=parm1)
    #ロバスト回帰分析の結果パラメータを取得
    parm_df.5 <- get_rob_parameters(res5)
    
    #モデルの当てはめ結果(ベクトル)
    est_df.5<- fitted(res5)

    #モデル名を追加
    parm_df.5$model_type <- "robust_normal"
    #最直近の期待値
    parm_df.5$last_y <- est_df.5[length(est_df.5)]
  
    #-----------------------------------------
    # both_log model
    # y= ax^b
    # ln(y)=ln(a)+b*ln(x)
    # log(0)=INFを避けるために、0.01を加算
    #-----------------------------------------
    res6 <- lmrob(data=selected_df,I(log(get(y_var)+0.01))~ I(log(get(x_var)+0.01)),control=parm1)
    parm_df.6 <- get_rob_parameters(res6)
    
    #モデルの当てはめ結果(ベクトル)
    est_df.6<- fitted(res6)
    #モデル名を追加
    parm_df.6$model_type <- "robust_double_log"
    #最直近の期待値
    parm_df.6$last_y <- est_df.6[length(est_df.6)]
    
    #-----------------------------------------
    # robust semi_log model(log convert only click(y))
    # 指数モデル
    #$$$$ y=a*e^(bx) by(toyota)
    # a^ = exp(a)
    # y = exp(a)*exp(bx)
    #-----------------------------------------
    res7 <- lmrob(data=selected_df,I(log(get(y_var)+0.01))~ get(x_var),control=parm1)
    parm_df.7 <- get_rob_parameters(res7)
    
    #モデルの当てはめ結果(ベクトル)
    est_df.7<- fitted(res7)
    
    #モデル名を追加
    parm_df.7$model_type <- "robust_y_log"
    #最直近の期待値
    parm_df.7$last_y <- est_df.7[length(est_df.7)]
    
    #-----------------------------------------
    # robust semi_log model(log convert media(x))
    # y = a + b*ln(x)
    #-----------------------------------------
    res8 <- lmrob(data=selected_df,get(y_var)~ I(log(get(x_var)+0.01)),control=parm1)
    parm_df.8 <- get_rob_parameters(res8)

    #モデルの当てはめ結果(ベクトル)
    est_df.8<- fitted(res8)
    
    #モデル名を追加
    parm_df.8$model_type <- "robust_x_log"
    #最直近の期待値
    parm_df.8$last_y <- est_df.8[length(est_df.8)]
    
    #-----------------------------------------
    # モデルの結果をレコードに繋げる
    #-----------------------------------------
    parm_all_robust <- rbind(parm_df.5,parm_df.6,parm_df.7,parm_df.8)
    
    #r2(adjusted)の最高のモデルを特定する
    parm_all_robust<- parm_all_robust %>% dplyr::arrange(desc(r2)) %>% head(1)
    
        
    #通常回帰モデル群のの最良モデルと便宜上列名を合わせる
    colnames(parm_all_robust)<-colnames(parm_good_model)
    
    #通常回帰の最良モデルと、ロバスト回帰の最良モデルをマージ
    good_model<-rbind(parm_good_model,parm_all_robust)
    
    #r2のより高いモデルを選択する
    good_model <- good_model %>%  dplyr::arrange(desc(r2)) %>% head(1)
    
    #最直近のxの値(media_cnt,cl_cnt)の値を出力に持たせる
    tmp<-selected_df %>% dplyr::arrange(desc(week_num)) %>% head(1)%>% dplyr::select(x_var)
    #tmp<-tmp$media_cnt
    tmp <- tmp[,c(x_var)]
    good_model$last_x <- tmp
    
    #最直近のy(cl_cnt,cv_cnt)の観測値を出力に持たせる
    #parm_good_model$last_obs.y<- selected_df$cl_cnt[nrow(selected_df)]
    good_model$last_obs.y<-selected_df[nrow(selected_df),c(y_var)]
    
     
    return(good_model)

  }
 # })


  output$model_type_info<-renderText({
    m_name <- switch(input$radio_model_sim,
                         "1" = "media数でclick数を説明するモデル",
                         "2" = "click数でcv数を説明するモデル"
    )
    paste("モデル：",m_name,sep="")
  })
  

  output$check_value<- renderText({
    class(input$radio_model_sim)
  })
  
  #effect of media to click
  output$click_info <- renderPrint({
    
    #データを小カテ、極小カテのいずれかに決定し、回帰モデルを実行
    val_df <- calc_reg(1)
    
    
    #モデル種類
    model_type <- val_df$model
    
    model_type_str <- switch(model_type,
           "normal" = "変換なしモデル",
           "double_log" = "両対数変換モデル",
           "y_log" = "click対数変換モデル",
           "x_log" = "media対数変換モデル",
           "robust_normal" = "ロバスト変換なしモデル",
           "robust_double_log" = "ロバスト両対数変換モデル",
           "robust_y_log" = "ロバストclick対数変換モデル",
           "robust_x_log" = "ロバストmedia対数変換モデル")
    
    cat(paste("モデル種類:",model_type_str,sep=""),"\n")
    region_name<- switch(input$var_region_sim,
                         "Niigata"="新潟",
                         "Tokyo"="東京")
    cat(paste("地域:",region_name,sep=""),"\n")
    cat("\n")
    val <- val_df$estimate
    cat("メディア数のクリック数に対する効果:")
    cat(sprintf("%10.3f",val),"\n")
    cat("95% 信頼区間(下限):")
    cat(sprintf("%10.3f",val_df$low_ci),"\n")
    cat("95% 信頼区間(上限):")
    cat(sprintf("%10.3f",val_df$upper_ci),"\n")
    
    # significance of t value of coefficient
    sig_t<- ifelse(val_df$t_pval <= 0.05,"有意","有意でない")
    cat("偏回帰係数(media)の有意性:",sig_t,"\n")
    # significance of model
    sig_F <- ifelse(val_df$f_pval <= 0.05,"有意","有意でない")
    cat("モデルの有意性:",sig_F,"\n")
    cat("モデルの説明力_調整済決定係数:",sprintf("%6.2f",val_df$r2),"\n")
    #-- 調整済決定係数による有意性の判定結果
    hantei_res <- hantei(val_df$r2)
    cat("調整済決定係数の評価：",hantei_res,"\n")
    #val_df$r2の値が0.7以上なら、当てはまりがよい。
    # 0.5～0.7なら、やや当てはまりがよい。という判断を入れる。
    # 0.5未満なら、当てはまりが悪い。
    #-----ダブルログモデルの時の出力
    if(model_type =="double_log"){
      cat("/*------------------------------------------------------------------------*/\n")
      cat("/*-- 両対数変換モデルのパラメータの解釈 ----------------------------------*/\n")
      cat("推定されたモデル: cl_cnt=",sprintf("%6.3f",exp(val_df$intercept)),"*media_cnt^",sprintf("%6.3f",val_df$estimate),"\n" )
      cat("最直近の週のメディア数:")
      cat(sprintf("%7d",val_df$last_x),"\n")
      cat("最直近の週のメディア数の1%のメディア数:")
      val_1per_last_x <- val_df$last_x * 0.01
      cat(sprintf("%7.2f",val_1per_last_x),"\n")
      cat("最直近の週のclick数の期待値:")
      estimate_value <-  exp(val_df$last_y)
      cat(sprintf("%7.1f",estimate_value),"\n")
      cat("最直近の週のclick数の観測値:")
      observed_value <- val_df$last_obs.y
      cat(sprintf("%7.1f",observed_value),"\n")
      cat("メディア数が1％増加した場合のclickの増加率(%):")
      cat(sprintf("%7.3f",val_df$estimate),"\n")
      cat("最直近のメディア数においてメディアが1％増加した場合のclickの期待値:")
      estimate_2 <- exp(log(val_df$last_x*1.01)*val_df$estimate+val_df$intercept)
      cat(sprintf("%7.1f",estimate_2),"\n")
      cat("最直近のmedia数においてメディアが1％増加した場合のclickの変化量:")
      diff_cl <-  estimate_2 - estimate_value
      cat(sprintf("%7.1f",diff_cl),"\n")
      cat("/*-------------------------------------------------------------------------*/\n")

    }
    if(model_type =="y_log"){
      cat("/*------------------------------------------------------------------------*/\n")
      cat("/*-- clickのみ対数変換モデルのパラメータの解釈 --------------------------*/\n")
      cat("推定されたモデル: cl_cnt =",sprintf("%6.3f",exp(val_df$intercept)),"*exp(",val_df$estimate,"* media_cnt)\n" )
      cat("最直近の週のメディア数:",sprintf("%7d",val_df$last_x),"\n")
      observed_value <- val_df$last_obs.y
      cat("最直近の週のclick数の観測値:",sprintf("%7.1f",observed_value),"\n")
#     最直近の週のclickの期待値のexp
      estimate_value <-  exp(val_df$last_y)
      cat("最直近の週のclick数の期待値:",sprintf("%7.1f",estimate_value),"\n")
#      cat("メディア数が1つ増加した場合のclickの予測値の対数での増分\n")
#      cat(sprintf("%7.3f",val_df$estimate),"\n")
      cat("最直近のメディア数においてメディアが1つ増加した場合のclickの期待値:")
      estimate_2 <- exp((val_df$last_x+1)*val_df$estimate+val_df$intercept)
      cat(sprintf("%7.1f",estimate_2),"\n")
      cat("最直近のmedia数においてメディアが1つ増加した場合のclickの期待値の変化量:")
      diff_cl <-  estimate_2 - estimate_value
      cat(sprintf("%7.2f",diff_cl),"\n")
      cat("/*------------------------------------------------------------------------*/\n")
      
    }
    if(model_type =="x_log"){
      cat("/*------------------------------------------------------------------------*/\n")
      cat("/*-- mediaのみ対数変換モデルのパラメータの解釈 --------------------------*/\n")
      cat("推定されたモデル: cl_cnt =",sprintf("%10.3f",val_df$intercept),"+",sprintf("%10.3f",val_df$estimate),"* ln(media_cnt)\n" )
      cat("最直近の週のメディア数:",sprintf("%7d",val_df$last_x),"\n")
      observed_value <- val_df$last_obs.y
      cat("最直近の週のclick数の観測値:",sprintf("%7.0f",observed_value),"\n")
      #最直近の週のclickの期待値
      estimate_value <-  val_df$last_y
      cat("最直近の週のclick数の期待値:",sprintf("%7.1f",estimate_value),"\n")
      cat("最直近のメディア数においてメディアが1つ増加した場合のclickの期待値:")
      estimate_2 <- log(val_df$last_x+1)*val_df$estimate+val_df$intercept
      cat(sprintf("%10.1f",estimate_2),"\n")
      cat("最直近のmedia数においてメディアが1つ増加した場合のclickの期待値の変化量:")
      diff_cl <-  estimate_2 - estimate_value
      cat(sprintf("%10.2f",diff_cl),"\n")
      cat("/*------------------------------------------------------------------------*/\n")
      
    }
    
  })
  
  #これをglobal.Rに書くとinvalid multibyte character in parser となる
  #決定係数の値によってモデルの精度の見方のコメント文を返す関数
  hantei <- function(x){
    if(x < 0.5){hantei_str <-"モデルの当てはまりが悪い"}
    else if( (x >=0.5) && (x < 0.7)){hantei_str <- "モデルの当てはまりがやや良い"}
    else {hantei_str <- "モデルの当てはまりが良い"}
    return(hantei_str)
  }
  

  #click_infoの結果モデルを使い、予測値を返す
  output$result_simulation<- renderPrint({
    
    #知りたいxの値をinputから受けとる。
    x_val <- input$num
    #x_val <- 100

    #click数予測か、cv数予測かをradio_model_simから判定し、ベストなモデルのパラメータをdfで受けとる。
    parm_output <-switch( input$radio_model_sim,
                          "1" = calc_reg(1),
                          "2" = calc_reg(2)
    )
    
    #dataを確定する。
    df_sim <- define_data()
    
    #モデルを選ぶ。
    #obj_var ==1 説明変数がmedia_cnt,目的変数がcl_cntのモデル
    #obj_var ==2 説明変数がcl_cnt,目的変数がcv_cntのモデル
    if( input$radio_model_sim == "1"){
      y_var <- "cl_cnt"
      x_var <- "media_cnt"
    }else if (input$radio_model_sim == "2"){
      y_var <- "cv_cnt"
      x_var <- "cl_cnt"
    }
    
    #最良なモデル名を表示する。
#    cat(paste(y_var,x_var,"\n",sep=":"))
    model_name<-parm_output$model
    
    if(model_name=="normal"){
      estimate_value_set <- calc_normal(x_val,df_sim,y_var,x_var)
    } else if(model_name =="double_log"){
      estimate_value_set <- calc_double_log(x_val,df_sim,y_var,x_var)
    } else if(model_name =="y_log"){
      estimate_value_set <- calc_y_log(x_val,df_sim,y_var,x_var)
    } else if(model_name =="x_log"){
      estimate_value_set <- calc_x_log(x_val,df_sim,y_var,x_var)
    } else if(model_name =="robust_normal"){
      estimate_value_set <- calc_rob_normal(x_val,df_sim,y_var,x_var)
    } else if(model_name == "robust_double_log"){
      estimate_value_set <- calc_rob_double(x_val,df_sim,y_var,x_var)
    } else if (model_name =="robust_y_log"){
      estimate_value_set <- calc_rob_y(x_val,df_sim,y_var,x_var)
    } else if (model_name =="robust_x_log"){
      estimate_value_set <- calc_rob_x(x_val,df_sim,y_var,x_var)
    }
    
    msgstr <- paste("xが",x_val,"の時のモデルによる推測値",sep="")
    cat(paste(msgstr,sprintf("%7.3f",estimate_value_set[1]),sep=":"))
    cat("\n")
    cat("推測値の95%区間","\n")
    #cat( paste(sprintf("%s","推測値の95予測区間")))
    cat(paste(sprintf("%10s","下限"),sprintf("%7.3f",estimate_value_set[2]),sep=":"))
    #cat(paste("推測値の95%予測区間","(下限):",sep=""))
    cat("\n")
    cat(paste(sprintf("%10s","上限"),sprintf("%7.3f",estimate_value_set[3]),"",sep=":"))

  })
    
  calc_normal<-function(x_val,df,y_var,x_var){
    res1 <- lm(data=df,get(y_var)~ get(x_var))
    new_value <- data.frame(x_val)
    colnames(new_value) <- x_var
    outres<-predict(res1,new_value,interval="prediction",level = 0.95)
    return(outres)
  }
  
  calc_double_log<-function(x_val,df,y_var,x_var){
    res2 <- lm(data=df,I(log(get(y_var)+0.01))~ I(log(get(x_var)+0.01)))
    new_value <- data.frame(x_val)
    colnames(new_value) <- x_var
    outres <-predict(res2,new_value,interval="prediction",level = 0.95)
    outvalue <-exp(outres)
    return(outvalue)
  }
  
  calc_y_log <- function(x_val,df,y_var,x_var){
    res3 <- lm(data=df,I(log(get(y_var)+0.01))~ get(x_var))
    new_value <- data.frame(x_val)
    colnames(new_value) <- x_var
    outres <-predict(res3,new_value,interval="prediction",level = 0.95)
    outvalue <-exp(outres)
    return(outvalue)
  }
  
  calc_x_log <- function(x_val,df,y_var,x_var){
    res4 <- lm(data=df,get(y_var)~ I(log(get(x_var)+0.01)))
    new_value <- data.frame(x_val)
    colnames(new_value) <- x_var
    outres <-predict(res4,new_value,interval="prediction",level = 0.95)
    return(outres)
  }
  
  calc_rob_normal <- function(x_val,df,y_var,x_var){
    parm1<-lmrob.control()
    #parm1$seed=1.
    parm1$maxit.scale=500
    parm1$max.it=500
    parm1$k.max=500
    parm1$setting="KS2011"
    
    res5 <- lmrob(data=df,get(y_var)~ get(x_var),control=parm1)

    new_value <- data.frame(x_val)
    colnames(new_value) <- x_var
    outres <-predict(res5,new_value,interval="prediction",level = 0.95)
    return(outres)
  }
  
  calc_rob_double <- function(x_val,df,y_var,x_var){
    parm1<-lmrob.control()
    #parm1$seed=1.
    parm1$maxit.scale=500
    parm1$max.it=500
    parm1$k.max=500
    parm1$setting="KS2011"
    
    res6 <- lmrob(data=df,I(log(get(y_var)+0.01))~ I(log(get(x_var)+0.01)),control=parm1)
    new_value <- data.frame(x_val)
    colnames(new_value) <- x_var
    outres <-predict(res6,new_value,interval="prediction",level = 0.95)
    #推測値を対数から元の値に戻す
    outvalue <-exp(outres)
    return(outvalue)
  }
  
  calc_rob_y <- function(x_val,df,y_var,x_var){
    parm1<-lmrob.control()
    #parm1$seed=1.
    parm1$maxit.scale=500
    parm1$max.it=500
    parm1$k.max=500
    parm1$setting="KS2011"
    
    res7 <- lmrob(data=df,I(log(get(y_var)+0.01))~ get(x_var),control=parm1)
    new_value <- data.frame(x_val)
    colnames(new_value) <- x_var
    outres <-predict(res7,new_value,interval="prediction",level = 0.95)
    
    #推測値を対数から元の値に戻す
    outvalue <-exp(outres)
    return(outvalue)
  }
  
  calc_rob_x <-function(x_val,df,y_var,x_var){
    parm1<-lmrob.control()
    #parm1$seed=1.
    parm1$maxit.scale=500
    parm1$max.it=500
    parm1$k.max=500
    parm1$setting="KS2011"
    
    res8 <- lmrob(data=df,get(y_var)~ I(log(get(x_var)+0.01)),control=parm1)
    new_value <- data.frame(x_val)
    colnames(new_value) <- x_var
    outres <-predict(res8,new_value,interval="prediction",level = 0.95)
    return(outres)
  }
  
  #return Simulation Page output--click effect to cv
  #effect of click to cv
  output$model_cv <- renderPrint({
    
    #データを小カテ、極小カテのいずれかに決定し、回帰モデルを実行
    #calc_reg(引数=2で、cvをclickで説明するモデル)
    val_df <- calc_reg(2)
    #モデル種類
    model_type <- val_df$model
    
    model_type_str <- switch(model_type,
                             "normal" = "変換なしモデル",
                             "double_log" = "両対数変換モデル",
                             "y_log" = "cv対数変換モデル",
                             "x_log" = "click対数変換モデル",
                             "robust_normal" = "ロバスト変換なしモデル",
                             "robust_double_log" = "ロバスト両対数変換モデル",
                             "robust_y_log" = "ロバストcv対数変換モデル",
                             "robust_x_log" = "ロバストclick対数変換モデル")
    
    cat(paste("モデル種類:",model_type_str,sep=""),"\n")
    region_name<- switch(input$var_region_sim,
                         "Niigata"="新潟",
                         "Tokyo"="東京")
    cat(paste("地域:",region_name,sep=""),"\n")
    cat("\n")
    val <- val_df$estimate
    cat("クリック数のcv数に対する効果:")
    cat(sprintf("%10.3f",val),"\n")
    cat("95% 信頼区間(下限):")
    cat(sprintf("%10.3f",val_df$low_ci),"\n")
    cat("95% 信頼区間(上限):")
    cat(sprintf("%10.3f",val_df$upper_ci),"\n")
    
    # significance of t value of coefficient
    sig_t<- ifelse(val_df$t_pval <= 0.05,"有意","有意でない")
    cat("偏回帰係数(media)の有意性:",sig_t,"\n")
    # significance of model
    sig_F <- ifelse(val_df$f_pval <= 0.05,"有意","有意でない")
    cat("モデルの有意性:",sig_F,"\n")
    cat("モデルの説明力_調整済決定係数:",sprintf("%6.2f",val_df$r2),"\n")
    #-- 調整済決定係数による有意性の判定結果
    hantei_res <- hantei(val_df$r2)
    cat("調整済決定係数の評価：",hantei_res,"\n")
    #val_df$r2の値が0.7以上なら、当てはまりがよい。
    # 0.5～0.7なら、やや当てはまりがよい。という判断を入れる。
    # 0.5未満なら、当てはまりが悪い。
    #-----ダブルログモデルの時の出力
    if(model_type =="double_log"){
      cat("/*------------------------------------------------------------------------*/\n")
      cat("/*-- 両対数変換モデルのパラメータの解釈 ----------------------------------*/\n")
      cat("推定されたモデル: cl_cnt=",sprintf("%6.3f",exp(val_df$intercept)),"*media_cnt^",sprintf("%6.3f",val_df$estimate),"\n" )
      cat("最直近の週のclick数:")
      cat(sprintf("%7d",val_df$last_x),"\n")
      cat("最直近の週のclick数の1%のclick数:")
      val_1per_last_x <- val_df$last_x * 0.01
      cat(sprintf("%7.2f",val_1per_last_x),"\n")
      cat("最直近の週のcv数の期待値:")
      estimate_value <-  exp(val_df$last_y)
      cat(sprintf("%7.1f",estimate_value),"\n")
      cat("最直近の週のcv数の観測値:")
      observed_value <- val_df$last_obs.y
      cat(sprintf("%7.1f",observed_value),"\n")
      cat("click数が1％増加した場合のcv数の増加率(%):")
      cat(sprintf("%7.3f",val_df$estimate),"\n")
      cat("最直近のclick数においてclickが1％増加した場合のcv数の期待値:")
      estimate_2 <- exp(log(val_df$last_x*1.01)*val_df$estimate+val_df$intercept)
      cat(sprintf("%7.1f",estimate_2),"\n")
      cat("最直近のclick数においてclick数が1％増加した場合のcv数の変化量:")
      diff_cl <-  estimate_2 - estimate_value
      cat(sprintf("%7.1f",diff_cl),"\n")
      cat("/*-------------------------------------------------------------------------*/\n")
      
    }
    if(model_type =="y_log"){
      cat("/*------------------------------------------------------------------------*/\n")
      cat("/*-- cvのみ対数変換モデルのパラメータの解釈 --------------------------*/\n")
      cat("推定されたモデル: cv_cnt =",sprintf("%6.3f",exp(val_df$intercept)),"*exp(",val_df$estimate,"* cl_cnt)\n" )
      cat("最直近の週のclick数:",sprintf("%7d",val_df$last_x),"\n")
      observed_value <- val_df$last_obs.y
      cat("最直近の週のclick数の観測値:",sprintf("%7.1f",observed_value),"\n")
      #     最直近の週のclickの期待値のexp
      estimate_value <-  exp(val_df$last_y)
      cat("最直近の週のclick数の期待値:",sprintf("%7.1f",estimate_value),"\n")
      cat("最直近のclick数においてclickが1つ増加した場合のcv数の期待値:")
      estimate_2 <- exp((val_df$last_x+1)*val_df$estimate+val_df$intercept)
      cat(sprintf("%7.1f",estimate_2),"\n")
      cat("最直近のclick数においてメディアが1つ増加した場合のcv数の期待値の変化量:")
      diff_cl <-  estimate_2 - estimate_value
      cat(sprintf("%7.2f",diff_cl),"\n")
      cat("/*------------------------------------------------------------------------*/\n")
      
    }
    if(model_type =="x_log"){
      cat("/*------------------------------------------------------------------------*/\n")
      cat("/*-- clickのみ対数変換モデルのパラメータの解釈 --------------------------*/\n")
      cat("推定されたモデル: cv_cnt =",sprintf("%10.3f",val_df$intercept),"+",sprintf("%10.3f",val_df$estimate),"* ln(cl_cnt)\n" )
      cat("最直近の週のclick数:",sprintf("%7d",val_df$last_x),"\n")
      observed_value <- val_df$last_obs.y
      cat("最直近の週のcv数の観測値:",sprintf("%7.0f",observed_value),"\n")
      #最直近の週のcvの期待値
      estimate_value <-  val_df$last_y
      cat("最直近の週のcv数の期待値:",sprintf("%7.1f",estimate_value),"\n")
      cat("最直近のclick数においてclickが1つ増加した場合のcv数の期待値:")
      estimate_2 <- log(val_df$last_x+1)*val_df$estimate+val_df$intercept
      cat(sprintf("%10.1f",estimate_2),"\n")
      cat("最直近のclick数においてclickが1つ増加した場合のcvの期待値の変化量:")
      diff_cl <-  estimate_2 - estimate_value
      cat(sprintf("%10.2f",diff_cl),"\n")
      cat("/*------------------------------------------------------------------------*/\n")
      
    }
    
  })
  

  #define reactive code
  min_id_input <- reactive({
    tmp_id <- subset(mcate_exist,choise_label==input$min_cate)
    tmp_id$category_min_id
    #tmp_id is integer
  })
  
  output$selected_min_cate_id<-renderText({
     paste("極小カテゴリ",input$min_cate," 地域",input$var_region_cr_m,sep=":")
  })
  
  
  
  
  #contens of "min_corrmatrix"
  #calculate correlation matrix between KPI at mincate lebel
  output$min_corrmatrix<- renderPlot({
    #input_cate<- min_id_input()
    #極小カテのIDでフィルタ
    min_id <- as.integer(unlist(strsplit(input$min_cate,"_"))[1])
    selected_df <-dfm %>% dplyr::filter(category_min_id == min_id)
    #地域でフィルタ
    selected_df <- selected_df %>% dplyr::filter(region == input$var_region_cr_m)
    
    #week_numでフィルタする
    sum_df<-selected_df %>% dplyr::group_by(week_num) %>% 
      dplyr::summarise(cl_cnt=sum(cl_cnt),cv_cnt=sum(cv_cnt),cl_uu=sum(cl_uu),cv_uu=sum(cv_uu),media_cnt=sum(media_cnt)) 
    
    #--CVRを計算する。
    sum_df<- sum_df %>% dplyr::mutate(cvr=cv_cnt/cl_cnt)
    
    # errorトラップ
    validate(
      need(nrow(sum_df) >0,"対象カテゴリのレコード数が0のため計算できません。")
    )
    
    #if(nrow(selected_df) ==0){
    #  break
    #}
      
    if(input$checkbox_4 == TRUE){
      #week_num=12が異常値なのでこれを全て削除
      sum_df <- sum_df %>% dplyr::filter(week_num != 12)
    }
    corr<-round(cor(sum_df[2:7]),3)
    p.mat<- cor_pmat(sum_df[2:7])
    p<-ggcorrplot(corr,lab=TRUE,lab_size=4,
                  type="lower",p.mat=p.mat,insig="pch",title="KPI間相関行列")
    plot(p) 
  })
  

  #reactive code
  #task for min cate scatterplot
  
  #contens of "min_scatterPlot"
  #crate scatterplot mincate 
  output$min_scatterPlot<- renderPlot({
    #input_cate <-min_id_input_s()
    input_cate <- as.integer(unlist(strsplit(input$var_min,"_"))[1])
    selected_df <-dfm %>% dplyr::filter(category_min_id == input_cate)
    
    selected_df <- selected_df %>% dplyr::filter(region == input$var_region_m)
    
    #--12/7追加(極小カテはプログラム毎に成果を足すのはＮＧだからここは注意)
    #--week_numでKPIを集約する
    sum_df<-selected_df %>% dplyr::group_by(week_num) %>% 
      dplyr::summarise(cl_cnt=sum(cl_cnt),cv_cnt=sum(cv_cnt),cl_uu=sum(cl_uu),cv_uu=sum(cv_uu),media_cnt=sum(media_cnt)) 
    
    #--CVRを計算する。
    sum_df<- sum_df %>% dplyr::mutate(cvr=cv_cnt/cl_cnt)
    
    #レコード数が0だった場合のエラーメッセージ
    validate(
      need(nrow(sum_df) >0, "このカテゴリのデータ数が0レコードのため、散布図を表示できません。")
    )
    
    if(input$checkbox_2 == TRUE){
    #week_num=12が異常値なのでこれを全て削除
      sum_df <- sum_df %>% dplyr::filter(week_num != 12)
    }
    
    #最直近の週を区別するlatestカラムを追加
    sum_df <- sum_df %>% dplyr::mutate(latest="FALSE")
    latest_week <- max(sum_df$week_num)
    sum_df[sum_df$week_num == latest_week,"latest"]<- "TRUE"
    
    
    var_x <- input$var_min_x
    var_y <- input$var_min_y
    
    p <- ggplot(sum_df,aes(y=get(input$var_min_y), x=get(input$var_min_x),colour=latest))
    p <- p+geom_point()+ylab(input$var_min_y)+xlab(input$var_min_x)
    p <- p + scale_color_manual(values= c("TRUE"="red","FALSE"="blue"))
    p <- p+ggtitle(paste("散布図",input$var_min,"地域",input$var_region_m,sep=":"))
    p <- p+theme_bw()
    p <- p + theme(axis.text.x = element_text(size=12),
                   axis.text.y = element_text(size=12))
    p <- p + scale_y_continuous(labels = comma)
    p <- p + scale_x_continuous(labels = comma)
    plot(p)
  })
  
  
#  #--low category: plot timeseries chart
#  sm_id_input_ts <- reactive({
#    tmp_id <- subset(scate_exist,choise_label==input$cate_ts)
#    tmp_id$category_low_id
#    #tmp_id : integer
#  })
  


  
  #--------------------------------------------------------------------------------
  #timeseriesページのmerchant_リストの更新を行う。小カテゴリが変化した時。
  observeEvent(input$cate_ts,{
    set_m <- merchant_scate_list_ts()
    updateSelectInput(session,"merchant_ts",
                      label="プログラムを選択して下さい",
                      choices= set_m,
                      selected = tail(set_m,1)
    )
  })
  
  #merchant_リストの更新は、地域が変更された時にも行う。
  observeEvent(input$var_region_ts,{
    set_m <- merchant_scate_list_ts()
    updateSelectInput(session,"merchant_ts",
                      label="プログラムを選択して下さい",
                      choices= set_m,
                      selected = tail(set_m,1)
    )
  })
  
#--------------------------------------------------------------------------------

  #小カテと地域によって決定する存在するマーチャント集合を作成する関数(timeseriesページ用)
  #timeseriesページのUIを読み取り、マーチャントの選択肢集合を変える
  merchant_scate_list_ts <- reactive({
    
      #入力された小カテIDラベル付
      scate_id <- as.integer(unlist(strsplit(input$cate_ts,"_"))[1])
      
      #小カテの時にはこちら
      #merchant_labを入力されたscaet_idに限定する。
      merchant_list <- merchant_lab %>% dplyr::filter(category_low_id == scate_id)
      
      #実際にデータdf存在するマーチャントのリストにする。
      exist_mc_list <- df %>%dplyr::filter(category_low_id == scate_id) %>% dplyr::filter(region == input$var_region_ts) %>% distinct(merchant_site_id)
      
      exist_mc_list <- left_join(x=exist_mc_list,y=merchant_list,by="merchant_site_id")

    program_set <- exist_mc_list$program_name
    return(program_set)
    
  })

  #---------以上 scatterplotのmerchantlist更新と同じ-----------------------------------------------------------------------
  
  #create timeseries plot of selected variable of selected low category/ min category.
  output$sm_plot_1 <- renderPlot({
    
    #データ選択
#      input_cate <- as.integer(unlist(strsplit(input$cate_ts,"_"))[1])
#      selected_df <-df %>% dplyr::filter(category_low_id ==input_cate)
      mid <- as.integer(unlist(strsplit(input$merchant_ts,"_"))[1])
      selected_df<- df %>% dplyr::filter(merchant_site_id == mid)
    #regionのフィルタ
      selected_df <- selected_df %>% dplyr::filter(region == input$var_region_ts)

      #merchant別に縦持ちにする
      reshape_df <- melt(selected_df,id.vars=c("category_low_id","week_num","region","merchant_site_id"),
                         variable.name ="Varname",value.name="index")

      validate(
      need(nrow(selected_df) >0,"このカテゴリのデータ数は0です。")
      )
    

    #slidebarで指定した期間のプロットに限定する
    start_week <-input$slider_1[1]
    end_week <-input$slider_1[2]
    
    reshape_df <- reshape_df %>% dplyr::filter(week_num >= start_week, week_num <= end_week)
    
        
    p <- ggplot(reshape_df,aes(x=week_num,y=index,color= Varname))
    p <- p + geom_line(aes(group = Varname))
    p <- p + facet_wrap(~Varname,scales="free_y",ncol = 2)
    p <- p + theme_bw()
    #凡例を非表示にする
    p <- p + theme(legend.position ="nome")
    p <- p + theme(strip.text = element_text(size=15))
    p <- p + guides(fill=FALSE)
    p <- p +xlab("週番号")+ ylab("")
    p <- p + scale_y_continuous(labels = comma)
    plot(p)
    
  })
  
  #tableoutput
  output$view_table <- DT::renderDataTable({
    
      #input_cate <- sm_id_input_ts()
      #selected_df <-df %>% dplyr::filter(category_low_id ==input_cate)
      mid <- as.integer(unlist(strsplit(input$merchant_ts,"_"))[1])
      selected_df<- df %>% dplyr::filter(merchant_site_id == mid)
    
      #regionをフィルタする
      selected_df <- selected_df %>% dplyr::filter(region == input$var_region_ts)
      
      
      validate(
        need(nrow(selected_df) >0,"このカテゴリのデータ数は0です。")
      )
    
      #slidebarで指定した期間の表にする
      start_week <-input$slider_1[1]
      end_week <-input$slider_1[2]
     
      selected_df <- selected_df %>% dplyr::filter(week_num >= start_week, week_num <= end_week)
    
    # Datatableでオプションを指定するときには、server.Rで指定する
   
      DT::datatable(selected_df,
                  extensions = c('Buttons','Scroller'),
                  rownames=FALSE,
                  options = list(PageLength = 15,
                                 dom = 'Bfrtip',
                                 deferRender = TRUE,
                                 scrollY = 400,
                                 scroller = TRUE,
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                searching = FALSE
                                 
                                 )
                  ) %>%
    formatRound("cvr",digits =4) %>%
    formatCurrency(c("cl_cnt","cv_cnt","cl_uu","cv_uu"),currency="", interval=3,mark=",",digits=0)
  })


  get_compare_reg<- reactive({
    # 以下小カテの場合全てのカテゴリに対しての変換無しモデルの実行
    # 小カテか極小カテかでリストを作成
    if ( input$radio_cp == 1 ){
      all_categories <- unique(df$category_low_id)
      type_df <- df
      field_type <- "category_low_id"
    }
    else if(input$radio_cp == 2){
      all_categories <- unique(dfm$category_min_id)
      type_df <- dfm
      field_type <- "category_min_id"
    }
    
    #モデルの目的変数を選ぶ
    if(input$radio_model=="1"){
      #media数でclick数を説明するモデル
      y_var <- "cl_cnt"
      x_var <- "media_cnt"
    } else if(input$radio_model =="2"){
      y_var <- "cv_cnt"
      x_var <- "cl_cnt"
    }
    
    #ここで、小カテであれ、極小カテであれ、地域ごとのフィルターをかける。
    type_df <- type_df %>% dplyr::filter(region == input$var_region_comp)
    
    

    parm1<-lmrob.control()
   # parm1$seed=1546
    parm1$maxit.scale=400
    parm1$max.it=400
    parm1$k.max=500
    parm1$setting="KS2011"
    
    # maxit.scale:default=200,Err C level find_scale() iterationsの収束回数を増やす。
    # max.it M-step IRWLS(反復重み付け最小二乗法) iterationsの収束回数を増やす.
    # k.max:(for the fast-S algorithm): maximal number of refinement steps for the “fully” iterated best candidates.
    
    for (i in all_categories){

      selected_df<- type_df %>% dplyr::filter(get(field_type)==i)
      
      #この時点で、レコードが無ければ、ループを抜ける。
      if(nrow(selected_df)==0){next}
      
      #--モデルを複数実行する
      res_rg<-lm(data=selected_df,get(y_var)~get(x_var))
      res_rg2<-lm(data=selected_df,I(log(get(y_var)+0.01))~ I(log(get(x_var)+0.01)))
      res_rg3<-lm(data=selected_df,I(log(get(y_var)+0.01))~ get(x_var))
      res_rg4<-lm(data=selected_df,get(y_var)~I(log(get(x_var)+0.01)))
      
      parm_df.1 <-get_parameters(res_rg)
      parm_df.1$model_type<-"normal"
      
      parm_df.2 <-get_parameters(res_rg2)
      parm_df.2$model_type<-"double_log"
      parm_df.3 <-get_parameters(res_rg3)
      parm_df.3$model_type<-"y_log"
      
      parm_df.4 <-get_parameters(res_rg4)
      parm_df.4$model_type<-"x_log"
      
      #-----------------------------------------
      # モデルの結果(通常回帰)をレコードに繋げる
      #-----------------------------------------
      parm_all <- rbind(parm_df.1,parm_df.2,parm_df.3,parm_df.4)
      
      #-----------------------------------------
      # ロバスト回帰(MM推定)を実施
      # ロバスト回帰条件として、レコード数が5以上ある場合と条件を付ける。
      #-----------------------------------------

      if(nrow(selected_df)>=5){
        if( sd( selected_df[,y_var])  != 0 ){
         warning_mes1<-"simpleWarning in lmrob.S(x, y, control = control, mf = mf): S-estimated scale == 0:  Probably exact fit; check your data\n"
         warning_mes2<-"simpleWarning in lmrob.S(x, y, control = control, mf = mf): find_scale() did not converge in 'maxit.scale' (= 400) iterations\n"  
         
         error.flag5<-tryCatch( lmrob(data=selected_df,get(y_var)~ get(x_var), control=parm1),
                                  warning=function(w){
                                    if(paste(w)==warning_mes1){
                                      print(paste("lmrob.S Warning",i,"model6"));"NaN"
                                    }
                                    },
                                    error=function(e){print(paste("error:model5:",i,e));"NaN"},
                                finally={
                                  #print(paste("model5",i))
                                }
                                    )
            res_rg5 <- error.flag5
            #ロバスト回帰分析の結果パラメータを取得
            #res_rg5が、lmrobオブジェクトではなく"Nan"の場合、全てNAのデータを返す。
            parm_df.5 <- get_rob_parameters(res_rg5)
            parm_df.5$model_type <- "robust_normal"
         
          
          #-- ロバストy対数変換モデル
          error.flag6<-tryCatch(lmrob(data=selected_df,I(log(get(y_var)+0.01))~ I(log(get(x_var)+0.01)),control=parm1),
                                warning=function(w){
                                  if(paste(w)==warning_mes1){
                                    print(paste("lmrob.S Warning",i,"model6"));"NaN" 
                                  }
                                  else if( paste(w)== warning_mes2){
                                    print("Did not converge if use default parameter");
                                    #syori
                                    parm1$maxit.scale<-4000
                                    res<-lmrob(data=selected_df,I(log(get(y_var)+0.01))~ I(log(get(x_var)+0.01)),control=parm1)
                                    print(paste("change parameter",i))
                                    return(res)
                                  }
                                },
                                error=function(e){},
                                finally={
                                  #print(paste("model6",i))
                                },
                                silent=TRUE
                                )
          res_rg6<-error.flag6
                          
          #この段階で、res_rg6は、lmrobオブジェクトになっている。
          parm_df.6 <-get_rob_parameters(res_rg6)
          parm_df.6$model_type<-"robust_double_log"
   
          
          #lmrobの結果がerrorになる場合当該モデルを推定しない。
          error.flag7<-tryCatch(lmrob(data=selected_df,I(log(get(y_var)+0.01))~ get(x_var),control=parm1),
                    warning=function(w){
                      if(paste(w)==warning_mes1){
                        print( paste("lmrob.S Warning",i,"model7") );"NaN" 
                      } else{
                        print(paste(w));"NaN"
                      }
                    } ,
                    error=function(e){
                      print(paste(e,"error"));"NaN"
                    },
                    finally = {
                      #print(paste("model7",i))
                      
                    }
           )
          res_rg7<- error.flag7
          parm_df.7 <- get_rob_parameters(res_rg7)
          parm_df.7$model_type<-"robust_y_log"
          
      #    {
      #      if(class(error.flag7) =="try-error" ){
      #        #もし何もエラーだった時は、このモデルを推定しない。
      #        print("error:",i)}
      #      else if ( class(error.flag)=="lmrob" ){
      #        res_rg7<-error.flag
      #        parm_df.7 <- get_rob_parameters(res_rg7)
      #        parm_df.7$model_type<-"robust_y_log"
      #      }
      #    }
          
    
          error.flag8<-tryCatch(lmrob(data=selected_df,get(y_var)~I(log(get(x_var)+0.01)),control=parm1),
                                warning=function(w){
                                  if(paste(w)==warning_mes1){
                                    paste("lmrob.S Warning",i,"model8");"NaN" 
                                  }
                                } ,
                                error=function(e){print(paste(e)); "NaN" },
                                finally={
                                  #print(paste("model8",i))
                                }
                        )
     
          
          res_rg8<- error.flag8
          parm_df.8 <-get_rob_parameters(res_rg8)
          parm_df.8$model_type<-"robust_x_log"
    
          
          #-----------------------------------------
          # ロバスト回帰モデル(MM推定)の結果をレコードに繋げる
          #-----------------------------------------
          parm_robust_all <- rbind(parm_df.5,parm_df.6,parm_df.7,parm_df.8)
          
          #chisqとmodel_prの列名が通常回帰モデルと異なるので列名を揃える
          colnames(parm_robust_all)<-colnames(parm_all)
          
          parm_all<-rbind(parm_all,parm_robust_all)

        }
      }
      
      #r2(adjusted)の最高のモデルを特定する
      #文字列の"NA"が入力されたデータは省く
      parm_good_model <- parm_all %>% dplyr::filter(r2 !="NA" & r2 != "NaN")%>%
        dplyr::arrange(desc(r2)) %>% head(1)
      
      #もし、通常回帰モデルの結果が全てR2=NaNで(つまり計算されない）、ロバスト回帰もなされなかった場合のエラートラップを作っておく
      #その時はoutdfにはレコードを追加しないで抜ける。
      if(nrow(parm_good_model)==1){
      
        #カテゴリNoを付与
        parm_good_model$cateID <- i
        
        #if (exists("outdf")==FALSE){ # デバック用
        if(i == 1000||i == 1){
          outdf<-parm_good_model } else{
            outdf<- rbind(outdf,parm_good_model)
            }
      }

    #--for ループ終わり
    }
    
    
    #全てのカテゴリのモデル推定結果にカテゴリ名をjoinする
    if (input$radio_cp == 1 ){
      outdf<-left_join(x=scate_exist[,c("category_low_id","choise_label")],y=outdf,by=c("category_low_id"="cateID"))
    }
    else if (input$radio_cp == 2){
      outdf<-left_join(x=mcate_exist[,c("category_min_id","choise_label")],y=outdf,by=c("category_min_id"="cateID"))
    }
    #各カテゴリごとのモデルパラメータから決定係数だけ取得する
    outdf <- outdf %>% dplyr::select(choise_label,r2,model_type)
    return(outdf)
  })
  
  #simulation_output_table
  output$view_compare_sim<-DT::renderDataTable({

    #checkbox_viewで実行ボタン
    if(input$checkbox_view == TRUE){
      #get_compare_reg()で、モデルを全カテゴリで実行した結果を受けとる
      compare_model_output<-get_compare_reg()
      # 調整済決定係数(r2)のカテゴリⅡ絞って表示させる
      compare_model_output <- compare_model_output %>% dplyr::filter(r2>=0.4)
      DT::datatable(compare_model_output,
                    rownames=FALSE,
                    options = list(
                      order = list(1,'desc'),
                      searching = FALSE
                    )) %>%
      formatRound("r2",digits=3) 
    }
  })
  
  #Page::compare_sim
  #return current model name
  output$current_model_name<-renderText({
    model_name <- switch(input$radio_model,
           "1" = "media数でclick数を説明するモデル",
           "2" = "click数でcv数を説明するモデル"
    )
    paste("モデル：",model_name,sep="")
  })
  

})


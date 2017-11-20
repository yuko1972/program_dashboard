
library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("ATカテゴリ別の分析",
                 tabPanel("Scatterplot",
                          titlePanel("小カテゴリ毎週次データの散布図"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var_sm",
                                          label="小カテゴリを選んで下さい",
                                          choices = scate_exist$choise_label,
                                          selected = "1000"),
                              #任意の変数を2つ選ぶ
                              selectInput("var_x",
                                          label="x軸の変数を選択して下さい",
                                          #choices = names(df)[3:8],
                                          choices = c("cl_cnt","cv_cnt","media_cnt","cvr"),
                                          selected = "media_cnt"),
                              selectInput("var_y",
                                          label="y軸の変数を選択して下さい",
                                          #choices = names(df)[3:8],
                                          choices = c("cl_cnt","cv_cnt","media_cnt","cvr"),
                                          selected = "cv_cnt"),
                              hr(),
                              # 12週目(異常値)を除くかどうか選択
                              checkboxInput("checkbox_1", label = "週番号12を除く", value = TRUE)
                            ),
                            
                            mainPanel(
                              # "distPlot"という名前でreactiveなPlotタイプの出力を宣言する
                                plotOutput("scatterPlot")
                              
                            )
                            
                          ) #close sidebarLayout  
                 ), #close tabPanel,
                 

                 tabPanel("corrmatrix",
                          titlePanel("小カテゴリ毎週次データKPI間相関行列"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var_sm_cr",
                                          label="小カテゴリを選んで下さい",
                                          choices = scate_exist$choise_label,
                                          selected = "1000"),
                              # 12週目(異常値)を除くかどうか選択
                              checkboxInput("checkbox_3", label = "週番号12を除く", value = TRUE)
                            ), 
                            mainPanel(
                              textOutput("selected_cate_name"),
                              helpText("値に×がついているセルは、列の変数と行の変数のペアが有意でないことを意味します。"),
                              div("当該指標の標準偏差が0になる時、2つの変数間の相関係数は求められません。",style="color:blue"),
                              div("その場合、当該指標の列が非表示になります",style="color:blue" ),
                              plotOutput("corrmatrix")
                            )
                          )
                 ),
                 tabPanel("min_Scatterplot",
                          titlePanel("極小カテゴリ毎週次データの散布図"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var_min",
                                          label="極小カテゴリを選んで下さい",
                                          choices = mcate_exist$choise_label,
                                          selected = "1"),
                              #任意の変数を2つ選ぶ
                              selectInput("var_min_x",
                                          label="x軸の変数を選択して下さい",
                                          #choices = names(dfm)[3:8],
                                          choices = c("cl_cnt","cv_cnt","media_cnt","cvr"),
                                          selected = "media_cnt"),
                              selectInput("var_min_y",
                                          label="y軸の変数を選択して下さい",
                                          #choices = names(dfm)[3:8],
                                          choices = c("cl_cnt","cv_cnt","media_cnt","cvr"),
                                          selected = "cv_cnt"),
                              hr(),
                              # 12週目(異常値)を除くかどうか選択
                              checkboxInput("checkbox_2", label = "週番号12を除く", value = TRUE)
                              
                            ),
                            
                            mainPanel(
                              # "distPlot"という名前でreactiveなPlotタイプの出力を宣言する
                              plotOutput("min_scatterPlot")
                              
                            )
                            
                          ) #sidebarLayout  
                 ), #tabPanel,  
                 
                 
                 tabPanel("min_corrmatrix",
                          titlePanel("極小カテゴリ毎週次デーKPI間相関行列"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("min_cate",
                                          label="極小カテゴリを選んで下さい",
                                          # 実際にデータに存在する極小カテしか選べない
                                          choices = mcate_exist$choise_label,
                                          selected = "1"),
                              # 12週目(異常値)を除くかどうか選択
                              checkboxInput("checkbox_4", label = "週番号12を除く", value = TRUE)
                            ), 
                            mainPanel(
                              textOutput("selected_min_cate_id"),
                              helpText("値に×がついているセルは、列の変数と行の変数のペアが有意でないことを意味します。"),
                              div("当該指標の標準偏差が0になる時、2つの変数間の相関係数は求められません。"),
                              div("その場合、当該指標の列が非表示になります" ),
                              plotOutput("min_corrmatrix")
                            )
                          )
                 ),

                 tabPanel("timeseriesplot",
                          titlePanel("カテゴリ：傾向"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("radio",
                                          label = "小カテか、極小カテかを選んでください。",
                                          choices = c("小カテ"= "1", "極小カテ" ="2")),
                              
                              br(),
                              
                              conditionalPanel( condition = "input.radio == 1",
                                                selectInput("cate_ts",
                                                            label="小カテゴリを選んで下さい",
                                                            choices=scate_exist$choise_label,
                                                            selected ="1000")
                                                ),
                              conditionalPanel(condition = "input.radio == 2",
                                               selectInput("cate_ts_m",
                                                           label="極小カテゴリを選んで下さい",
                                                           choices=mcate_exist$choise_label,
                                                           selected ="1")
                                              ),
                              #input: slider for the period of week_number
                              sliderInput("slider_1",
                                          label = "プロットする期間(週番号)を選んで下さい",
                                          min = 1,
                                          max = max_weeknum,
                                          value = c(1,max_weeknum)
                              )
                            ),
                            mainPanel(
                              tabsetPanel( position = "below",
                              #output: 時系列プロット：選択した変数
                              tabPanel("Plot",
                                       #textOutput("selected_cate_name_plotvis"),  
                                       plotOutput("sm_plot_1")),
                              tabPanel("Table",
                                       #textOutput("selected_cate_name_plotvis"),  
                                       DT::dataTableOutput("view_table"))
                              )
                            )
                          )
                                             
                 ),
                 tabPanel("Simulation",
                          titlePanel("カテゴリ別予測値"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("radio_s",
                                           label = "小カテか、極小カテかを選んでください。",
                                           choices = c("小カテ"= "1", "極小カテ" ="2"),
                                           selected = 1),
                              
                              br(),
                              conditionalPanel( condition = "input.radio_s == 1",
                                                selectInput("cate_sim",
                                                            label="小カテゴリを選んで下さい",
                                                            choices=scate_exist$choise_label,
                                                            selected ="1000")
                              ),
                              conditionalPanel(condition = "input.radio_s == 2",
                                               selectInput("cate_sim_m",
                                                           label="極小カテゴリを選んで下さい",
                                                           choices=mcate_exist$choise_label,
                                                           selected ="1")
                              ),
                              # 12週目(異常値)を除くかどうか選択
                              checkboxInput("checkbox_5", label = "週番号12を除く", value = TRUE)
                            ),
                            mainPanel(
                              div("media数でclick数を説明するモデル",style="color:blue"),
                              verbatimTextOutput("click_info"),
                              div("click数でcv数を説明するモデル",style="color:blue"),
                              verbatimTextOutput("model_cv")
                            )
                            
                          )
                          ),
                 tabPanel("compare_sim",
                          titlePanel("カテゴリ間のモデル精度比較"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("radio_cp",
                                           label = "小カテか、極小カテかを選んでください。",
                                           choices = c("小カテ"= "1", "極小カテ" ="2")),
                              radioButtons("radio_model",
                                           label = "モデルを選んで下さい",
                                           choices = c("click数をmedia数で説明するモデル"= 1,"cv数をclick数で説明するモデル"=2)),
                              hr(),
                              div("[実行]をチェックしてから結果が表示されるまで10～20秒ほどお待ち下さい。",style="color:blue"),
                              checkboxInput("checkbox_view", label = "実行", value = FALSE)
                            ),
                            mainPanel(
                              div("当てはまりがある程度良いカテゴリの表示"),
                              div("モデルの当てはまりの良い(r2が1に近いほど良い)カテゴリ順"),
                              verbatimTextOutput("current_model_name"),
                              br(),
                              DT::dataTableOutput("view_compare_sim")
                            )
                          )
                          ),
                 tabPanel("about",
                          titlePanel("データ情報"),
                          sidebarLayout(
                            sidebarPanel(
                              helpText("集計元データ: islog.sales_log,islog.isweb_haisin_click_log, islog_haisin_atm_click_log"),
                              div("データ取得期間：2017/01/02～2017/11/12"),
                              br(),
                              div("データ粒度:週次,月曜始まり日曜終わり"),
                              div("2017/1/1は、1日しかない週となったため割愛"),
                              div("集計対象：service_type:AT/ATMのみ、非ポイントのみ、RKのみ"),
                              br()
                            ),
                            mainPanel(
                              helpText("更新履歴"),
                              p("更新履歴(2017/11/17)"),
                              div("compare_simページで、cv数をclick数で説明するモデルのERRに対処。",style="color:blue"),
                              div("カテゴリのデータの目的変数のsdが0の時はロバスト回帰は行わないとした。パラメータはNAとして出力した。",style="color:blue"),
                              br(),
                              p("更新履歴(2017/11/16)"),
                              div("Simulationページで、ロバスト回帰モデルを取り入れた。",style="color:green"),
                              div("compare_simページで、ロバスト回帰モデルを含む8種類のモデルから最良のモデルを選択するようにした。",style="color:green"),
                              div("バグフィックス：対象カテゴリのレコード数が5以下の場合、ロバストモデルは行わない。結果はNAになる。とした。"),
                              br(),
                              p("更新履歴(2017/11/13)"),
                              div("データの更新:2017/11/12までデータ取り込み",style="color:red"),
                              div("gitを導入した。",style="color:darkblue"),
                              br(),
                              p("更新履歴(2017/11/10)"),
                              div("compare_simページを追加",style="color:blue"),
                              div("各カテゴリ毎に最適モデルの決定係数を表示し、(当てはまり)精度の良いカテゴリが瞬時に分かるようにした。",style="color:blue"),
                              div("Simulationページ：click数を説明変数としてcv数を目的変数とする回帰モデルの結果を追加",style="color:blue"),
                              div("compare_simページ：click数をmedia数で説明するモデルか、cv数をclick数で説明するモデルかの選択肢を追加",style="color:blue"),
                              div("現在、cv数をclick数で説明するモデルで、精度の表が表示されない"),
                              br(),
                              p("更新履歴(2017/11/9)"),
                              div("corrmatrix,min_corrmatrix,Simulationページで、週番号(week_num)12のデータを削除するチェックボックスを追加、デフォルトは12wは削除",style="color:Magenta"),
                              br(),
                              p("更新履歴(2017/11/8)"),
                              div("Scatterplotとmin_Scatterplotタブページで、週番号(week_num)12のデータを削除するチェックボックスを追加、12w削除版と削除なし版を比較出来るようにした",style="color:darkred"),
                              div("週番号12で、media数の原因不明の異常な増加が複数カテゴリにおいて見られるため",style = "color:darkblue"),
                              br(),
                              p("更新履歴(2017/11/7)"),
                              div("社内Shiny-serverでのURLでのapplicationアクセスできる環境に移行",style="color:darkblue"),
                              div("simulationタブページ:clickのみ対数変換モデル(自然指数関数モデル)、mediaのみ対数変換モデルでの出力結果解釈補足を追加",style = "color:darkblue"),
                              br(),
                              p("更新履歴(2017/11/6)"),
                              div("simulationタブページ：ダブルログモデルの出力結果の解釈を追加",style="color:blue"),
                              div("timeseriesplotタブページ(Tableタブ):csvエクスポート可能にした",style="color:blue"),
                              div("同上：数値表記に桁区切り、少数点桁数制御を追加",style="color:blue"),
                              br(),
                              p("更新履歴(2017/11/2)"),
                              p("simulationタブページ：結果は、4つのモデルのうちの最良のモデルの結果を出力させた"),
                              br(),
                              p("更新履歴(2017/10/26 17:00)"),
                              div("Simulation タブページの追加: 小カテ/極小カテを選択し、選択したカテゴリの単純回帰分析結果予測値を表示できるようにした。"),
                              div("データの更新:2017/10/22までデータ取り込み"),
                              div("データの修正:mediaのカウントロジック修正"),
                              div("データの修正:pointメディアを除去できていなかった点を修正"),
                              br(),
                              helpText("更新履歴(2017/10/26)"),
                              div("Scatterplotとmin_Scatterplotのページ：変数選択肢からcl_uuとcv_uuを削除"),
                              div("clickとmedia対数化するしないの4種類のモデルを実行し、最良のモデルの結果を表示させる"),
                              p("バグ"),
                              div("Simulationページで小カテ:1002_商品先物取引(CX)のcv数説明モデルの結果がERR--調査中")
                            )
                          )
                 )


  )
)



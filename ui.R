
library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("ATカテゴリ別の分析",
                 tabPanel("Scatterplot",
                  fluidPage(
                    titlePanel("マーチャント毎の散布図"),
                    fluidRow(
                      column(4,wellPanel(
                        selectInput("var_region_s",
                                    label="東京、新潟を選択して下さい",
                                    choices = c("Niigata","Tokyo"),
                                    selected ="Niigata"),
                        selectInput("var_sm",
                                    label="小カテゴリを選んで下さい",
                                    choices = scate_exist$choise_label,
                                    selected = "1000"),
                        #var_smによって変化させるmerchant集合
                        #htmlOutput("select_program"),
                        #var_smが変化すると同時にupdateされるマーチャントリスト
                        selectInput("select_program_up","選択して下さい",
                                    choices = c("33477_ひまわり証券【ひまわりFX】")),
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
                        checkboxInput("checkbox_1", label = "週番号12を除く", value = FALSE)
                        #actionButton("goButton","更新"),
                        #p("上記条件でグラフを更新するには更新をクリックして下さい。")
                      )),
                      column(8,
                             tabsetPanel(position = "below",
                                        tabPanel("散布図",
                                                  verbatimTextOutput("program_name_selected"),
                                                  plotOutput("scatterPlot")
                                         ),
                                         tabPanel("テーブル",
                                                  tableOutput("testdata")
                                         )
                              )
                      )
                    ) #fluidRowを閉じる。
                  ) #fluidPageを閉じる
                  #        titlePanel("小カテゴリ毎週次データの散布図"),
                 ), #close tabPanel,
                 
                 tabPanel("corrmatrix",
                          titlePanel("マーチャント毎週次データKPI間相関行列"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var_region_cr",
                                          label="東京、新潟を選択してください",
                                          choices= c("Niigata","Tokyo"),
                                          selected="Niigata"
                                          ),
                              selectInput("var_sm_cr",
                                          label="小カテゴリを選んで下さい",
                                          choices = scate_exist$choise_label,
                                          selected = "1000"),
                              selectInput("var_merchant_cr","選択して下さい",
                                          choices = c("33477_ひまわり証券【ひまわりFX】")),
                              # 12週目(異常値)を除くかどうか選択
                              checkboxInput("checkbox_3", label = "週番号12を除く", value = FALSE)
                            ), 
                            mainPanel(
                              tabsetPanel(position = "below",
                                          tabPanel("相関行列",
                                                   textOutput("selected_cate_name"),
                                                   helpText("値に×がついているセルは、列の変数と行の変数のペアが有意でないことを意味します。"),
                                                   div("当該指標の標準偏差が0になる時、2つの変数間の相関係数は求められません。",style="color:blue"),
                                                   div("その場合、当該指標の列が非表示になります",style="color:blue" ),
                                                   plotOutput("corrmatrix")
                                          ),
                                          tabPanel("テーブル",
                                                   tableOutput("view_corrdata")
                                          )
                              )#tabsetPanelの終わり
                            )
                          )
                 ),
                 tabPanel("min_Scatterplot",
                          titlePanel("極小カテゴリ毎週次データの散布図"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var_region_m",
                                          label="東京、新潟を選択して下さい",
                                          choices = c("Niigata","Tokyo"),
                                          selected ="Niigata"),
                              selectInput("var_min",
                                          label="極小カテゴリを選んで下さい",
                                          choices = mcate_exist$choise_label,
                                          selected = "2_DMP用＞ファッション"),
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
                              checkboxInput("checkbox_2", label = "週番号12を除く", value = FALSE)
                              
                            ),
                            
                            mainPanel(
                                # "distPlot"という名前でreactiveなPlotタイプの出力を宣言する
                                plotOutput("min_scatterPlot")
                            )
                            
                          ) #sidebarLayout  
                 ), #tabPanel,  
                 
                 
                 tabPanel("min_corrmatrix",
                          titlePanel("極小カテゴリ毎週次データKPI間相関行列"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var_region_cr_m",
                                          label="東京、新潟を選択してください",
                                          choices= c("Niigata","Tokyo"),
                                          selected="Niigata"
                              ),
                              selectInput("min_cate",
                                          label="極小カテゴリを選んで下さい",
                                          # 実際にデータに存在する極小カテしか選べない
                                          choices = mcate_exist$choise_label,
                                          selected = "2_DMP用＞ファッション"),
                              # 12週目(異常値)を除くかどうか選択
                              checkboxInput("checkbox_4", label = "週番号12を除く", value = FALSE)
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
                              selectInput("var_region_ts",
                                          label="東京か新潟を選んで下さい",
                                          choices = c("Niigata","Tokyo"),
                                          selected = "Niigata"),
                              br(),
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
                              selectInput("var_region_sim",
                                          label="東京か新潟を選んで下さい",
                                          choices = c("Niigata","Tokyo"),
                                          selected = "Niigata"),
                              br(),
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
                              checkboxInput("checkbox_5", label = "週番号12を除く", value = FALSE),
                              radioButtons("radio_model_sim",
                                           label = "モデルを選んで下さい",
                                           choices = c("click数予測モデル"= "1","cv数予測モデル"="2"),
                                           selected = "1")
                            ),
                            mainPanel(
                              verbatimTextOutput("model_type_info"),
                              conditionalPanel(
                                condition = "input.radio_model_sim == '1'",
                                verbatimTextOutput("click_info")
                              ),
                              conditionalPanel(
                                condition = "input.radio_model_sim == '2'",
                                verbatimTextOutput("model_cv")
                              ),
                              numericInput("num", label = h3("入力してください。"), value = 100),
                              #hr(),
                              verbatimTextOutput("result_simulation")
                            )
                          )
                          ),
                 tabPanel("Compare_sim",
                          titlePanel("カテゴリ間のモデル精度比較"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var_region_comp",
                                          label="東京か新潟を選んで下さい",
                                          choices = c("Niigata","Tokyo"),
                                          selected = "Niigata"),
                              br(),
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
                 #tabPanel("test",
                #          sidebarLayout(
                #          sidebarPanel(),
                #          mainPanel(
                #            numericInput("lat", "Latitude"),
                #            numericInput("long", "Longitude"),
                #            uiOutput("cityControls")
                #          )
                #            )
                #          ),
                 tabPanel("about",
                          titlePanel("データ情報"),
                          sidebarLayout(
                            sidebarPanel(
                              helpText("集計元データ: islog.sales_log,islog.isweb_haisin_click_log, islog_haisin_atm_click_log"),
                              div("データ取得期間：2017/01/02～2017/11/19"),
                              br(),
                              div("データ粒度:週次,月曜始まり日曜終わり"),
                              div("2017/1/1は、1日しかない週となったため割愛"),
                              div("集計対象：service_type:AT/ATMのみ、非ポイントのみ、RKのみ"),
                              br()
                            ),
                            mainPanel(
                              helpText("更新履歴"),
                              p("更新履歴(2017/12/07)"),
                              div("corrmatrixページで、マーチャント別の選択を追加した。",style="color:blue"),
                              div("12週目を除くチェックボタンは動作しない状態。",style="color:blue"),
                              br(),
                              p("更新履歴(2017/12/06)"),
                              div("Scatterplotページで、マーチャント別の選択を追加した。",style="color:purpl"),
                              div("地域、小カテゴリの選択肢によって、選択可能なマーチャントがリストアップされる。",style="color:purpl"),
                              br(),
                              p("更新履歴(2017/11/30)"),
                              div("simulationページで、robustモデルのシミュレーションにも対応",style="color:green"),
                              div("Scatterplot/min_Scatterplotページで、最直近の週のポイントの色を赤で表示させるようにした",style="color:red"),
                              br(),
                              p("更新履歴(2017/11/29)"),
                              div("simulationページで、xを入力させるinputboxを追加。シミュレーションの結果の推測値と95%信頼区間を表示。
                                  robustモデル以外は予測値をUIに表示させることに成功。",style="color:darkred"),
                              br(),
                              p("更新履歴(2017/11/28)"),
                              div("scatterplotページでx軸、y軸の数値表示形式を指数表示でなくする。",style="color:blue"),
                              div("simulationページで、選択したモデルの結果のみ出力させるようにした。",style="color:blue"),
                              br(),
                              p("更新履歴(2017/11/27)"),
                              div("地域とカテゴリの組み合わせによってデータが0となる場合のエラートラップとメッセージを全ページに追加した。",style="color:teal"),
                              br(),
                              p("更新履歴(2017/11/24)"),
                              div("全てのページで、地域(新潟か東京）別の分析を可能にした。",style="color:navy"),
                              div("データの更新:2017/11/19までデータ取り込み",style="color:darkgreen"),
                              div("compare_simで、データのレコード数が0になる場合スキップ処理を追加",style="color:darkgreen"),
                              div("小カテのデータにregion変数(Niigata/Tokyo)を追加して再集計した。",style="color:darkgreen"),
                              div("極小カテのデータにregion変数を追加して再集計した。",style="color:darkgreen"),
                              br(),
                              p("更新履歴(2017/11/21)"),
                              div("compare_simページで、ワーニングの例外処理を追加。",style="color:cyan"),
                              div("・データが6行以上でもxの分散が小さいと計算できない",style="color:cyan"),
                              br(),
                              p("更新履歴(2017/11/20)"),
                              div("compare_simページで、極小カテ、cv数を説明するモデルのErrに対処。"),
                              div("lmrobのS推定で収束しない場合だけ反復回数を増やす。",style="color:red"),
                              br(),
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



# Kuznets Curve Project
## プロジェクトの全体像解説
- 01_admin/01_master/admin.Rを実行することによって,以下のファイルが実行される
  1.  GDPの読み込み: 03_build/gdp_tidy/code/build.R 
      - outputファイルは 03_build/gdp_tidy/outputにある
  2. Gini係数の読み込み: 03_build/inequality_tidy/code/build.R
      -outputファイルは 03_build/inequality_tidy/outputにある
  3. GDPのデータの整形 03_build/gdp_ready/code/build.R
      - outputファイルは 03_build/gdp_ready/outputにある
  4. Gini係数のデータの整形: 03_build/inequality_ready/code/build.R
     - outputファイルは 03_build/inequality_ready/outputにある
  5. master dataの作成: 03_build/master/code/build.R
     -  outputファイルは 03_build/master/outputにある
  6. 分析の実行 04_analyze/initial/code/analyze.R
   - 図の作成
    -　散布図の作成
    - Gini係数の時系列変化の図
    - 一人当たりGDPの対数値の時系列変化の図
    - 尺度の異なる「ジニ係数と一人当たりGDP」の時系列変化を一つの図で見られる図を作成した<br>
    (Gini係数と一人当たりGDPの**二軸グラフ**を作成)<br>
    これによって一つの図でKuznets-Curveの特徴を視覚的に確かめられる
    - outputファイルは 04_analyze/initial/Figureにある
   - 回帰分析
    - 系列相関,固定効果,線形時間トレンドを考慮した一階差分法,線形時間トレンドを考慮した線形回帰モデルを行う
     - outputファイルは 04_analyze/initial/tableにある
  

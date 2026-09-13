# BioStat Studio 驗證紀錄

驗證日期：2026-09-10。環境：Windows 11 x64、R 4.5.2。完整套件版本另列 `package-versions.csv`。

## 驗證方式

- 可重跑數值與失敗處理測試：`tests/test_engine.R`；使用合成資料，沒有使用研究個資。
- Welch p 值與 R `t.test` 對照；線性係數與 `lm` 對照；α 與 `psych::alpha` 對照；已知二乘二表驗證 RR=2、RD=0.1。
- 檢查病例對照拒絕 RR、零事件不產生不適當 log CI、常數題拒絕、反向題缺上下限拒絕、非正定矩陣拒絕、AG 重疊拒絕。
- 問卷 α 實際跑 5,000 次 Bootstrap；EFA 測試以 20 次全 ML 多分相關置換驗證路徑及重現性。介面預設目前為 500 次，但未對任意資料規模完成全量效能驗證。
- SEM 的 MLR／WLSMV、AVE／HTMT2、個案重配適 Bootstrap 與非退化 HTMT2 區間另行檢查；SEM 自動測試採 20 次、補充 WLSMV 檢查採 30 次，未逐模型跑滿 5,000 次。
- 特別檢查估計函式改變亂數種子時，Bootstrap 樣本仍不同且可重現：先產生每次種子，並在傳入函式前實際建立重抽樣資料，避免 R 延遲求值造成重複樣本。
- 新增交互作用 delta CI／Wald p 值、比例勝算聯合 Wald、Bootstrap 交互作用及匯出回歸檢查。
- Shiny `testServer` 驗證合成資料匯入、型態確認、Table 1、Logistic 交互作用、問卷信度流程。本機 HTTP 回應為 200。
- 真正 DOCX／XLSX 以 OOXML 結構檢查確認，非改副檔名的 HTML。已檢查部分科學圖形的影像輸出。

## 實際限制

沒有使用你的正式資料，因此不能保證特定模型的可識別性、樣本量、收斂或研究設計正確。測試通過不代表軟體完成醫療器材認證、研究方法審查或所有統計情境驗證。

這個環境未提供 LibreOffice，Word 報告的自動 PDF／逐頁影像轉換未能完成；因此已驗證檔案結構及共用數字格式，但**未宣稱所有 Word 分頁與欄寬均經視覺驗證**。正式交稿前需開啟實際匯出報告檢查。

未進行全套瀏覽器點選／響應式畫面測試；已做伺服端工作流程測試及 HTTP 啟動驗證。

## 原始檔保留

來源 `biostat_studio.html`（37,996 bytes）未修改。

SHA-256：`1DE80B30B8A32173716FCE1769AE4E58830C79DE776330A2B56DE535D151C52D`

最終核心自動測試 **31 項通過、0 項失敗**。結果另存 `tests/last-test-summary.txt`；另有上述介面流程與交互作用补充驗證。


## 本次功能擴充

原有 30 個 Shiny server 分析流程案例已重新通過，另有 19 項新增測試：Shapiro 範圍與抽樣、一般中介、因果中介 2 種中介 × 5 種結果模型、Bootstrap、識別条件與輸入拒絕、模型診斷、本機選欄及結果區匯出。Bootstrap 20 次為程式流程測試，不是正式精度建議。

大型合成 CSV 測試成功讀入指定 3 欄的全部資料列，所選資料物件約 12.2 MiB。這是大量未選文字欄的合成案例，不代表使用者的原始大檔已測試，也不代表完整模型記憶體足夠。

信度、EFA、CFA、SEM 已拆分並另驗證 CFA 拒絕結構迴歸路徑；平行分析預設改為 500 次。介面名稱使用「干擾因子」。


## 2026-09-11 資料結構擴充

`tests/test_correlated.R` 共 26 項通過。包括 GEE Gaussian／Logistic／Modified Poisson／Poisson、三種工作相關、亂序及缺訪間距、LMM Satterthwaite、隨機斜率／交互作用、GLMM、奇異擬合拒絕 Wald 推論、配對 t／Wilcoxon／精確 McNemar、條件式 Logistic、參考組、ID／完整配對檢查、Shiny 模組派送。比對直接 geepack、lmerTest／lme4、stats、survival 結果。資料為合成或公開 R 範例。未以使用者原始大型資料驗證 GEE／混合模型資源需求。


## 2026-09-11 複雜抽樣與方法紀錄

`tests/test_survey.R` **24 / 24 通過**：加權平均／SD／百分比、設計 SE 與 t／logit CI、權重尺度不變性、PSU 在分層內重複、ids=~1、FPC 兩種輸入及完全抽樣、設計物件子母體、lonely PSU 策略與選項還原、設計輸入拒絕、Rao–Scott／Wald F／Bonferroni 家族、四種 GLM、事件／參考組／交互作用、完整個案、共線性拒絕、Cox，以及 Shiny 三個入口。與直接 survey 函數比對數值。另核對每個分析模式的方法紀錄、真實 R／套件版本、輸入改變後紀錄凍結及單一結果 Word／Excel 內含版本資訊。

新增共線性檢查後整套通過；不接受套件悄悄略掉共線性變項後仍對全部所選變項宣稱估計完成。最新原有 26 項資料結構、19 項中介／診斷等測試、Bonferroni／SMD 與分析分流檢查亦通過。原始 31 項核心測試與 30 個流程為較早批次，沒有重稱本批全部重跑。全部數值驗證使用合成資料或套件範例。


## 信度／EFA 出版表

`tests/test_measurement_tables.R` 16 項通過：原始／標準化 α／CITC／刪題 α 與 psych、兩題 SB、整組作答 Bootstrap 及 percentile CI、5,000 次可重現性／各組種子、題組缺失／反向題、可定義負值、低於 95% 停止、KR-20 對照、名目／ID／題組拒絕、Word／Excel 垂直合併與版本文字、EFA 型態分流、KMO／MSA／RMS、多因素完整負荷量、原有模型匯出相容性及 Shiny 流程。

已另修正 `bootstrap` 控制項 ID 造成 window.bootstrap 名稱衝突，改為 bootstrap_ci；瀏覽器確認首頁「開始匯入」可自動切至資料頁。修正後最新 24 項 survey、19 項中介等與最終分流檢查仍通過。此批未重新宣稱早期全部瀏覽器案例皆已重跑。

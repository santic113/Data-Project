WITH STEP1 AS( --Getting Company Fact Information | Base Table
SELECT
   A.symbol              AS SYMBOL
 , A.sector	             AS SECTOR
 , A.industry	           AS INDUSTRY
 , A.exchange_short_name AS EXCHNAGE
 , A.mkt_cap             AS MKT_CAP
 , A.full_time_employees AS EMPLOYEE_CNT
 , A.company_name	       AS COMPANY_NM
 , A.ceo	               AS CEO
 , A.description	       AS DESCRIPTION
 , A.image               AS IMAGE
FROM `stable-arch-295502.STOCK_DATA.Company_Facts` AS A

), STEP2 AS( --Getting Company Stock Price Information for Year 2020

SELECT
   A.Symbol           AS SYMBOL_QUANT
 , ROUND(A.Close, 2)	AS CLOSE_STOCK_PRICE
 , A.date   AS DATE
FROM `stable-arch-295502.STOCK_DATA.Advanced_Quant_Metrics` AS A

WHERE 1=1
  AND A.DATE > '2020-01-01'

GROUP BY
   A.Symbol
 , A.Close
 , A.date

), STEP3 AS(--Getting Stock Price at the Start of the Year

SELECT
   A.Symbol           AS SYMBOL_QUANT
 , ROUND(A.Close, 2)	AS CLOSE_STOCK_PRICE_START_YR
 , A.date   AS DATE
FROM `stable-arch-295502.STOCK_DATA.Advanced_Quant_Metrics` AS A

WHERE 1=1
  AND A.DATE = '2020-01-02'

),  STEP4 AS( --Getting the Max Date in my Dataset
SELECT
  A.symbol      AS SYMBOL
, MAX(A.date)   AS MAX_DATE
FROM `stable-arch-295502.STOCK_DATA.Advanced_Quant_Metrics` AS A

WHERE 1=1

GROUP BY
  A.symbol

), STEP5 AS( --Getting Most Recent Stock Price of the Most Recent Date
SELECT
   A.symbol      AS SYMBOL
 , A.Close       AS CLOSE_RECENT_DT
 , A.date        AS DATE
FROM `stable-arch-295502.STOCK_DATA.Advanced_Quant_Metrics` AS A

INNER JOIN STEP4 AS B
ON 1=1
  AND A.SYMBOL = B.SYMBOL
  AND A.DATE = B.MAX_DATE

WHERE 1=1
  AND A.Close IS NOT NULL

), STEP6 AS( --Joining & Consolidating My Data
SELECT
   A.ticker	      AS SP_TICKER
 , A.ref_date     AS DATE
 , A.price_close	AS SP_CLOSE_PRICE

FROM `stable-arch-295502.STOCK_DATA.S_P_500` AS A

GROUP BY
   A.ticker
 , A.ref_date
 , A.price_close
 ), STEP7 AS(
SELECT
   A.SYMBOL
 , A.SECTOR
 , A.INDUSTRY
 , A.EXCHNAGE
 , A.MKT_CAP
 , A.EMPLOYEE_CNT
 , A.COMPANY_NM
 , A.CEO
 , A.DESCRIPTION
 , A.IMAGE
 , B.DATE
 , C.CLOSE_STOCK_PRICE_START_YR
 , D.CLOSE_RECENT_DT
 , B.CLOSE_STOCK_PRICE
FROM STEP1 AS A

INNER JOIN STEP2 AS B
  ON 1=1
    AND A.SYMBOL = B.SYMBOL_QUANT

INNER JOIN STEP3 AS C
ON 1=1
  AND A.SYMBOL = C.SYMBOL_QUANT

INNER JOIN STEP5 AS D
ON 1=1
  AND A.SYMBOL = D.SYMBOL


GROUP BY
   A.SYMBOL
 , A.SECTOR
 , A.INDUSTRY
 , A.EXCHNAGE
 , A.MKT_CAP
 , A.EMPLOYEE_CNT
 , A.COMPANY_NM
 , A.CEO
 , A.DESCRIPTION
 , A.IMAGE
 , B.DATE
 , C.CLOSE_STOCK_PRICE_START_YR
 , D.CLOSE_RECENT_DT
 , B.CLOSE_STOCK_PRICE

 )
 SELECT
   A.SYMBOL
 , B.SP_TICKER
 , A.SECTOR
 , A.INDUSTRY
 , A.EXCHNAGE
 , A.MKT_CAP
 , A.EMPLOYEE_CNT
 , A.COMPANY_NM
 , A.CEO
 , A.DESCRIPTION
 , A.IMAGE
 , A.DATE
 , A.CLOSE_STOCK_PRICE_START_YR
 , A.CLOSE_RECENT_DT
 , A.CLOSE_STOCK_PRICE
 , B.SP_CLOSE_PRICE
 FROM STEP7 AS A

INNER JOIN STEP6 AS B
ON 1=1
  AND A.DATE = B.DATE

GROUP BY
   A.SYMBOL
 , B.SP_TICKER
 , A.SECTOR
 , A.INDUSTRY
 , A.EXCHNAGE
 , A.MKT_CAP
 , A.EMPLOYEE_CNT
 , A.COMPANY_NM
 , A.CEO
 , A.DESCRIPTION
 , A.IMAGE
 , A.DATE
 , A.CLOSE_STOCK_PRICE_START_YR
 , A.CLOSE_RECENT_DT
 , A.CLOSE_STOCK_PRICE
 , B.SP_CLOSE_PRICE

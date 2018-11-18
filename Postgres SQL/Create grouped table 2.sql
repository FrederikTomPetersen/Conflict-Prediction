 CREATE TABLE public.gdelt_y_group AS
	SELECT 
		 "ActionGeo_CountryCode" as Country
        ,"year"
        ,"month"

		,AVG("AvgTone") as AvgTone
		,AVG(cast(NULLIF("q1at", 0) AS BIGINT)) as q1at
        ,SUM(CASE WHEN "QuadClass" = 1 THEN 1 ELSE 0 END) as q1cnt
		,AVG(cast(NULLIF("q2at", 0) AS BIGINT)) as q2at
        ,SUM(CASE WHEN "QuadClass" = 2 THEN 1 ELSE 0 END) as q2cnt
		,AVG(cast(NULLIF("q3at", 0) AS BIGINT)) as q3at
        ,SUM(CASE WHEN "QuadClass" = 3 THEN 1 ELSE 0 END) as q3cnt
		,AVG(cast(NULLIF("q4at", 0) AS BIGINT)) as q4at
        ,SUM(CASE WHEN "QuadClass" = 4 THEN 1 ELSE 0 END) as q4cnt
		--Politiske begivenheder med religiøs betoning
		,AVG(cast(NULLIF("relq1at", 0) AS BIGINT)) as relq1at
		,SUM(CASE WHEN "QuadClass" = 2 AND "rel"= 1 THEN 1 ELSE 0 END) as relq1cnt
		,AVG(cast(NULLIF("relq2at", 0) AS BIGINT)) as relq2at
		,SUM(CASE WHEN "QuadClass" = 2 AND "rel"= 1 THEN 1 ELSE 0 END) as relq2cnt
		,AVG(cast(NULLIF("relq3at", 0) AS BIGINT)) as relq3at
		,SUM(CASE WHEN "QuadClass" = 3 AND "rel"= 1 THEN 1 ELSE 0 END) as relq3cnt
		,AVG(cast(NULLIF("relq4at", 0) AS BIGINT)) as relq4at
		,SUM(CASE WHEN "QuadClass" = 4 AND "rel"= 1 THEN 1 ELSE 0 END) as relq4cnt
		--Politiske begivenheder med etnisk betoning
		,AVG(cast(NULLIF("ethq1at", 0) AS BIGINT)) as ethq1at
		,SUM(CASE WHEN "QuadClass" = 2 AND "eth"= 1 THEN 1 ELSE 0 END) as ethq1cnt
		,AVG(cast(NULLIF("ethq2at", 0) AS BIGINT)) as ethq2at
		,SUM(CASE WHEN "QuadClass" = 2 AND "eth"= 1 THEN 1 ELSE 0 END) as ethq2cnt
		,AVG(cast(NULLIF("ethq3at", 0) AS BIGINT)) as ethq3at
		,SUM(CASE WHEN "QuadClass" = 3 AND "eth"= 1 THEN 1 ELSE 0 END) as ethq3cnt
		,AVG(cast(NULLIF("ethq4at", 0) AS BIGINT)) as ethq4at
		,SUM(CASE WHEN "QuadClass" = 4 AND "eth"= 1 THEN 1 ELSE 0 END) as ethq4cnt
		
FROM public.gdelt_y 
GROUP BY "ActionGeo_CountryCode"
		  ,"year"
		  ,"month"
ORDER BY "ActionGeo_CountryCode"
			,"year"
            ,"month"
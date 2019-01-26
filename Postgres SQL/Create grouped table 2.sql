 CREATE TABLE public.gdelt_y_group AS
	SELECT 
		 "ActionGeo_CountryCode" as country
        ,"year"
        ,"month"

		,AVG("AvgTone") as AvgTone
		,AVG(cast(NULLIF("q1at", 0) AS INT)) as q1at
		,AVG(cast(NULLIF("q1gs",0) AS INT)) as q1gs
        ,SUM(CASE WHEN "QuadClass" = 1 THEN 1 ELSE 0 END) as q1cnt
		,AVG(cast(NULLIF("q2at", 0) AS INT)) as q2at
		,AVG(cast(NULLIF("q2gs",0) AS INT)) as q2gs
        ,SUM(CASE WHEN "QuadClass" = 2 THEN 1 ELSE 0 END) as q2cnt
		,AVG(cast(NULLIF("q3at", 0) AS INT)) as q3at
		,AVG(cast(NULLIF("q3gs",0) AS INT)) as q3gs
        ,SUM(CASE WHEN "QuadClass" = 3 THEN 1 ELSE 0 END) as q3cnt
		,AVG(cast(NULLIF("q4at", 0) AS INT)) as q4at
		,AVG(cast(NULLIF("q4gs",0) AS INT)) as q4gs
        ,SUM(CASE WHEN "QuadClass" = 4 THEN 1 ELSE 0 END) as q4cnt
		--Politiske begivenheder med religiøs betoning
		,AVG(cast(NULLIF("relq1at", 0) AS INT)) as relq1at
		,AVG(cast(NULLIF("relq1gs",0) AS INT)) as relq1gs
		,SUM(CASE WHEN "QuadClass" = 1 AND "rel"= 1 THEN 1 ELSE 0 END) as relq1cnt
		,AVG(cast(NULLIF("relq2at", 0) AS INT)) as relq2at
		,AVG(cast(NULLIF("relq2gs",0) AS INT)) as relq2gs
		,SUM(CASE WHEN "QuadClass" = 2 AND "rel"= 1 THEN 1 ELSE 0 END) as relq2cnt
		,AVG(cast(NULLIF("relq3at", 0) AS INT)) as relq3at
		,AVG(cast(NULLIF("relq3gs",0) AS INT)) as relq3gs
		,SUM(CASE WHEN "QuadClass" = 3 AND "rel"= 1 THEN 1 ELSE 0 END) as relq3cnt
		,AVG(cast(NULLIF("relq4at", 0) AS INT)) as relq4at
		,AVG(cast(NULLIF("relq4gs",0) AS INT)) as relq4gs
		,SUM(CASE WHEN "QuadClass" = 4 AND "rel"= 1 THEN 1 ELSE 0 END) as relq4cnt
		--Politiske begivenheder med etnisk betoning
		,AVG(cast(NULLIF("ethq1at", 0) AS INT)) as ethq1at
		,AVG(cast(NULLIF("ethq1gs",0) AS INT)) as ethq1gs
		,SUM(CASE WHEN "QuadClass" = 1 AND "eth"= 1 THEN 1 ELSE 0 END) as ethq1cnt
		,AVG(cast(NULLIF("ethq2at", 0) AS INT)) as ethq2at
		,AVG(cast(NULLIF("ethq2gs",0) AS INT)) as ethq2gs
		,SUM(CASE WHEN "QuadClass" = 2 AND "eth"= 1 THEN 1 ELSE 0 END) as ethq2cnt
		,AVG(cast(NULLIF("ethq3at", 0) AS INT)) as ethq3at
		,AVG(cast(NULLIF("ethq3gs",0) AS INT)) as ethq3gs
		,SUM(CASE WHEN "QuadClass" = 3 AND "eth"= 1 THEN 1 ELSE 0 END) as ethq3cnt
		,AVG(cast(NULLIF("ethq4at", 0) AS INT)) as ethq4at
		,AVG(cast(NULLIF("ethq4gs",0) AS INT)) as ethq4gs
		,SUM(CASE WHEN "QuadClass" = 4 AND "eth"= 1 THEN 1 ELSE 0 END) as ethq4cnt
		
FROM public.gdelt_y 
GROUP BY "ActionGeo_CountryCode"
		  ,"year"
		  ,"month"
ORDER BY "ActionGeo_CountryCode"
			,"year"
            ,"month"
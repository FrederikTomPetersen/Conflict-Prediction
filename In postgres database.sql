98
 GB



-- Lav backup
CREATE TABLE gdelt_y_m_d AS
SELECT *
	  FROM public.gdelt_y_m_d_full
	  Limit 1000;

-- Antal observationer der bidrager til analysen
SELECT COUNT("GLOBALEVENTID") FROM gdelt_y_m_d_full

-- Lav testdata
CREATE TABLE gdelt_y_m_d AS
SELECT *
	  FROM public.gdelt_y_m_d_full
	  Limit 1000;


 -- Tilføj nødvendige kolonner     
 ALTER TABLE public.gdelt_y_m_d_full
	ADD "maanedaar" character(6),
    ADD "year" character(4),
	ADD "month" character(2);
	
-- Indsæt data i hovedkolonne
UPDATE public.gdelt_y_m_d_full
	SET "maanedaar" = "MonthYear";

-- Brug hovedkolonne til at indsætte subset    
UPDATE public.gdelt_y_m_d_full
	SET year = SUBSTRING("maanedaar", 1,4),
     month = SUBSTRING("maanedaar",5,2);
    
-- lad os komme af med rapporter om voldelige begivenheder 	
DELETE FROM public.gdelt_y_m_d
	WHERE "EventCode" IN (145, 1451, 14521, 1454);
	
	
-- Lav grupperet tabel	
 CREATE TABLE public.gdelt_y_m_d_group AS
	SELECT 
		 "ActionGeo_CountryCode" as Country
        ,"year"
        ,"month"
        ,SUM("NumMentions") as nummentions
		,AVG("GoldsteinScale") as goldstein
		,AVG("AvgTone") as AvgTone
		,sum("q1nm") as q1nm
		,AVG(cast(NULLIF("q1at", 0) AS BIGINT)) as q1at
		,AVG(cast(NULLIF("q1gs", 0) AS BIGINT)) as q1gs
        ,SUM(CASE WHEN "QuadClass" = 1 THEN 1 ELSE 0 END) as q1cnt
		,sum("q2nm") as q2nm
		,AVG(cast(NULLIF("q2at", 0) AS BIGINT)) as q2at
		,AVG(cast(NULLIF("q2gs", 0) AS BIGINT)) as q2gs
        ,SUM(CASE WHEN "QuadClass" = 2 THEN 1 ELSE 0 END) as q2cnt
		,sum("q3nm") as q3nm
		,AVG(cast(NULLIF("q3at", 0) AS BIGINT)) as q3at
		,AVG(cast(NULLIF("q3gs", 0) AS BIGINT)) as q3gs
        ,SUM(CASE WHEN "QuadClass" = 3 THEN 1 ELSE 0 END) as q3cnt
		,sum("q4nm") as q4nm
		,AVG(cast(NULLIF("q4at", 0) AS BIGINT)) as q4at
		,AVG(cast(NULLIF("q4gs", 0) AS BIGINT)) as q4gs
        ,SUM(CASE WHEN "QuadClass" = 4 THEN 1 ELSE 0 END) as q4cnt
		
		,AVG(cast(NULLIF("relq1at", 0) AS BIGINT)) as relq1at
		,AVG(cast(NULLIF("relq1gs",0) AS BIGINT)) as relq1gs
		,SUM(CASE WHEN "QuadClass" = 2 AND "rel"= 1 THEN 1 ELSE 0 END) as relq1cnt
		
		,AVG(cast(NULLIF("relq2at", 0) AS BIGINT)) as relq2at
		,AVG(cast(NULLIF("relq2gs",0) AS BIGINT)) as relq2gs
		,SUM(CASE WHEN "QuadClass" = 2 AND "rel"= 1 THEN 1 ELSE 0 END) as relq2cnt
		
		,AVG(cast(NULLIF("relq1at", 0) AS BIGINT)) as relq3at
		,AVG(cast(NULLIF("relq1gs",0) AS BIGINT)) as relq3gs
		,SUM(CASE WHEN "QuadClass" = 3 AND "rel"= 1 THEN 1 ELSE 0 END) as relq3cnt
		
		,AVG(cast(NULLIF("relq4at", 0) AS BIGINT)) as relq4at
		,AVG(cast(NULLIF("relq4gs",0) AS BIGINT)) as relq4gs
		,SUM(CASE WHEN "QuadClass" = 4 AND "rel"= 1 THEN 1 ELSE 0 END) as relq4cnt
		
		
FROM public.gdelt_y_m_d 
GROUP BY "ActionGeo_CountryCode"
		  ,"year"
		  ,"month"
ORDER BY "ActionGeo_CountryCode"
			,"year"
            ,"month"
			
			
			
------------------------------
ALTER TABLE gdelt_y_m_d
	ADD "q1nm" INTEGER 
	,ADD "q1at" DECIMAL 
	,ADD "q1gs" DECIMAL 
	,ADD "q2nm" INTEGER 
	,ADD "q2at" DECIMAL 
	,ADD "q2gs" DECIMAL 
	,ADD "q3nm" INTEGER 
	,ADD "q3at" DECIMAL 
	,ADD "q3gs" DECIMAL 
	,ADD "q4nm" INTEGER 
	,ADD "q4at" DECIMAL 
	,ADD "q4gs" DECIMAL;
			
UPDATE gdelt_y_m_d
	SET "q1nm" = "NumMentions"
	, "q1at" = "GoldsteinScale"
	, "q1gs" = "AvgTone"
WHERE "QuadClass" = 1;	
			
UPDATE gdelt_y_m_d
	SET "q2nm" = "NumMentions"
	, "q2at" = "GoldsteinScale"
	, "q2gs" = "AvgTone"
WHERE "QuadClass" = 2				
			
UPDATE gdelt_y_m_d
	SET "q3nm" = "NumMentions"
	, "q3at" = "GoldsteinScale"
	, "q3gs" = "AvgTone"
WHERE "QuadClass" = 3				
						
UPDATE gdelt_y_m_d
	SET "q4nm" = "NumMentions"
	, "q4at" = "GoldsteinScale"
	, "q4gs" = "AvgTone"
WHERE "QuadClass" = 4			


UPDATE gdelt_y_m_d
SET "q1nm" = 0 WHERE q1nm IS NULL; 
UPDATE gdelt_y_m_d
SET "q1at" = 0 WHERE q1at IS NULL;
UPDATE gdelt_y_m_d
SET "q1gs" = 0 WHERE q1gs IS NULL;


UPDATE gdelt_y_m_d
SET "q2nm" = 0 WHERE q2nm IS NULL; 
UPDATE gdelt_y_m_d
SET "q2at" = 0 WHERE q2at IS NULL;
UPDATE gdelt_y_m_d
SET "q2gs" = 0 WHERE q2gs IS NULL;

UPDATE gdelt_y_m_d
SET "q3nm" = 0 WHERE q3nm IS NULL; 
UPDATE gdelt_y_m_d
SET "q3at" = 0 WHERE q3at IS NULL;
UPDATE gdelt_y_m_d
SET "q3gs" = 0 WHERE q3gs IS NULL;

UPDATE gdelt_y_m_d
SET "q4nm" = 0 WHERE q4nm IS NULL; 
UPDATE gdelt_y_m_d
SET "q4at" = 0 WHERE q4at IS NULL;
UPDATE gdelt_y_m_d
SET "q4gs" = 0 WHERE q4gs IS NULL;
			
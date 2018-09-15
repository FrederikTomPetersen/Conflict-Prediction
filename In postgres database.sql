

-- Lav backup
CREATE TABLE test_sql AS
SELECT *
	  FROM public.gdelt_y_m_d_full
	  Limit 1000;

-- Antal observationer der bidrager til analysen
SELECT COUNT("GLOBALEVENTID") FROM gdelt_y_m_d_full

-- Lav testdata
CREATE TABLE test_sql AS
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
    
	
	
-- Lav grupperet tabel	
 CREATE TABLE public.gdelt_y_m_d_full_group AS
	SELECT 
		 "ActionGeo_CountryCode"
        ,"year"
        ,"month"
        ,"QuadClass"
        ,COUNT("NumMentions") as nummentions
		,AVG("GoldsteinScale") as goldstein
		,AVG("AvgTone") as AvgTone
FROM public.gdelt_y_m_d_full 
GROUP BY "ActionGeo_CountryCode"
		  ,"year"
		  ,"month"
		  ,"QuadClass"
ORDER BY "ActionGeo_CountryCode"
			,"year"
            ,"month"
			
			
			
			
------------------------------
ALTER TABLE test_sql
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
			
UPDATE test_sql
	SET "q1nm" = "NumMentions"
	, "q1at" = "GoldsteinScale"
	, "q1gs" = "AvgTone"
WHERE QuadClass = 1;	
			
UPDATE test_sql
	SET "q2nm" = "NumMentions"
	, "q2at" = "GoldsteinScale"
	, "q2gs" = "AvgTone"
WHERE QuadClass = 2				
			
UPDATE test_sql
	SET "q3nm" = "NumMentions"
	, "q3at" = "GoldsteinScale"
	, "q3gs" = "AvgTone"
WHERE QuadClass = 3				
						
UPDATE test_sql
	SET "q4nm" = "NumMentions"
	, "q4at" = "GoldsteinScale"
	, "q4gs" = "AvgTone"
WHERE QuadClass = 4			


UPDATE test_sql
SET "q1nm" = 0 WHERE q1nm IS NULL; 
UPDATE test_sql
SET "q1at" = 0 WHERE q1at IS NULL;
UPDATE test_sql
SET "q1gs" = 0 WHERE q1gs IS NULL;


UPDATE test_sql
SET "q2nm" = 0 WHERE q2nm IS NULL; 
UPDATE test_sql
SET "q2at" = 0 WHERE q2at IS NULL;
UPDATE test_sql
SET "q2gs" = 0 WHERE q2gs IS NULL;

UPDATE test_sql
SET "q3nm" = 0 WHERE q3nm IS NULL; 
UPDATE test_sql
SET "q3at" = 0 WHERE q3at IS NULL;
UPDATE test_sql
SET "q3gs" = 0 WHERE q3gs IS NULL;

UPDATE test_sql
SET "q4nm" = 0 WHERE q4nm IS NULL; 
UPDATE test_sql
SET "q4at" = 0 WHERE q4at IS NULL;
UPDATE test_sql
SET "q4gs" = 0 WHERE q4gs IS NULL;
			
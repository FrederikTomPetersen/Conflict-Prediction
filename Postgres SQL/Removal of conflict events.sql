
 

-- Sletning af observationer, der er direkte relaterede til konflikt. 	
DELETE FROM public.gdelt_y
WHERE "EventCode" IN (19,190,193,194,195,1951,1952,20,200,201,202,203,204,2041,2042,18,180,182,1822,1823,183,1831,1832,1833,1834,184,1451,1452,1453,1454);


-- Sletning af observationer, der er direkte relaterede til konflikt. 	
DELETE FROM public.gdelt_y_m
WHERE "EventCode" IN (19,190,193,194,195,1951,1952,20,200,201,202,203,204,2041,2042,18,180,182,1822,1823,183,1831,1832,1833,1834,184,1451,1452,1453,1454);

-- Sletning af observationer, der er direkte relaterede til konflikt. 	

DELETE FROM public.gdelt_y_m_d
WHERE "EventCode" IN (19,190,193,194,195,1951,1952,20,200,201,202,203,204,2041,2042,18,180,182,1822,1823,183,1831,1832,1833,1834,184,1451,1452,1453,1454);

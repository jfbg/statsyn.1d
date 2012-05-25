% makes the VPREMOON_original vecolity model file

model = [...
       0	1737	1.00	0.50	2.60
   1	1736	1.00	0.50	2.60
   1	1736	3.20	1.80	2.70	
  15	1722	3.20	1.80	2.70
  28	1710	4.35	2.50	2.80
  40	1697	5.50	3.20	2.80
  40	1697	7.70	4.40	3.30
 238	1499	7.70	4.40	3.30
 238	1499	7.80	4.40	3.40
 488	1249	7.80	4.40	3.40
 488	1249	7.60	4.40	3.40
 738	 999	7.60	4.40	3.40
 738	 999	8.50	4.50	3.40
1257	 480	8.50	4.50	3.40
1257	 480	7.50	3.20	3.40
1407	 330	7.50	3.20	3.40
1407	 330	4.10	0.00	5.10
1497	 240	4.10	0.00	5.10
1497	 240	4.30	2.30	8.00
1737	   0	4.30	2.30	8.00];

fid = fopen('WEBER_2011_original','w');

for ii = 1:length(model)
    fprintf(fid,'%4.2f %4.2f %2.2f %2.2f %2.4f\n',model(ii,:));
end

fclose(fid);
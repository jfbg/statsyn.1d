% makes the VPREMOON_original vecolity model file

model = [...
0	1737	1	0.5	2.6	6750
0.9	1736.1	1	0.5	2.6	6750
0.9	1736.1	3.2	1.8	2.762	6750
11.9	1725.1	3.2	1.8	2.762	6750
11.9	1725.1	5.5	3.3	2.762	6750
27.9	1709.1	5.5	3.3	2.762	6750
27.9	1709.1	7.54	4.34	3.312	6750
39.9	1697.1	7.55	4.34	3.314	6750
65.3	1671.7	7.57	4.35	3.318	9000
89.9	1647.1	7.59	4.36	3.322	9000
109.9	1627.1	7.61	4.37	3.325	9000
129.9	1607.1	7.63	4.38	3.329	9000
149.9	1587.1	7.64	4.39	3.332	9000
169.9	1567.1	7.66	4.4	3.335	9000
189.9	1547.1	7.68	4.4	3.338	9000
209.9	1527.1	7.69	4.41	3.341	9000
235	1502	7.71	4.42	3.344	9000
249.9	1487.1	7.72	4.43	3.346	9000
275.3	1461.7	7.74	4.44	3.35	3375
289.9	1447.1	7.75	4.44	3.352	3375
309.9	1427.1	7.77	4.45	3.355	3375
329.9	1407.1	7.78	4.45	3.357	3375
349.9	1387.1	7.8	4.46	3.36	3375
369.9	1367.1	7.81	4.47	3.363	3375
389.9	1347.1	7.82	4.47	3.365	3375
409.9	1327.1	7.84	4.48	3.368	3375
429.9	1307.1	7.85	4.49	3.37	3375
449.9	1287.1	7.86	4.49	3.373	3375
469.9	1267.1	7.88	4.5	3.375	3375
485	1252	7.88	4.5	3.377	3375
505.3	1231.7	7.9	4.51	3.379	1125
529.9	1207.1	7.91	4.51	3.382	1125
549.9	1187.1	7.92	4.52	3.384	1125
569.9	1167.1	7.94	4.53	3.386	1125
589.9	1147.1	7.95	4.53	3.388	1125
609.9	1127.1	7.96	4.54	3.391	1125
629.9	1107.1	7.97	4.54	3.393	1125
649.9	1087.1	7.98	4.54	3.395	1125
669.9	1067.1	7.99	4.55	3.397	1125
689.9	1047.1	8	4.55	3.398	1125
709.9	1027.1	8.01	4.56	3.4	1125
735	1002	8.02	4.56	3.403	1125
749.9	987.1	8.03	4.57	3.404	1125
775.3	961.7	8.04	4.57	3.406	675
789.9	947.1	8.05	4.57	3.408	675
809.9	927.1	8.06	4.58	3.409	675
829.9	907.1	8.07	4.58	3.411	675
849.9	887.1	8.08	4.58	3.413	675
869.9	867.1	8.08	4.59	3.414	675
889.9	847.1	8.09	4.59	3.416	675
909.9	827.1	8.1	4.59	3.417	675
929.9	807.1	8.11	4.6	3.419	675
949.9	787.1	8.12	4.6	3.42	675
969.9	767.1	8.12	4.6	3.421	675
989.9	747.1	8.13	4.61	3.423	675
1009.9	727.1	8.14	4.61	3.424	675
1029.9	707.1	8.14	4.61	3.425	675
1049.9	687.1	8.15	4.61	3.427	675
1069.9	667.1	8.16	4.62	3.428	675
1089.9	647.1	8.16	4.62	3.429	675
1109.9	627.1	8.17	4.62	3.43	675
1129.9	607.1	8.18	4.62	3.431	675
1149.9	587.1	8.18	4.62	3.433	675
1169.9	567.1	8.19	4.63	3.434	675
1189.9	547.1	8.19	4.63	3.435	675
1209.9	527.1	8.2	4.63	3.436	675
1229.9	507.1	8.2	4.63	3.437	675
1249.9	487.1	8.21	4.63	3.438	675
1269.9	467.1	8.21	4.63	3.438	675
1289.9	447.1	8.22	4.63	3.439	675
1309.9	427.1	8.22	4.64	3.44	675
1329.9	407.1	8.23	4.64	3.441	675
1349.9	387.1	8.23	4.64	3.442	675
1357	380	8.23	4.64	3.442	675
1357	380	9	5	5.171	10000
1737	0	9	5	5.171	10000];

fid = fopen('VPREMOON_Qp_ori','w');

for ii = 1:length(model)
    fprintf(fid,'%4.2f %4.2f %2.2f %2.2f %2.4f %.0f\n',model(ii,:));
end

fclose(fid);
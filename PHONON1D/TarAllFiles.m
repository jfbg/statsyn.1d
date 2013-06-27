% this script makes .mat files of average_output files


% list of output to convert:

folder = 'OUTPUT/';
tarfolder = 'OUTPUT/TAR_GZ_OUTPUT/';
st = '*';

list = {...
'VPREM_001B'
'VPREM_008B'
'VPREM_021'
'VPREM_022'
'VPREM_023'
'VPREM_030'
'VPREM_031'
'VPREM_nC_001B'
'VPREM_nVL_001B'
'VPREM_nVL_liquid_001B'
    };

fid = fopen('tar_models.sh','w');

%% Generate list files + .csh script

for ii = 1:length(list)

    fprintf(fid,'tar -cvzf %s%s.tar.gz %s%s%s.%s\n',tarfolder,list{ii},folder,list{ii},st,st);
    
end


fclose(fid);

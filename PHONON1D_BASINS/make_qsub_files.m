% make the qsub submission shell for the input list of model names

list = {...
'CSMOON_012a'
'CSMOON_001'
'CSMOON_001b'
'CSMOON_002a'
'CSMOON_002b'
'CSMOON_003a'
'CSMOON_003b'
'CSMOON_003c'
'CSMOON_004a'
'CSMOON_004b'
'CSMOON_004c'
'CSMOON_005a'
'CSMOON_006a'
'CSMOON_007a'
'CSMOON_008a'
'CSMOON_009a'
'CSMOON_010a'
'CSMOON_010b'
'CSMOON_011a'
'CSMOON_013a'
};

folder = 'CEES_QSUB_SCRIPTS/';

fid1 = fopen('run_all_qsub.sh','w');
fprintf(fid1,'#!/bin/tcsh\n');
fprintf(fid1,'\n');
fprintf(fid1,'\n');

for ii = 1:length(list)
    
    fid = fopen([folder 'qsub_' list{ii} '.sh'],'w');
    
    fprintf(fid,'#!/bin/tcsh\n');
    fprintf(fid,'#PBS -N %s\n',list{ii});
    fprintf(fid,'#PBS -l nodes=1:ppn=16\n');
    fprintf(fid,'#PBS -q jfl\n');
    fprintf(fid,'#PBS -V\n');
    fprintf(fid,'cd $PBS_O_WORKDIR\n');
    fprintf(fid,'\n');
    fprintf(fid,'csh SCRIPTS_CEES/%s.csh\n',list{ii});

    fclose(fid);
    
    fprintf(fid1,'qsub %sqsub_%s.sh\n',folder,list{ii});
    fprintf(fid1,'sleep 4\n');

end

fclose(fid1); 

    

clear all
clc

%%{    
% UNCOMMENT TO RETRIEVE OUTPUT FILE LIST FROM CEES SERVER
!getout
%}
% Read OUTPUT list
fid = fopen('./OUTPUTfileLIST');
outputlist = textscan(fid,'%s','delimiter','\n');
outputlist = outputlist{1};  
fclose(fid);

%WRITE list of models for which a averageoutput shell will be written

modellist = {...
...'VPREM_002'
...'VPREM_012B'
'VPREM_024'
'VPREM_nCORE_001'
...'VPREM_042'
...'VPREM_043'
...'VPREM_051'
...'VPREM_052'
...'VPREM_061'
...'VPREM_001C'
...'VPREM_liquid_001'
...'VPREM_nVL_liquid_001C'
};

depths = [.01 20 100 500 750 1000]; % All potential depths
freqs = [7 40];                     % All potential frequencies
kerns = 16;                 
iters = 4;

outputfolder = '../OUTPUT/';
scriptfolder = './AVERAGING_SCRIPTS/';
qsubfolder = './QSUB_SCRIPTS/';

% Open main qsub submission file to run all of the averaging below

fidqsub = fopen('qsub_allaverages.sh','w');
fprintf(fidqsub,'#!/bin/tcsh\n');
fidall = fopen('tool_allaverages.sh','w');


%% Generate list files + .csh script

grandtot = 0;

for qq = 1:length(modellist)
    
fidscr = fopen([scriptfolder modellist{qq} '_average.csh'],'w');
fprintf(fidscr,'#!/bin/csh\n\n');

totfile = 0;  %If totfile ==0, not need to keep the .csh file

for kk = 1:length(depths)
    for hh = 1:length(freqs)
        
    avesrc = 'average_output';

    if freqs(hh) == 7, dt = 0.15; end
    if freqs(hh) == 40, dt = 0.025; end
    if freqs(hh) == 1, dt = 1.00; end       % was used during Benchmarking
    
    
    %Check for any file for freq/depth combination
    
    sumR = 0;   % If sum > 1, then write up in the csh file
    sumZ = 0;
    sumT = 0;
    
    
    listnameR = sprintf('LISTS/%s.%04.0f.%02.0f.lpr.list',...
                    modellist{qq},depths(kk),freqs(hh));
    listnameZ = sprintf('LISTS/%s.%04.0f.%02.0f.lpz.list',...
                    modellist{qq},depths(kk),freqs(hh));
    listnameT = sprintf('LISTS/%s.%04.0f.%02.0f.lpt.list',...
                    modellist{qq},depths(kk),freqs(hh));
    
    fidR = fopen(listnameR,'w');
    fidZ = fopen(listnameZ,'w');
    fidT = fopen(listnameT,'w');
    
    
    for ii = 1:kerns
        for jj = 1:iters

    
        filenameR = sprintf('%s.%.0f.%.0f.%.0f.%02.0f.lpr',...
                    modellist{qq},depths(kk),ii,jj,freqs(hh));

        filenameZ = sprintf('%s.%.0f.%.0f.%.0f.%02.0f.lpz',...
                    modellist{qq},depths(kk),ii,jj,freqs(hh));
                
        filenameT = sprintf('%s.%.0f.%.0f.%.0f.%02.0f.lpt',...
                    modellist{qq},depths(kk),ii,jj,freqs(hh));


            if depths(kk) == 0.01
            filenameR = sprintf('%s.0.01.%.0f.%.0f.%02.0f.lpr',...
                        modellist{qq},ii,jj,freqs(hh));

            filenameZ = sprintf('%s.0.01.%.0f.%.0f.%02.0f.lpz',...
                        modellist{qq},ii,jj,freqs(hh));

            filenameT = sprintf('%s.0.01.%.0f.%.0f.%02.0f.lpt',...
                        modellist{qq},ii,jj,freqs(hh));
            end
                        
              
                        
        if sum(strcmp(filenameR,outputlist)) == 1
            fprintf(fidR,'%s\n',[outputfolder filenameR]);
            sumR = sumR +1;
        end

        if sum(strcmp(filenameZ,outputlist)) == 1
            fprintf(fidZ,'%s\n',[outputfolder filenameZ]);
            sumZ = sumZ +1;
        end
        
        if sum(strcmp(filenameT,outputlist)) == 1
            fprintf(fidT,'%s\n',[outputfolder filenameT]);
            sumT = sumT +1;
        end

        end
    end
    
    fclose(fidR);
    fclose(fidZ);
    fclose(fidT);

    totfile = totfile +sumR +sumZ +sumT;
    grandtot = grandtot + totfile;
                
    if sumR > 0                       
        outputR = sprintf('AVERAGED_OUTPUTS/%s.%04.0f.%02.0f.lpr',...
                        modellist{qq},depths(kk),freqs(hh));
        fprintf(fidscr,'%s << EOF\n',avesrc);
        fprintf(fidscr,'./%s\n',listnameR);
        fprintf(fidscr,'./%s\n',outputR);
        fprintf(fidscr,'%.4f\n',dt);
        fprintf(fidscr,'EOF\n\n');                  
    else
        eval(sprintf('!\\rm %s',listnameR))
    end
        
    if sumZ > 0            
        outputZ = sprintf('AVERAGED_OUTPUTS/%s.%04.0f.%02.0f.lpz',...
                    modellist{qq},depths(kk),freqs(hh));
        fprintf(fidscr,'%s << EOF\n',avesrc);
        fprintf(fidscr,'./%s\n',listnameZ);
        fprintf(fidscr,'./%s\n',outputZ);
        fprintf(fidscr,'%.4f\n',dt);
        fprintf(fidscr,'EOF\n\n'); 
    else
        eval(sprintf('!\\rm %s',listnameZ))
    end                
                
    if sumT > 0           
        outputT = sprintf('AVERAGED_OUTPUTS/%s.%04.0f.%02.0f.lpt',...
                    modellist{qq},depths(kk),freqs(hh));
        fprintf(fidscr,'%s << EOF\n',avesrc);
        fprintf(fidscr,'./%s\n',listnameT);
        fprintf(fidscr,'./%s\n',outputT);
        fprintf(fidscr,'%.4f\n',dt);
        fprintf(fidscr,'EOF\n\n');
    else
        eval(sprintf('!\\rm %s',listnameT))
    end      

    end
end

fclose(fidscr);

if totfile == 0
    eval(['!\rm ' scriptfolder modellist{qq} '_average.csh'])
else
    fidtemp = fopen([qsubfolder 'qsub_' modellist{qq} '_average.sh'],'w');
    fprintf(fidtemp,'#!/bin/tcsh\n');
    fprintf(fidtemp,'#PBS -N %s\n',['COMPILE_' modellist{qq}]);
    fprintf(fidtemp,'#PBS -lmem=12gb,nodes=1:ppn=1\n');
    fprintf(fidtemp,'#PBS -q jfl\n');
    fprintf(fidtemp,'#PBS -V\n');
    fprintf(fidtemp,'cd $PBS_O_WORKDIR\n\n');
    fprintf(fidtemp,'csh %s\n',[scriptfolder modellist{qq} '_average.csh']);
    fclose(fidtemp);
    
    fprintf(fidqsub,'qsub %s\n',[qsubfolder 'qsub_' modellist{qq} '_average.sh']);
    fprintf(fidall,'csh %s\n',[scriptfolder modellist{qq} '_average.csh']);
end

end

fclose(fidqsub);
fclose(fidall);

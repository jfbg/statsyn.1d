clear all
clc

%%{    
% UNCOMMENT TO RETRIEVE OUTPUT FILE LIST FROM CEES SERVER
% !getout
!ls ../OUTPUT/ > ./OUTPUTfileLIST_GCC
%}
% Read OUTPUT list
fid = fopen('./OUTPUTfileLIST_GCC');
outputlist = textscan(fid,'%s','delimiter','\n');
outputlist = outputlist{1};  
fclose(fid);

%WRITE list of models for which a averageoutput shell will be written

modellist = {...
'BMF_EARTHPREM_SPIKE_100km_EXP_noATT_AllPs'
'BMF_MOON2LAYERS_700km_noATT_AllPs'
};

% depths = .01;
% depths = [.01 20 30 50 100 150 800 500 750 1000]; % All potential depths
depths = [0.01 30 50 1000 700 100]; % All potential depths
% depths = [0.01];
freqs = [7];%2 5 7 10 20 1];                     % All potential frequencies
kerns = 16;                  
iters = 15;

outputfolder = '../OUTPUT/';
scriptfolder = './AVERAGING_SCRIPTS/';
qsubfolder = './QSUB_SCRIPTS/';

% Open main qsub submission file to run all of the averaging below

fidall =  fopen('matGCC_averagelist.list','w');
fiddt  =  fopen('matGCC_dt.list','w');
fidout  = fopen('matGCC_outputs.list','w');


%% Generate list files + .csh script

grandtot = 0;

for qq = 1:length(modellist)
    
totfile = 0;  %If totfile ==0, not need to keep the .csh file

for kk = 1:length(depths)
    for hh = 1:length(freqs)
        
    avesrc = 'average_output';

    if freqs(hh) == 7, dt = 0.15; 
    elseif freqs(hh) == 40, dt = 0.025;
    elseif freqs(hh) == 1, dt = 1.00;       % was used during Benchmarking
    elseif freqs(hh) == 5, dt = 0.20; 
    elseif freqs(hh) == 20, dt = 0.05;
    elseif freqs(hh) == 2, dt = 0.5;
    elseif freqs(hh) == 10, dt = 0.1;
    else
        error('Input new frequency');
    end
    
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
            
%             if freqs(hh) == 2
%             filenameR = sprintf('%s.%.0f.%.0f.%.0f.%.0f.lpr',...
%                         modellist{qq},depths(kk),ii,jj,freqs(hh));
% 
%             filenameZ = sprintf('%s.%.0f.%.0f.%.0f.%.0f.lpz',...
%                         modellist{qq},depths(kk),ii,jj,freqs(hh));
% 
%             filenameT = sprintf('%s.%.0f.%.0f.%.0f.%.0f.lpt',...
%                         modellist{qq},depths(kk),ii,jj,freqs(hh));
%             end
                        
              
                        
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
        fprintf(fidall,'%s\n',listnameR);
        fprintf(fiddt,'%.4f\n',dt);
        
        outputR = sprintf('AVERAGED_OUTPUTS/%s.%04.0f.%02.0f.lpr',...
                        modellist{qq},depths(kk),freqs(hh));
        fprintf(fidout,'%s\n',outputR);         
    else
        eval(sprintf('!\\rm %s',listnameR))
    end
        
    if sumZ > 0            
        fprintf(fidall,'%s\n',listnameZ); 
        fprintf(fiddt,'%.4f\n',dt);
        
        outputZ = sprintf('AVERAGED_OUTPUTS/%s.%04.0f.%02.0f.lpz',...
                        modellist{qq},depths(kk),freqs(hh));
        fprintf(fidout,'%s\n',outputZ);  
    else
        eval(sprintf('!\\rm %s',listnameZ))
    end                
                
    if sumT > 0           
        fprintf(fidall,'%s\n',listnameT); 
        fprintf(fiddt,'%.4f\n',dt);
        
        outputT = sprintf('AVERAGED_OUTPUTS/%s.%04.0f.%02.0f.lpt',...
                        modellist{qq},depths(kk),freqs(hh));
        fprintf(fidout,'%s\n',outputT);  
    else
        eval(sprintf('!\\rm %s',listnameT))
    end      

    end
end

end


fclose(fidall);

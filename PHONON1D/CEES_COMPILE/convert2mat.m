% this script makes .mat files of average_output files


% list of output to convert:

folder = 'AVERAGED_OUTPUTS/';

list = {...
'RSMOON_001.0000.40.lpr'
'RSMOON_001.0000.40.lpt'
'RSMOON_001.0000.40.lpz'
'RSMOON_001.1000.40.lpr'
'RSMOON_001.1000.40.lpt'
'RSMOON_001.1000.40.lpz'
'RSMOON_002a.1000.40.lpr'
'RSMOON_002a.1000.40.lpt'
'RSMOON_002a.1000.40.lpz'
'RSMOON_004a.0000.40.lpr'
'RSMOON_004a.0000.40.lpt'
'RSMOON_004a.0000.40.lpz'
'RSMOON_004a.1000.40.lpr'
'RSMOON_004a.1000.40.lpt'
'RSMOON_004a.1000.40.lpz'
'RSMOON_008a.0000.40.lpr'
'RSMOON_008a.0000.40.lpt'
'RSMOON_008a.0000.40.lpz'
'RSMOON_008a.1000.40.lpr'
'RSMOON_008a.1000.40.lpt'
'RSMOON_008a.1000.40.lpz'
'RSMOON_010b.0000.40.lpr'
'RSMOON_010b.0000.40.lpt'
'RSMOON_010b.0000.40.lpz'
'RSMOON_010b.1000.40.lpr'
'RSMOON_010b.1000.40.lpt'
'RSMOON_010b.1000.40.lpz'
'RSMOON_011a.1000.40.lpr'
'RSMOON_011a.1000.40.lpt'
'RSMOON_011a.1000.40.lpz'
};


%% Generate list files + .csh script

for ii = 1:length(list)

    data = load([folder list{ii}]);
    
    t = data(2:end,1);
    dist = data(1,2:end);
    d = data(2:end,2:end);
    
    save([folder list{ii} '.mat'],'t','dist','d');
    
end


%%

%{

for ii = 1:size(d,2)
    figure(1)
    clf
    plot(t,d(:,ii))
    
    title(sprintf('ED = %.1f?',dist(ii)));
    
    pause
end


%}
    

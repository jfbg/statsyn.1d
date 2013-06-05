% this script makes .mat files of average_output files


% list of output to convert:

list = {...
'BM_EARTHPREM_SPIKE_100km_AMP.0100.01.lpr'
'BM_EARTHPREM_SPIKE_100km_AMP.0100.01.lpt'
'BM_EARTHPREM_SPIKE_100km_AMP.0100.01.lpz'
'BM_EARTHPREM_SPIKE_700km_AMP.0700.01.lpr'
'BM_EARTHPREM_SPIKE_700km_AMP.0700.01.lpt'
'BM_EARTHPREM_SPIKE_700km_AMP.0700.01.lpz'    };


%% Generate list files + .csh script

for ii = 1:length(list)

    data = load(list{ii});
    
    t = data(2:end,1);
    dist = data(1,2:end);
    d = data(2:end,2:end);
    
    save([list{ii} '.mat'],'t','dist','d');
    
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
    

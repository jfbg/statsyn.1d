% this script makes .mat files of average_output files


% list of output to convert:

folder = 'AVERAGED_OUTPUTS/';

list = {...
'BM_EARTHPREM_SINE_100km_dt02_CEES.0100.05.lpr'
'BM_EARTHPREM_SINE_100km_dt02_CEES.0100.05.lpt'
'BM_EARTHPREM_SINE_100km_dt02_CEES.0100.05.lpz'
'BM_EARTHPREM_SPIKE_100km_dt02_CEES.0100.05.lpr'
'BM_EARTHPREM_SPIKE_100km_dt02_CEES.0100.05.lpt'
'BM_EARTHPREM_SPIKE_100km_dt02_CEES.0100.05.lpz'
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
    

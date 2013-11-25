% this script makes .mat files of average_output files


% list of output to convert:

folder = 'AVERAGED_OUTPUTS/';

list = {...
'BSMOON_001.0000.40.lpr'
'BSMOON_001.0000.40.lpt'
'BSMOON_001.0000.40.lpz'
'BSMOON_002a.0000.40.lpr'
'BSMOON_002a.0000.40.lpt'
'BSMOON_002a.0000.40.lpz'
% 'CSMOON_001_noscat_OnlyPS_C.0000.40.lpr'
% 'CSMOON_001_noscat_OnlyPS_C.0000.40.lpt'
% 'CSMOON_001_noscat_OnlyPS_C.0000.40.lpz'
% 'CSMOON_001_noscat_OnlyPS.0020.40.lpr'
% 'CSMOON_001_noscat_OnlyPS.0020.40.lpt'
% 'CSMOON_001_noscat_OnlyPS.0020.40.lpz'
% 'CSMOON_001_noscat_OnlyPS.1000.40.lpr'
% 'CSMOON_001_noscat_OnlyPS.1000.40.lpt'
% 'CSMOON_001_noscat_OnlyPS.1000.40.lpz'
% 'CSMOON_001_noscat_OnlyPS_noatt_B.0000.40.lpr'
% 'CSMOON_001_noscat_OnlyPS_noatt_B.0000.40.lpt'
% 'CSMOON_001_noscat_OnlyPS_noatt_B.0000.40.lpz'
% 'CSMOON_001_noscat_OnlyPS_noatt.0020.40.lpr'
% 'CSMOON_001_noscat_OnlyPS_noatt.0020.40.lpt'
% 'CSMOON_001_noscat_OnlyPS_noatt.0020.40.lpz'
% 'CSMOON_001_noscat_OnlyPS_noatt.1000.40.lpr'
% 'CSMOON_001_noscat_OnlyPS_noatt.1000.40.lpt'
% 'CSMOON_001_noscat_OnlyPS_noatt.1000.40.lpz'
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
    

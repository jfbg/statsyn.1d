% this script makes .mat files of average_output files


% list of output to convert:

folder = 'AVERAGED_OUTPUTS/';

list = {...
'SMOON_012a.0000.40.lpr'
'SMOON_012a.0000.40.lpt'
'SMOON_012a.0000.40.lpz'
'SMOON_012a.0020.40.lpr'
'SMOON_012a.0020.40.lpt'
'SMOON_012a.0020.40.lpz'
'SMOON_012a.1000.40.lpr'
'SMOON_012a.1000.40.lpt'
'SMOON_012a.1000.40.lpz'
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
    

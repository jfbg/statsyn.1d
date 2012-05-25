% Plots the velocity models used in PHONON1D

model_folder = '/Users/jf/Documents/UBC/Moon/Phonon1D/PHONON1D/MODELS/';
fig_folder = '/Users/jf/Documents/UBC/Moon/Phonon1D/PHONON1D/MODELS/FIGURES/';

modelnames = {...
    'iasp91'
    'luna_beyn_base'
    'luna_beyn_base_crust'
    'luna_beyn_int'
    'luna_nakamura'
    'luna_nakamura_2'
    'luna_nakamura_3'
    'luna_nakamura_core_000'
    'luna_nakamura_core_100'
    'luna_nakamura_core_200'
    'luna_nakamura_core_300'
    'luna_nakamura_core_300_400'
    'luna_nakamura_core_400'
    'luna_nakamura_int'
    'luna_nakamura_int_2'
    'luna_nakamura_int_core_000'
    'luna_nakamura_int_core_100'
    'luna_nakamura_int_core_200'
    'luna_nakamura_int_core_300'
    'luna_nakamura_int_core_300_400'
    'luna_nakamura_int_core_400'
    'prem'
    'prem_moon'
    'prem_moon_int'};


for ii = 1:length(modelnames)
    
    model = load([model_folder modelnames{ii}]);
    
    figure(1)
    clf
    
    subplot(2,2,[1 3])
    hold on
    plot(model(:,3),model(:,2),'b-')
    plot(model(:,4),model(:,2),'r-')
    
    if size(model,2) > 4
        plot(model(:,5),model(:,2),'k--')
    end
    
    xl = get(gca,'xlim');
    nxl = [-2 max(xl)];
    xlim(nxl)
    plot(nxl,[max(model(:,2)) max(model(:,2))],'k')
    ylabel('Radius (km)')
    xlabel(sprintf('Velocities (km/s) / Density (g/cm^3)'))
    ylim([0 max(model(:,2))+50])
    title(sprintf('%.2f km',max(model(:,2))))
    
    subplot(2,2,[2 4])
    hold on
    plot(model(:,3),model(:,2),'b-')
    plot(model(:,4),model(:,2),'r-')
    
    if size(model,2) > 4
        plot(model(:,5),model(:,2),'k--')
    end
    
    plot(nxl,[max(model(:,2)) max(model(:,2))],'k')
    ylim([max(model(:,2))-100 max(model(:,2))+10])
    xlim(nxl)
    title('Top 100 km')
    
    
    if size(model,2) > 4
        legend('P-wave velocity','S-wave Velocity','Density','Surface',...
               'Location','SO')
    else
        legend('P-wave velocity','S-wave Velocity','Surface','Location','SO')
    end
    
    
    ha = axes('Position',[0 0 1 1],'Xlim',[0 1],'Ylim',[0 1],...
            'Box','off','Visible','off','Units','normalized', 'clipping' , 'off');

    ta = text(0.5, 1,modelnames{ii},'HorizontalAlignment',...
            'center','VerticalAlignment', 'top');
    set(ta,'interpreter','none')
    
    set(gcf, 'PaperUnits', 'inches');
    set(gcf, 'PaperPositionMode', 'manual');
    set(gcf, 'PaperPosition', [0 0 6 9]);
    print(gcf, '-depsc2', [fig_folder modelnames{ii} '.eps']);
    
    
    
%     pause
    
end
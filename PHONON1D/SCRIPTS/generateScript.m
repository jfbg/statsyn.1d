function generateScript(data)


fid = fopen(data.Scriptfile,'w');

fprintf(fid,'#!/bin/csh\n\n');

modelname = sprintf('moR%04.0f',data.ID);

% Interpolate Model for the Run.
fprintf(fid,'./MODELS/interp_model << EOF\n');
fprintf(fid,'./MODELS/ORIGINALS/%s\n',data.Model);
fprintf(fid,'%.2f\n',data.SLtop);
fprintf(fid,'%.2f\n',data.SLbtm);
fprintf(fid,'./MODELS/%s\n',modelname);
fprintf(fid,'%.4f\n',data.DistScat);
fprintf(fid,'EOF\n\n\n');


 
ray_par    = '0.0 0.1668 0.2931';
d_range    = '0 180 91';
rand_kern    = '155';


commandlines = {...
'cd SRC'
'make tracktime'
'cd ..'
' '
' '
'./bin/statsyn_1D_track_time << EOF'
['./MODELS/' modelname]
'1'
ray_par                                 %LIMIT THE RAY PARAMETER
sprintf('%f %f %f',data.t_start,data.t_max,data.dt) %LIMIT THE TIME WINDOW (SECONDS)
d_range                                 %LIMIT THE DISTANCE (DEGREES)
num2str(data.n_phonons)                  %NUMBER OF PHONONS TO FIRE
rand_kern                               %RANDOMIZING KERNEL TO USE
num2str(data.SourceDepth)               %DEPTH OF SOURCE
'2'                                     %1=EARTH, 2=MOON
'1'                                     %1=P, 2=SH WAVES
num2str(data.SLThickness)               %MAX SCATTERING DEPTH
num2str(data.ScatProb)                  %SCATTERING PROB (SIMILAR TO RMS)
['TRACKING/' data.Trackoutfile]         % Track File
['OUTPUT/' data.Dataoutfile]
'EOF'
};

for ii = 1:length(commandlines)
    fprintf(fid,'%s\n',commandlines{ii});
end

%% 
eval(['!chmod +x ' data.Scriptfile])

fprintf('Script %s Generated\n',data.Scriptfile);
fclose (fid);

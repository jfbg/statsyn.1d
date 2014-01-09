% Load list of runs in mat3_averagelist.list and compile the listed files.
% Assumes  distance range is 0-180

fid = fopen('mat3_averagelist.list','r');
run_names = textscan(fid,'%s','delimiter','\n');
run_names = run_names{1};

fid = fopen('mat3_outputs.list','r');
run_out = textscan(fid,'%s','delimiter','\n');
run_out = run_out{1};

dt = load('mat3_dt.list');

for ii = 1:size(run_names,1)

   fid = fopen(run_names{ii},'r');
   run_list = textscan(fid,'%s','delimiter','\n');
   run_list = run_list{1};

   for jj = 1:size(run_list,1)

      if jj == 1
         d = load(run_list{jj}, '-ascii');
      else
         dt = load(run_list{jj}, '-ascii');
         d = d + dt;
      end

   fprintf('%s  --  %03.0f / %03.0f\n',run_names{ii},jj,size(run_list,1))
   end

   t = ((0:(size(d)-2))*dt(ii))';
   dist = linspace(0,180,size(d,2)-1);
   d = d(2:end,2:end)/size(run_list,1);

   save([run_out{ii} '.mat'],'d','t','dist')
   clc
end

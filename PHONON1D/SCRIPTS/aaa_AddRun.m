% Script generates new script file based on info below, assign a number and
% a name and enters info in RUNS databses (runs.mat)

% RUNS.mat

%% Description

% RUNS.R(RunID):
% 
% Model
% NumSL
% SLThickness
% SLtop
% SLbtm
% DistScat
% ScatProb
% SourceDepth
% t_start
% t_max
% dt
% freq
% n_phonons
% n_kernel
% n_iter
% Scriptfile
% dataoutfile
% trackoutfile

%% Enter Info

Model =         'VPREMOON_mod';
NumSL =         1;
SLThickness =   10;
SLtop =         0;
SLbtm =         10;
DistScat =      .25;
ScatProb =      .5;
SourceDepth =   .01;
t_start =       0;
t_max =         3600;
dt =            .08;
freq =          1/dt;
n_phonons =     5000000;
n_kernel =      1;
n_iter =        1;
Machine =       'Thor';

% Folders
ScriptFolder = './';

%% Add data to RUNS database
load RUNS.mat

newid = RUNS.TotalID +1;

pref = sprintf('RUNS.R%04.0f',newid);

eval([pref '.ID = newid;'])
eval([pref '.Model = Model;'])
eval([pref '.NumSL = NumSL;'])
eval([pref '.SLThickness = SLThickness;'])
eval([pref '.SLtop = SLtop;'])
eval([pref '.SLbtm = SLbtm;'])
eval([pref '.DistScat = DistScat;'])
eval([pref '.ScatProb = ScatProb;'])
eval([pref '.SourceDepth = SourceDepth;'])
eval([pref '.t_start = t_start;'])
eval([pref '.t_max = t_max;'])
eval([pref '.dt = dt;'])
eval([pref '.freq = freq;'])
eval([pref '.n_phonons = n_phonons;'])
eval([pref '.n_kernel = n_kernel;'])
eval([pref '.n_iter = n_iter;'])

SF = sprintf('RUN%04.0f.csh',newid);
DO = sprintf('RUN%04.0f.data.out',newid);
TO = sprintf('RUN%04.0f.track.out',newid);

eval([pref '.Scriptfile = SF;'])
eval([pref '.Dataoutfile = DO;'])
eval([pref '.Trackoutfile = TO;'])
eval([pref '.Machine = Machine;'])
eval([pref '.DateCreated = date;'])

RUNS.TotalID = newid;
save RUNS.mat RUNS

%% Generate script

eval(['generateScript(' pref ');'])

%% Reinitialize RUNS (Be careful with this....)
% % 
% % clear RUNS
% % RUNS.TotalID = 0;
% % save RUNS.mat RUNS

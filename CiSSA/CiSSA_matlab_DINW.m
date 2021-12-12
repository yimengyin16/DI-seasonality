
% Signal Processing Toolbox required

%% Preparation
addpath CiSSA_matlab\;


%% Loading data
df_DINW = readtable("C:\Git\DI-seasonality\CiSSA\data_DINW.csv");
df_DINW;

m_SSDI = table2array(df_DINW(:, "SSDI"));
m_SSI  = table2array(df_DINW(:, "SSI"));
m_conc = table2array(df_DINW(:, "Concurrent"));


%% Do CiSSA
% determine the value of L
% T = 251 for 2000:10 to 2021:8
% L must below T/2 = 125.5, 
% and multiple of the largest frequency considered (8 years, 96 months for business cycle)
% Use L = 96

L = 96;
[Z_SSDI, psd_SSDI] = cissa(m_SSDI, L, 1);
[Z_SSI,  psd_SSI]  = cissa(m_SSI, L, 1);
[Z_conc, psd_conc] = cissa(m_conc, L, 1);

%% Reconstructing trend, business cycle and seasonal components
I = 12; % for 12 months in a year
[rc_SSDI, sh_SSDI, kg_SSDI] = group(Z_SSDI, psd_SSDI,I);
[rc_SSI,  sh_SSI,  kg_SSI]  = group(Z_SSI,  psd_SSI, I);
[rc_conc, sh_conc, kg_conc] = group(Z_conc, psd_conc,I);

%% write to csv
writematrix(rc_SSDI, "rc_SSDI_NW.csv");
writematrix(rc_SSI,  "rc_SSI_NW.csv");
writematrix(rc_conc, "rc_conc_NW.csv");

%%
rc_SSDI;
rc_SSI;
rc_conc;

%%
sh_SSDI;
sh_SSI;
sh_conc;








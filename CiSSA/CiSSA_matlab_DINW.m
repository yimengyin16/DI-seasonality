
% Signal Processing Toolbox required

% Paper: https://arxiv.org/pdf/2102.01742.pdf

%% Preparation
addpath CiSSA_matlab\;


%% Loading data
df_DINW = readtable("C:\GitHub\DI-seasonality\CiSSA\data_DINW.csv");
df_DINW;

df_DINW_pre = readtable("C:\GitHub\DI-seasonality\CiSSA\data_DINW_preCovid.csv");
df_DINW_pre;

m_SSDI = table2array(df_DINW(:, "SSDI"));
m_SSI  = table2array(df_DINW(:, "SSI"));
m_conc = table2array(df_DINW(:, "Concurrent"));

m_SSDI_pre = table2array(df_DINW_pre(:, "SSDI"));
m_SSI_pre  = table2array(df_DINW_pre(:, "SSI"));
m_conc_pre = table2array(df_DINW_pre(:, "Concurrent"));


%% Do CiSSA
% determine the value of L
% T = 254 for 2000:10 to 2021:11 (preCovid: T = 233)
% L must below T/2 = 127 (PreCovid 116.5), 
% and multiple of the largest frequency considered (8 years, 96 months for business cycle)
% Use L = 96
% H:
%  - 0 (default): autoregressive
%  - 1 Mirroring. 
%  - 2 No extention

L = 24; % Re Poncela's suggestions, use shorter bandwidth to capture changes in seasonality
H = 0;
[Z_SSDI, psd_SSDI] = cissa(m_SSDI, L, H);
[Z_SSI,  psd_SSI]  = cissa(m_SSI, L, H);
[Z_conc, psd_conc] = cissa(m_conc, L, H);

[Z_SSDI_pre, psd_SSDI_pre] = cissa(m_SSDI_pre, L, H);
[Z_SSI_pre,  psd_SSI_pre]  = cissa(m_SSI_pre, L, H);
[Z_conc_pre, psd_conc_pre] = cissa(m_conc_pre, L, H);

%% Reconstructing trend, business cycle and seasonal components
I = 12; % for 12 months in a year
[rc_SSDI, sh_SSDI, kg_SSDI] = group(Z_SSDI, psd_SSDI,I);
[rc_SSI,  sh_SSI,  kg_SSI]  = group(Z_SSI,  psd_SSI, I);
[rc_conc, sh_conc, kg_conc] = group(Z_conc, psd_conc,I);

[rc_SSDI_pre, sh_SSDI_pre, kg_SSDI_pre] = group(Z_SSDI_pre, psd_SSDI_pre,I);
[rc_SSI_pre,  sh_SSI_pre,  kg_SSI_pre]  = group(Z_SSI_pre,  psd_SSI_pre, I);
[rc_conc_pre, sh_conc_pre, kg_conc_pre] = group(Z_conc_pre, psd_conc_pre,I);

%% write to csv

% writematrix(rc_SSDI, "DINW_L48\rc_SSDI_NW.csv");
% writematrix(rc_SSI,  "DINW_L48\rc_SSI_NW.csv");
% writematrix(rc_conc, "DINW_L48\rc_conc_NW.csv");
% 
% writematrix(rc_SSDI_pre, "DINW_L48\rc_SSDI_NW_pre.csv");
% writematrix(rc_SSI_pre,  "DINW_L48\rc_SSI_NW_pre.csv");
% writematrix(rc_conc_pre, "DINW_L48\rc_conc_NW_pre.csv");

writematrix(rc_SSDI, "DINW_L24\rc_SSDI_NW.csv");
writematrix(rc_SSI,  "DINW_L24\rc_SSI_NW.csv");
writematrix(rc_conc, "DINW_L24\rc_conc_NW.csv");

writematrix(rc_SSDI_pre, "DINW_L24\rc_SSDI_NW_pre.csv");
writematrix(rc_SSI_pre,  "DINW_L24\rc_SSI_NW_pre.csv");
writematrix(rc_conc_pre, "DINW_L24\rc_conc_NW_pre.csv");



%%
rc_SSDI;
rc_SSI;
rc_conc;

%%
sh_SSDI;
sh_SSI;
sh_conc;








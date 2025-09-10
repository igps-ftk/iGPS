function data = read_cols(file, nh)

if isempty(file)
  file='D:\gsar\coulomb\20150425.mw7.8.gorkha\hongsy\Nepal_afterslip_model\slip_54_hong(2).dat'
end

if isempty(nh)
  nh=0
end

nl=0;
fid = fopen(file);

%skip header lines
for i=1:nl
  tline = fgetl(fid);
end

tline = fgetl(fid);
nl=0;
while ischar(tline)
  line_data = str2num(tline);
  sz=size(line_data);
  if sz(1)*sz(2) > 1
    %                 disp(tline);
    nl=nl+1;
    data(:,nl)=line_data;
  end
  %             if strcmp(tline(1),' ') ~= 0
  %                 disp(tline);
  %                 nl=nl+1;
  % %                 dlines(:,nl)=sscanf(tline,'%f %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f ');
  %
  %                 %break
  %             end
  tline = fgetl(fid);
  %pause
end
fclose(fid);
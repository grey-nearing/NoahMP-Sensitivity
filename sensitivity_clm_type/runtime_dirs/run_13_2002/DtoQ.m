function Q = DtoQ(D,Parms)
%% ************************************************************************
% Translates parameter samples on [0,1] into parameter samples on 
%   ranges specified by parameter structure  
%% ************************************************************************
% -------------------------------------------------------------------------
[~,p] = size(D);
% -------------------------------------------------------------------------
Q = zeros(size(D));
ddex = 0;
% --- loop through the parameter dimensions -------------------------------
for pdex = 1:p
    if Parms(pdex,4)
        ddex = ddex+1;
        minbnd = Parms(pdex,2);
        maxbnd = Parms(pdex,3) ;  
        Q(:,pdex) = D(:,ddex)*(maxbnd-minbnd)+minbnd;
    else
        Q(:,pdex) = Parms(pdex,1);
    end
end
% -------------------------------------------------------------------------
%% *** End Function *******************************************************

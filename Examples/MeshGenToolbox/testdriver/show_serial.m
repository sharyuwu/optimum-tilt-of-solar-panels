function show_serial(n)

% plot original mesh
load oldVertices.dat; load oldCells.dat;
oldCells(:,:) = oldCells(:,:)+1;
triplot(oldCells, oldVertices(:,1), oldVertices(:,2))
title('Original Mesh')
axis equal

% plot new mesh
for (i=1:n)
    vfile = strcat('newVertices',num2str(i),'.dat');
    vfid = fopen(vfile, 'r');
    v = fscanf(vfid,'%f %f');
    fclose(vfid);

    [nv,nnv] = size(v);
    vv = zeros(nv/2, 2);
    [mv,mmv] = size(vv);
    for(j=1:mv)
        vv(j,1) = v((j-1)*2+1);
        vv(j,2) = v((j-1)*2+2);
    end
    
    
    cfile = strcat('newCells',num2str(i),'.dat');
    cfid = fopen(cfile, 'r');
    c = fscanf(cfid,'%g %g %g');
    fclose(cfid);

    [nc,nnc] = size(c);
    cc = zeros(nc/3, 3);
    [mc,mmc] = size(cc);
    for(j=1:mc)
        cc(j,1) = c((j-1)*3+1);
        cc(j,2) = c((j-1)*3+2);
        cc(j,3) = c((j-1)*3+3);
    end
    cc(:,:) = cc(:,:)+1;
    % plot
    figure
    triplot(cc, vv(:,1), vv(:,2))
    t = strcat('Mesh for "', vfile,'" and "', cfile,'"');
    title(t)
    axis equal
end


 
% cells2(:,:) = cells2(:,:)+1;
% cells3(:,:) = cells3(:,:)+1;
%     
% % plot
% triplot(cells1, vertices1(:,1), vertices1(:,2))
% title('Test Creation')
% axis equal
% 
% figure
% triplot(cells2, vertices2(:,1), vertices2(:,2))
% title('Test Refinement')
% axis equal
% 
%  figure
%  triplot(cells3, vertices3(:,1), vertices3(:,2))
%  title('Test Coarsening')
%  axis equal
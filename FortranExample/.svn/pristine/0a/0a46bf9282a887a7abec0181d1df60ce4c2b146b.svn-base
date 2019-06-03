clear; clc;

pfad = './';
filename = 'gear_end.txt';

figure('Name', filename);
hold('on'); grid('on'); axis('equal', 'square', 'vis3d');
xlabel('X-Achse'); ylabel('Y-Achse'); zlabel('Z-Achse');

data = importdata([pfad filename]);
plot3(data(:,1),data(:,2),data(:,3), 'k', 'MarkerSize',3);
if (size(data,2) > 3) 
    quiver3(data(:,1),data(:,2),data(:,3),data(:,4),data(:,5),data(:,6), 'k', 'MarkerSize',3);
end
function createcontrolfigure(X1, Y1, X2, Y2)
%CREATEFIGURE(X1, Y1, X2, Y2)
%  X1:  vector of x data
%  Y1:  vector of y data
%  X2:  vector of x data
%  Y2:  vector of y data

%  Auto-generated by MATLAB on 22-Jun-2018 17:15:09

% Create figure
figure1 = figure;

% Create axes
axes1 = axes('Parent',figure1);
hold(axes1,'on');

% Create plot
plot(X1,Y1);

% Create plot
plot(X2,Y2,'Marker','hexagram','LineStyle','none','color','k');

box(axes1,'on');

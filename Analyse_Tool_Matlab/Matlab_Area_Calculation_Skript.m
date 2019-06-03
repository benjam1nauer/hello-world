%% Importiere Datensatz und Nulle Datensatz

DataName = uigetfile(...
    '*.txt','Name des Fensters'); %Einleseaufforderung txt Datei

Datensatz = importfile(DataName); %txt Datei ist Datensatz-Matrix als mx2

DataMin = min(Datensatz(:,2)); %Suche minimalen Datenpunk y-Koordinaten

Datensatz(:,2) = Datensatz(:,2)-DataMin; %Verschiebe y-Koordinaten, sodass ymin neuer Nullpunk

%% Fit: 'untitled fit 1'. Bilde Ausgleichsgerade auf Punkteebene

[xData, yData] = prepareCurveData(Datensatz(:,1), Datensatz(:,2));

% Set up fittype and options.
ft = fittype( 'poly1' );
opts = fitoptions( 'Method', 'LinearLeastSquares' );
opts.Robust = 'Bisquare';

% Fit model to data.
[fitresult, gof] = fit( xData, yData, ft, opts );

clearvars xData yData

%% Bilde Schwerpunkt der Punktewolke

s1 = sum(Datensatz(:,1),1)/length(Datensatz(:,2));
s2 = sum(Datensatz(:,2),1)/length(Datensatz(:,2));

%% For Schleife dreht jeden Eintrag des Datensatzes

for zaehler = 1:length(Datensatz(:,1))
{
%% Bilde Drehmatrix und drehe Punk

% Drehzentrum = z
z = [s1;s2];

% Hilfspunk = q
% in Schleife, Dateneintrag Datensatz = p
p = [Datensatz(zaehler,1);Datensatz(zaehler,2)];
q = p-z;

% Drehe Hilfspunkt q um den Winkel alpha = a um den Ursprung
a = arctan(fitresult.p1);
A = [[cos(-a),-sin(-a)];[sin(-a),cos(-a)]];
qneu = A*q;
% Parallelverschiebe qneu auf z und erhalte pneu
pneu = qneu + z
% Schreibe pneu in den Datensatz
Datensatz(zaehler,1) = p(1,1);
Datensatz(zaehler,2) = p(2,1);
}
clearvars zaehler

%% Schnittpunk bestimmen

%Erneut Matrix mit Ausgleichgerade fitten, um neuen y-Achsenabschnitt zu
%finden

%% Fit: 'untitled fit 1'. Bilde Ausgleichsgerade auf Punkteebene

[xData, yData] = prepareCurveData(Datensatz(:,1), Datensatz(:,2));

% Set up fittype and options.
ft = fittype( 'poly1' );
opts = fitoptions( 'Method', 'LinearLeastSquares' );
opts.Robust = 'Bisquare';

% Fit model to data.
[fitresult, gof] = fit( xData, yData, ft, opts );

clearvars xData yData

%h_max ist der y-Achsenabschnitt der Ausgleichgeraden
h_max = fitresult.p2;

% Suche startpunk für Iteration der Schnittpunkte
startpunk=find(Datensatz(:,2)==0);

for zaehler = startpunk:length(Datensatz(:,1)
 {   if Datensatz(zaehler,2)<h_max
       
    else
        Gerade2 = @(x) ((Datensatz(zaehler,2)-Datensatz(zaehler-1,2))/(Datensatz(zaehler,1)-Datensatz(zahler-1,1)))*(x-Datensatz(zaehler-1,1)+Datensatz(zaehler-1,2);
        Schnittpunk2 = fsolve(Gerade2(x)-h_max);
        
 }
 clearvars zaehler
for zaehler = startpunk:-1:0
  { if Datensatz(zaehler,2)<h_max
       
    else
        Gerade1 = @(x) ((Datensatz(zaehler-1,2)-Datensatz(zaehler,2))/(Datensatz(zaehler-1,1)-Datensatz(zahler,1)))*(x-Datensatz(zaehler,1)+Datensatz(zaehler,2);
        Schnittpunk1 = fsolve(Gerade1(x)-h_max);
       
  }    
  clearvars zaehler  
  
%% Integral über die Fläche in der Mule bilden und in ein Array schreiben

for zaehler = 100:-1:0
    index= index+1

max_func = @(zaehler) -h_max/100*zaehler+h_max;

A_max = integral(h_max,Schnittpunkt1,Schnittpunk2)
A_min = trapz(Datensatz(:,1),Datensatz(:,2),Schnittpunkt1,Schnittpunk2);

A_real = A_max-A_min;

Volumen (index, 1) = index;
Volumen (index, 2) = A_real;

        
        
        
     
     
        



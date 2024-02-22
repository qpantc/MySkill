clear all

a = 0;
b = 10;

dx = 0.5;
x = a:dx:b;
fx = sin(x);

n = length(x); % number of data points

%% Plot
figure,
plot(x,fx,'-k','LineWidth',2)
hold on
stairs(x,fx,'--r')

%% Integration
% left-sided rectangle rule
areaL = 0;
for i = 1:n-1
    areaL = areaL + fx(i)*dx;
end
areaL

% right-sided rectangle rule
areaR = 0;
for i = 1:n-1
    areaR = areaR + fx(i+1)*dx;
end
areaR

% trapezoidal rule
areaZ = 0;
for i = 1:n-1
    areaZ = areaZ + (dx/2)*(fx(i+1)+fx(i));
end
areaZ

% Using Matlab built-in functions
areaL = sum(fx(1:end-1))*dx
areaR = sum(fx(2:end))*dx
areaZ = trapz(x,fx)

%% Simpson rule 
areaSimpson = quad(@(xdummy) spline(x,fx,xdummy), a,b)






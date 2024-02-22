clear all

% numerically differentiate sin(x) on a discrete grid.
% compare with exact derivative (cos(x))

x = .1:.1:3;
f = sin(x);

plot(x,f,'k')
hold on
plot(x,f,'rx','LineWidth',2)

dx = x(2)-x(1);

n = length(f);

dfdx = zeros(n,1);

dfdx(1) = (f(2)-f(1))/(x(2)-x(1));  % forward diff at f(x_1)
for i=2:n-1
    dfdx(i)  = (f(i+1)-f(i-1))/(x(i+1)-x(i-1)); % central in between
end
dfdx(n) = (f(n)-f(n-1))/(x(n)-x(n-1));  % backward diff at f(x_n)

figure
plot(x,cos(x),'k')
hold on
plot(x,dfdx,'r')
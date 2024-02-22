clear all

dt = .2;
t = -2:.1:4;
f = sin(t);

% Exact Derivative
dfdt = cos(t);

% plotting commands
plot(t,f,'k--','LineWidth',1.2)
hold on, grid on
plot(t,dfdt,'k','LineWidth',3)
l1=legend('Function','Exact Derivative');
set(l1,'FontSize',14)
axis([-2 4 -1.5 1.5])

%%
% Forward Difference Approximation
dfdtF = (sin(t+dt)-sin(t))/dt;
% Backward Difference Approximation
dfdtB = (sin(t)-sin(t-dt))/dt;
% Central Difference Approximation
dfdtC = (sin(t+dt)-sin(t-dt))/(2*dt);

plot(t,dfdtF,'b','LineWidth',1.2) % Forward Difference
plot(t,dfdtB,'g','LineWidth',1.2) % Backward Difference
plot(t,dfdtC,'r','LineWidth',1.2) % Central Difference
l2=legend('Function','Exact Derivative','Forward Diff','Backward Diff','Central Diff')
set(l2,'FontSize',14)

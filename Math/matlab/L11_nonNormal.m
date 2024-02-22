clear all, close all, clc

t = 0:.1:1000;
A = [-.009 1; 0 -.01];
y0 = [0; 1];
[t,y] = ode45(@(t,y)A*y, t, y0);
plot(t,y)
legend('x','v')
ylabel('x, v')
xlabel('Time')

%%
t = 0:.01:20;
A = [-1 1; 0 -1];
[t,y] = ode45(@(t,y)A*y, t, y0);
plot(t,y)
xlabel('Time')
ylabel('x,v')
legend('x','v')

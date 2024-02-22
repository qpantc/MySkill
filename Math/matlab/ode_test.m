function X=ode_test(t,X0)

X=zeros(2,1);
lambda=2;
zeta=1;
% dx^2/dt^2-lambda*x(t)=0
% x(0)=1

X(1)=X0(2);
X(2)=-lambda*X0(1)-zeta*X0(2);


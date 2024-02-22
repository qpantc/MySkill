function dy = pend(t,y,g,L,d)
dy(1,1) = y(2);
dy(2,1) = (g/L)*sin(y(1))-d*y(2);
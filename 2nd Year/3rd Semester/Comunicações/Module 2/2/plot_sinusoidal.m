function [ fx, fy ] = plot_sinusoidal()

close all;
%Apresenta as figuras geradas
t1 = 0: .0001 : 4/200;
x = 2*cos(2*pi*1000*t1) + 4*cos(2*pi*1200*t1);
figure(1);
plot(t1,x); grid on; title ('x(t)');
Ex = sum ( x.* x);

t2 = 0: .000001 : 4/2000;
y= 5;
for k=1 : 3
    y= y + k*cos(2*pi*2000*t2);
end
figure(2);
plot(t2,y); grid on; title ('y(t)');

%mdc
fx = 200;
fy = 2000;
function [fx, fy] = plot_sinusoidal()

% Limpar a consola.
clc;

% Criar o vetor de pontos - grelha de tempo.
t = 0 : 0.0001 : 4/200;	

x = 2 * cos(2 * pi * 1000 * t) + 4 * cos(2 * pi * 1200 * t);
figure; 
plot( t, x); 
legend (' x(t)=2 * cos(2 * pi * 1000 * t) + 4 * cos(2 * pi * 1200 * t)' );
grid on;
title( sprintf(' x(t) '));

syms k
y = 5 + symsum(k * cos (2 * pi * k * 2000 * t), k, 1, 3);
figure; 
plot( t, y); 
legend (' y(t)=symsum(5 + k * cos (2 * pi * k * 2000 * t), k, 1, 3)' );
grid on;
title( sprintf(' y(t) '));

fx = gcd(1000, 1200);
fy = gcd(2000, gcd(4000, 8000));
fprintf("f0x = %d Hz \n", fx);
fprintf("f0y = %d Hz \n", fy);

end


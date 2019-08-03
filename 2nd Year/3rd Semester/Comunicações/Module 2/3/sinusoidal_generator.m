function [ t , x ] = sinusoidal_generator( A, f0, fase, FS, Tx )

% Criar o vetor de pontos - grelha de tempo.
t = 0 : 1/FS : Tx;
x = A * cos(2 * pi * f0 * t + fase);

sound(x);
%uncomment in 4c and 4d
%figure; 
%plot( t, x); 

end


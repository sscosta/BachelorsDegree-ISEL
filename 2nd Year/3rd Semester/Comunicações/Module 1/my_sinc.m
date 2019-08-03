%
% ISEL - Instituto Superior de Engenharia de Lisboa.
%
% LEIC - Licenciatura em Engenharia Informatica e de Computadores.
%
% Com  - Comunica??es.
% 
%  
% my_sinc.m
% Fun??o para desenho de sinc(t) e sinc(at) no mesmo gr?fico.
% Recebe:
%       a, par?metro de escalamento da sinc(at).
%       b, parametro de escalamento da sinc(at - b).
% Retorna:
%       t, eixo dos tempos sobre o qual as sincs foram definidas.
%       x, amostras de sinc(t).
%       y, amostras de sinc(at).
function [t, x, y] = my_sinc(a, b)

% Verificar o n?mero de par?metros de entrada.
fprintf(' Fun??o chamada com %d par?metros de entrada\n', nargin );
if 0==nargin
    % Sem par?metros de entrada.
    % Atribuir o valor de 'a' e 'b' por omiss?o.
    a = 2;
    b = 2;
end

% Verificar o n?mero de par?metros de sa?da.
fprintf(' Fun??o chamada com %d par?metros de sa?da\n', nargout );


% Criar o vetor de pontos - grelha de tempo.
t = -3 : 0.01 : 3;			

% Definir as sincs.
x = sinc(t);
figure; 
plot( t, x); 
legend (' x(t)=sinc(t)' );
grid on;
xlabel(' Tempo ');
ylabel(' Amplitude ');
title( sprintf(' sinc(t) '));

y = sinc(a*t - b);
figure; 
plot( t, y); 
legend (sprintf('y(t)=sinc(%.1f t - %.1f)',a , b ));
grid on;
xlabel(' Tempo ');
ylabel(' Amplitude ');
title( sprintf(' sinc(%.1f t - %.1f)',a ,b ) );






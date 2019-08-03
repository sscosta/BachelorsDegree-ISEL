%
% ISEL - Instituto Superior de Engenharia de Lisboa.
%
% LEIC - Licenciatura em Engenharia Informatica e de Computadores.
%
% Com  - Comunicações. 
%
% analysis.m
% Análise de sinal contido em ficheiro wave ou vetor dado como parametro.
% Recebe:
%      filenameOrvector, nome do ficheiro wave ou vetor com as amostras do sinal.
%      printImage ( 0 ou 1), indica se a imagem deverá ser escrita num ficheiro gráfico. 
% Retorna:
%      um vetor com sinal contido no ficheiro / ou vetor passado por
%      parametro
function x = analysis( filenameOrvector, printImage )

% Testar se e' string (nome de ficheiro) ou vetor.
if ischar(filenameOrvector) 
    
    filename = filenameOrvector;
    
    % E' uma string. Ler o ficheiro.
    [x, Fs] = audioread( filename );    
    
    % Mostrar parâmetros do sinal.
    fprintf('Ficheiro. Sinal com %.3f segundos  Fs=%d Hz  \n', length(x)/Fs, Fs );
else
    % E' um vetor.
    x = filenameOrvector;
    
    % Assumir os valores de Fs e n.
    Fs = 6000;
    n = 8;
    fprintf('vetor. Assume-se Fs=%d e n=%d bit/amostra \n', Fs, n );    
    filename = 'vetor';
end


% Calibrar o eixo dos tempos em segundos.
time = (0 : 1 : length(x)-1) / Fs;

% Calibrar o eixo das frequências em Hertz.
freq = ((0 : 1 : length(x)-1)*Fs / ( length(x) )) - Fs/2;

% Mostrar o sinal no tempo e o seu espectro. 
% Tempo.
subplot(2,2,1); 
plot( time, x ); 
grid on; title(filename); 
xlabel('Tempo [seg]'); ylabel('Amplitude');

% Espectro.
% FFT calcula o espectro de x
% FFTSHIFT faz a representação em espectro bilateral.
CK = fftshift( fft(x) );  

% Normalizar pelo número de pontos do sinal (periódico).
CK = CK / length(x);

% Espetro de amplitude (módulo).
subplot(2,2,2); 
plot( freq, abs(CK) ); 
grid on; title('Espetro de amplitude (módulo) '); 
xlabel('Frequência [Hz]'); ylabel('Módulo');

% Espectrograma.
subplot(2,2,3); 
specgram( x , 256, Fs); 
grid on; title('Espetrograma'); 
xlabel('Tempo [seg]'); ylabel('Frequência [Hz]');

% Espetro de fase.
subplot(2,2,4); 
plot( freq, angle(CK) ); 
grid on; title('Espetro de fase '); 
xlabel('Frequência [Hz]'); ylabel('Fase');

if (printImage==1)
    % Escrever a figura num ficheiro JPEG.
    print( gcf, '-djpeg100', strcat(strtok( filename, '.'),'.jpg') );
end

return;
        
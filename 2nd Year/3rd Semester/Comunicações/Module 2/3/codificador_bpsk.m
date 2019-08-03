

function [bits_codificados] = codificador_bpsk(filename,Fs, Rb, debug)

%acessorios
%pode ser adaptado a codificador generico
%abrir pasta + abrir txt com --descriptor.txt
% ver se cabeçalho match
% ver tipo
% Frequency of sampling
%Binary Debit
%line_ex = fgetl(fid)  % read line

%TEST originalMessage2.mat



% Open file.
fid = fopen( filename, 'rt' );

%Fs = 8000;      %Hz
%Rb =  1000;     %Binary debit bit/s

%if -1==fid
%    error( sprintf('Error opening file %s for reading ',filename) );
%end



%C = textscan('codificador',%u8,N)

word = f
wordbinary = dec2bin(word', 7) - '0';

word = 'hello';
wordbinary = dec2bin(word', 7) - '0'
originalword = char(bin2dec(char(wordbinary + '0')))'


fclose(fid);
end
%
% ISEL - Instituto Superior de Engenharia de Lisboa.
% 
% LEIC - Licenciatura em Engenharia Informatica e de Computadores.
%
% Com  - Comunicações.
%
% vector2file.m
% Escreve um vector em ficheiro.
%
% Recebe:   x, vetor.
%           filename, nome do ficheiro destino.
%
%
function vector2file(x, filename)

% Open file.
fid = fopen( filename, 'wb' );

% Check descriptor.
if -1==fid
    error( sprintf('Error opening file %s for writing ',filename) );
end

% Write the vector.
fwrite( fid, x, 'uchar' );

% Close file.
fclose( fid );

return;


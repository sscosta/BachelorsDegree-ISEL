function [signal] = dtmf_generator( key, Tx )

DTMF1 = 0;
DTMF2 = 0;
FS = 12000;

switch key
    case {1,4,7,'*'}
        DTMF1 = 1209;
        
    case {2,5,8,0}
        DTMF1 = 1336;
        
    case {3,6,9,'#'}
        DTMF1 = 1477;
        
    case {'A','B','C','D'}
        DTMF1 = 1633;
       
    otherwise
        DTMF1 = 0;
end

switch key
    case {1,2,3,'A'}
        DTMF2 = 697;
        
    case {4,5,6,'B'}
        DTMF2 = 770;
        
    case {7,8,9,'C'}
        DTMF2 = 852;
        
    case {'*',0,'#','D'}
        DTMF2 = 941;
       
    otherwise
        DTMF2 = 0;
end

[ t1 x1 ] = sinusoidal_generator( 1, DTMF1, 0, FS, Tx );
[ t2 x2 ] = sinusoidal_generator( 1, DTMF2, 0, FS, Tx );

signal = x1 + x2;
%sound(signal);

end
function [ text ] = decodeMessage2( signal )

FS = 80000;
Rb = 1000;

timeOfCom = length(signal)/FS;
nBits = ceil(timeOfCom * Rb);

%limitEBit = (((max(signal) ^ 2) / 2) * (1/Rb)) / 2;

while rem(nBits,8) ~= 0
    nBits = nBits + 1;
end

n = ceil(length(signal)/nBits); %pontos por bit
decodedBin = zeros(1, nBits);

lowerBound = 1;
for i = 1 : 1 : nBits
    upperBound = i * n;
    if (upperBound > length(signal))
        upperBound = length(signal);
    end
        
    x = signal(lowerBound:upperBound);
    
    refPulse = cos(2*pi*294*(1/Rb)); %294 = 1/0.0035
    x = x.*refPulse;
    x = sum(x);    
    
    if (x >= 0)
        decodedBin(i) = 1;
    end
    
    lowerBound = upperBound + 1;
end

text = '';
for i = 1 : 8 : nBits
    upperBound = i + 7;
    
    if (upperBound > length(decodedBin))
       upperBound = length(decodedBin);
    end 
    
    c = decodedBin(i:upperBound);
    str_x = num2str(c);
    str_x(isspace(str_x)) = '';
    
    text = strcat(text, char(bin2dec(str_x)));
end

end

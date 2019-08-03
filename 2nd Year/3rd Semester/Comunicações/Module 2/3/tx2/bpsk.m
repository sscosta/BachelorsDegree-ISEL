function [ text ] = bpsk ( signal )

fc = 4000;
fs = 80000;
br = 1000;

onebit=fs/br; %number of samples per bit

n=(length(signal)-1)/onebit; %number of bits transmitted

b=onebit;
c=1;
k=[];

for m=1:n; %iteration from 1 to number of bits

y=signal(c:b); % take the needed signal for one bit

q=cos(2*pi*((c:b)-1)*fc/fs);
i=-cos(2*pi*((c:b)-1)*fc/fs);
a=y.*q; % (received signal);
d=y.*i; % (received signal);

t= sum(a)-sum(d); %constellation
if t>0;
    p=ones(1,onebit); %create output (1) for the length of one bit
else
    p=zeros(1,onebit); %create output (0) for the length of one bit
end

k=[k p]; %accumulate the value of output into k
b=b+onebit; %update the value of b for the next input
c=c+onebit; %update the value of c for the next input
end



%subplot(2,1,1); plot(signal);grid on; %plot the input signal on the same graph

%axis([0 n*onebit -1.5 1.5]); % specify the height and the witdh of the axis
%subplot(2,1,2);plot(k,'LineWidth',1.5);grid on; %plot the output (bpsk signal)
%axis([0 n*onebit -0.5 1.5]);

text = '';
for i = 1 : 8 : onebit
    upperBound = i + 7;
    
    if (upperBound > length(k))
       upperBound = length(k);
    end 
    
    c = k(i:upperBound);
    str_x = num2str(c);
    str_x(isspace(str_x)) = '';
    
    text = strcat(text, char(bin2dec(str_x)));
end

end
function [musica, v] = concatena()
%UNTITLED4 Summary of this function goes here
%   Detailed explanation goes here
FS = 8000;
NUMBER_OF_FILES=10;
[zero, Fs] = audioread('som_0.wav');
[one, Fs] = audioread('som_1.wav');
[two, Fs] = audioread('som_2.wav');
[three, Fs] = audioread('som_3.wav');
[four, Fs] = audioread('som_4.wav');
[five, Fs] = audioread('som_5.wav');
[six, Fs] = audioread('som_6.wav');
[seven, Fs] = audioread('som_7.wav');
[eight, Fs] = audioread('som_8.wav');
[nine, Fs] = audioread('som_9.wav');

%A = randn( 1, FS*NUMBER_OF_FILES );
%for i=0:8000;

res=0;
v = [zero; one; two; three; four; five; six; seven; eight; nine]
%v = [zero one two three four five six seven eight nine]

%v= v';
for i=1: 80000
    res= res + v(i,:)^2;
end
fprintf("A energia do sinal é %d \n", res);

[musica, Fs] = audioread('musica1.wav');

end


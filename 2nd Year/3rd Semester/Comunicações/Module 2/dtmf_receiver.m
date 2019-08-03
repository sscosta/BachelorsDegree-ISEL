function dtmf_receiver(signal)
NumPoints = 6000;

ini = 1;
for c = 1 : 1 : 9
    finval = c * NumPoints;
    chunk = signal(ini:finval);

    n=length(chunk)-1;
    ratio = 12000/n;
    
    f=0:ratio:1711;
    fl = 660:ratio:986;
    fu = 1145:ratio:1711;

    FFT = abs(fft(chunk));
    FFT_Positive = FFT(1:length(f));
    
    lowFreqs = FFT_Positive(660/ratio:986/ratio);  
    highFreqs = FFT_Positive(1145/ratio:1711/ratio);
    
    [maxL,indexL] = max(lowFreqs);
    [maxU,indexU] = max(highFreqs);

    ILow = round(fl(indexL))
    IHigh = round(fu(indexU))
    
    %line 1
    if(ILow >= 660 && ILow <= 733 && IHigh >= 1145 && IHigh <= 1272)
        % Play sound
        sound = audioread('som_1.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('1');        
    elseif(ILow >= 660 && ILow <= 733 && IHigh >= 1272 && IHigh <= 1406)
        % Play sound
        sound = audioread('som_2.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('2');
    elseif(ILow>=660 && ILow<733 && IHigh >= 1406 && IHigh <= 1555)
        % Play sound
        sound = audioread('som_3.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('3');
    elseif(ILow >= 660 && ILow <= 733 && IHigh >= 1555 && IHigh <= 1711)
        disp('A');
        
        %line 2
    elseif(ILow >= 733 && ILow <= 811 && IHigh >= 1145 && IHigh <= 1272)
        % Play sound
        sound = audioread('som_4.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('4');
    elseif(ILow >= 733 && ILow <= 811 && IHigh >= 1272 && IHigh <= 1406)
        % Play sound
        sound = audioread('som_5.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('5');
    elseif(ILow >= 733 && ILow <= 811 && IHigh >= 1406 && IHigh <= 1555)
        % Play sound
        sound = audioread('som_6.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('6');
    elseif(ILow >= 733 && ILow <= 811 && IHigh >= 1555 && IHigh <= 1711)
        disp('B');
        
        %line 3
    elseif(ILow >= 811 && ILow <= 897 && IHigh >= 1145 && IHigh <= 1272)
        % Play sound
        sound = audioread('som_7.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('7');
    elseif(ILow >= 811 && ILow <= 897 && IHigh >= 1272 && IHigh <= 1406)
        % Play sound
        sound = audioread('som_8.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('8');
    elseif(ILow >= 811 && ILow <= 897 && IHigh >= 1406 && IHigh <= 1555)
        % Play sound
        sound = audioread('som_9.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('9');
    elseif(ILow >= 811 && ILow <= 897 && IHigh >= 1555 && IHigh <= 1711)
        disp('C');
        
        %line 4
    elseif(ILow >= 897 && ILow <= 986 && IHigh >= 1145 && IHigh <= 1272)
        disp('*');
    elseif(ILow >= 897 && ILow <= 986 && IHigh >= 1272 && IHigh <= 1406)
        % Play sound
        sound = audioread('som_0.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('0');
    elseif(ILow >= 897 && ILow <= 986 && IHigh >= 1406 && IHigh <= 1555)
        disp('#');
    elseif(ILow >= 897 && ILow <= 986 && IHigh >= 1555 && IHigh <= 1711)
        disp('D');
    end
    
    ini=finval+ 1;
    pause(1);
end
end
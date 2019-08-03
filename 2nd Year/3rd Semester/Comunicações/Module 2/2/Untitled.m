function dtmf_receiver( signal)

Fs=12000;
%signal = dtmf_generator(2,1);

%dividir o signal em chunks de 1/Ts =Fs length
ini=1;
for c=1:1:9
    finval=c*12000;
    chunk=signal(ini:finval);
   
    CK = fftshift( abs(fft(chunk)) );  
    CK = CK(1:((Fs/2) + 1));
    lowFreq = CK(1:1000);
    highFreq = CK(1001:end);
    [MLow,ILow]= max(lowFreq);
    [MHigh,IHigh]= max(highFreq);
    %line 1
    if(ILow>=660 && ILow<733 && IHigh>=1145 && IHigh<=1272)
        % Play sound
        sound = audioread('som_1.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('1');
    else if(ILow>=660 && ILow<733 && IHigh>=1272 && IHigh<=1406)
        % Play sound
        sound = audioread('som_2.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('2');
    else if(ILow>=660 && ILow<733 && IHigh>=1406 && IHigh<=1555)
                % Play sound
        sound = audioread('som_3.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('3');
    else if(ILow>=660 && ILow<733 && IHigh>=1555 && IHigh<=1711)    
        % Play sound
        %sound = audioread('som_A.wav');
        %s = audioplayer(sound, 8000);
        %play(s);
        disp('A');
    %line 2
        else if(ILow>=733 && ILow<811 && IHigh>=1145 && IHigh<=1272)
        % Play sound
        sound = audioread('som_4.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('4');
    else if(ILow>=733 && ILow<811 && IHigh>=1272 && IHigh<=1406)
        % Play sound
        sound = audioread('som_5.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('5');
    else if(ILow>=733 && ILow<811 && IHigh>=1406 && IHigh<=1555)
                % Play sound
        sound = audioread('som_6.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('6');
    else if(ILow>=733 && ILow<811 && IHigh>=1555 && IHigh<=1711)    
        % Play sound
        %sound = audioread('som_A.wav');
        %s = audioplayer(sound, 8000);
        %play(s);
        disp('B');
    
    
%line 3
        else if(ILow>=811 && ILow<897 && IHigh>=1145 && IHigh<=1272)
        % Play sound
        sound = audioread('som_7.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('7');
    else if(ILow>=811 && ILow<897 && IHigh>=1272 && IHigh<=1406)
        % Play sound
        sound = audioread('som_8.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('8');
    else if(ILow>=811 && ILow<897 && IHigh>=1406 && IHigh<=1555)
                % Play sound
        sound = audioread('som_9.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('9');
    else if(ILow>=811 && ILow<897 && IHigh>=1555 && IHigh<=1711)    
        % Play sound
        %sound = audioread('som_A.wav');
        %s = audioplayer(sound, 8000);
        %play(s);
        disp('C');
        
        
        %line 4
        else if(ILow>=897 && ILow<986 && IHigh>=1145 && IHigh<=1272)
        % Play sound
        %sound = audioread('som_*.wav');
        %s = audioplayer(sound, 8000);
        %play(s);
        disp('*');
    else if(ILow>=897 && ILow<986 && IHigh>=1272 && IHigh<=1406)
        % Play sound
        sound = audioread('som_0.wav');
        s = audioplayer(sound, 8000);
        play(s);
        disp('0');
    else if(ILow>=897 && ILow<986 && IHigh>=1406 && IHigh<=1555)
                % Play sound
        %sound = audioread('som_3.wav');
        %s = audioplayer(sound, 8000);
        %play(s);
        disp('#');
    else if(ILow>=897 && ILow<986 && IHigh>=1555 && IHigh<=1711)    
        % Play sound
        %sound = audioread('som_A.wav');
        %s = audioplayer(sound, 8000);
        %play(s);
        disp('D');
        
      
   ini=finval+ 1;
        end
        
        end
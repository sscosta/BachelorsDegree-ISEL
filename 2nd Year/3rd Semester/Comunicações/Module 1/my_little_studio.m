function [ myRecording ] = my_little_studio()
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here

recObj = audiorecorder;
disp('Start speaking.')
recordblocking(recObj, 1);
disp('End of Recording.');

% Store data in double-precision array.
myRecording = getaudiodata(recObj);
audiowrite('som_.wav',myRecording,8000);

%vector2file(myRecording,"zero.wav");


end


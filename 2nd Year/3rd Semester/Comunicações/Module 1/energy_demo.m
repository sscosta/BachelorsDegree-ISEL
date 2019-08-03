function energy_demo()

[musica ,voz] = concatena();

alfa = 0.9;

Ex= sum( voz.^2 );
Ey= sum( musica.^2 );
fprintf("Ex = %d \n", Ex);
fprintf("Ey = %d \n", Ey);

Ez_sum = (alfa^2 * Ex) + ((1-alfa)^2 * Ey); 
fprintf("A soma de Ex e Ez com fator alfa é %d \n",Ez_sum);

dim = length(musica)- length(voz);
x_n=zeros(1,dim);
voz = [voz' , x_n];

musica = musica';
xt_yt=musica.*voz;
Ez_demo = (alfa^2 * Ex) + ((1-alfa)^2 * Ey) - (2* (sum(xt_yt))); 
fprintf("Ez pela pela formula demonstrada é %d\n",Ez_demo);


z = (alfa*voz)+((1-alfa)*musica);
Ez = sum(z.^2);
fprintf("Ez pela soma dos dois sinais operados é %d\n",Ez);

end
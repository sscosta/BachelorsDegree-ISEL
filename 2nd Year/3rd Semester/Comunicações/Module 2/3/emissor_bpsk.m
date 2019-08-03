function [cod_bits]= emissor_bpsk (mensagem, debug)
cod_bits = modulacao(codificador_bpsk(mensagem,debug),debug);
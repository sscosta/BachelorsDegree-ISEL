package com.isel.si1.Trabalho.model;

import java.sql.Timestamp;

public class Respostas {

    private Integer processo;
    private Timestamp dataRelatorio;
    private Integer numSequencial;
    private Timestamp dataResposta;
    private String texto;


    public Integer getProcesso() {
        return processo;
    }

    public void setProcesso(Integer processo) {
        this.processo = processo;
    }

    public Timestamp getDataRelatorio() {
        return dataRelatorio;
    }

    public void setDataRelatorio(Timestamp dataRelatorio) {
        this.dataRelatorio = dataRelatorio;
    }

    public Integer getNumSequencial() {
        return numSequencial;
    }

    public void setNumSequencial(Integer numSequencial) {
        this.numSequencial = numSequencial;
    }

    public Timestamp getDataResposta() {
        return dataResposta;
    }

    public void setDataResposta(Timestamp dataResposta) {
        this.dataResposta = dataResposta;
    }

    public String getTexto() {
        return texto;
    }

    public void setTexto(String texto) {
        this.texto = texto;
    }
}

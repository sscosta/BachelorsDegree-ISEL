package com.isel.si1.Trabalho.model;

import java.sql.Timestamp;

public class Relatorios {

    private Integer   processo;
    private Timestamp dataRelatorio;
    private String texto;

    public String getTexto() {
        return texto;
    }

    public void setTexto(String texto) {
        this.texto = texto;
    }

    public Timestamp getDataRelatorio() {
        return dataRelatorio;
    }

    public void setDataRelatorio(Timestamp dataRelatorio) {
        this.dataRelatorio = dataRelatorio;
    }

    public Integer getProcesso() {
        return processo;
    }

    public void setProcesso(Integer processo) {
        this.processo = processo;
    }
}

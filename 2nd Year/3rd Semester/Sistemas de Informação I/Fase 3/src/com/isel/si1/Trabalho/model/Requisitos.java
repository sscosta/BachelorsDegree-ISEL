package com.isel.si1.Trabalho.model;

public class Requisitos {

    private String idRequisito;
    private String requisito;
    private boolean obrigatorio;
    private Integer Tipo;


    public String getIdRequisito() {
        return idRequisito;
    }
    public void setIdRequisito(String idRequisito) {
        this.idRequisito = idRequisito;
    }

    public String getRequisito() {
        return requisito;
    }
    public void setRequisito(String requisito) {
        this.requisito = requisito;
    }

    public boolean isObrigatorio() {
        return obrigatorio;
    }
    public void setObrigatorio(boolean obrigatorio) {
        this.obrigatorio = obrigatorio;
    }

    public Integer getTipo() {
        return Tipo;
    }
    public void setTipo(Integer tipo) {
        Tipo = tipo;
    }
}

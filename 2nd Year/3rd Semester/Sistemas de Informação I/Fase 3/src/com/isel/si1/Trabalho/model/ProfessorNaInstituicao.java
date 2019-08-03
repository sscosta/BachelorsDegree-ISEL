package com.isel.si1.Trabalho.model;

import java.sql.Timestamp;

public class ProfessorNaInstituicao {

    private Integer idInst;
    private Integer idProf;
    private Timestamp dataInicio;
    private String cargo;
    private String categoria;

    public Integer getIdInst() {
        return idInst;
    }

    public void setIdInst(Integer idInst) {
        this.idInst = idInst;
    }

    public Integer getIdProf() {
        return idProf;
    }

    public void setIdProf(Integer idProf) {
        this.idProf = idProf;
    }

    public Timestamp getDataInicio() {
        return dataInicio;
    }

    public void setDataInicio(Timestamp dataInicio) {
        this.dataInicio = dataInicio;
    }

    public String getCargo() {
        return cargo;
    }

    public void setCargo(String cargo) {
        this.cargo = cargo;
    }

    public String getCategoria() {
        return categoria;
    }

    public void setCategoria(String categoria) {
        this.categoria = categoria;
    }
}

package com.isel.si1.Trabalho.model;

public class ProfessorAvaliadorDoProcesso {

    private Integer processo;
    private Integer professor;
    private boolean presidente;


    public Integer getProcesso() {
        return processo;
    }

    public void setProcesso(Integer processo) {
        this.processo = processo;
    }

    public Integer getProfessor() {
        return professor;
    }

    public void setProfessor(Integer professor) {
        this.professor = professor;
    }

    public boolean isPresidente() {
        return presidente;
    }

    public void setPresidente(boolean presidente) {
        this.presidente = presidente;
    }
}

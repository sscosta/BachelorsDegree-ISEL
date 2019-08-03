package com.isel.si1.Trabalho.model;

public class Alteracao {

    private Integer processoOrigem;
    private Integer processoAlteracao;
    private String curso;
    private Integer instituicao;
    private Integer qtdAlteracoes;
    private boolean ectsProporcionais;


    public Integer getProcessoOrigem() {
        return processoOrigem;
    }

    public void setProcessoOrigem(Integer processoOrigem) {
        this.processoOrigem = processoOrigem;
    }

    public Integer getProcessoAlteracao() {
        return processoAlteracao;
    }

    public void setProcessoAlteracao(Integer processoAlteracao) {
        this.processoAlteracao = processoAlteracao;
    }

    public String getCurso() {
        return curso;
    }

    public void setCurso(String curso) {
        this.curso = curso;
    }

    public Integer getInstituicao() {
        return instituicao;
    }

    public void setInstituicao(Integer instituicao) {
        this.instituicao = instituicao;
    }

    public Integer getQtdAlteracoes() {
        return qtdAlteracoes;
    }

    public void setQtdAlteracoes(Integer qtdAlteracoes) {
        this.qtdAlteracoes = qtdAlteracoes;
    }

    public boolean isEctsProporcionais() {
        return ectsProporcionais;
    }

    public void setEctsProporcionais(boolean ectsProporcionais) {
        this.ectsProporcionais = ectsProporcionais;
    }
}

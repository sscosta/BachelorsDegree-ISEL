package com.isel.si1.Trabalho.model;
import java.sql.Timestamp;

public class Processos {


    private Integer idProcesso;
    private Timestamp dateUAlteracao;
    private Timestamp dataSubmissao;
    private String TextoSubmissao;
    private Integer profResponsavel;
    private Integer estadoAtual;
    private Integer tipo;
    private Float preco;

    public Integer getIdProcesso() {
        return idProcesso;
    }

    public void setIdProcesso(Integer idProcesso) {
        this.idProcesso = idProcesso;
    }

    public Timestamp getDateUAlteracao() {
        return dateUAlteracao;
    }

    public void setDateUAlteracao(Timestamp dateUAlteracao) {
        this.dateUAlteracao = dateUAlteracao;
    }

    public Timestamp getDataSubmissao() {
        return dataSubmissao;
    }

    public void setDataSubmissao(Timestamp dataSubmissao) {
        this.dataSubmissao = dataSubmissao;
    }

    public String getTextoSubmissao() {
        return TextoSubmissao;
    }

    public void setTextoSubmissao(String textoSubmissao) {
        TextoSubmissao = textoSubmissao;
    }

    public Integer getProfResponsavel() {
        return profResponsavel;
    }

    public void setProfResponsavel(Integer profResponsavel) {
        this.profResponsavel = profResponsavel;
    }

    public Integer getEstadoAtual() {
        return estadoAtual;
    }

    public void setEstadoAtual(Integer estadoAtual) {
        this.estadoAtual = estadoAtual;
    }

    public Integer getTipo() {
        return tipo;
    }

    public void setTipo(Integer tipo) {
        this.tipo = tipo;
    }

    public Float getPreco() {
        return preco;
    }

    public void setPreco(Float preco) {
        this.preco = preco;
    }
}

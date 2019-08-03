package com.isel.si1.Trabalho.model;

public class Cursos {

    private String sigla;
    private Integer instituicao;
    private String nome;

    public String getSigla(){return sigla;}
    public void setSigla(String sigla){this.sigla=sigla;}


    public Integer getInstituicao() {return instituicao; }
    public void setInstituicao(Integer instituicao) {this.instituicao = instituicao; }


    public String getNome() {return nome;}
    public void setNome(String nome) {this.nome = nome;}

    @Override
    public String toString() {
        return "Cursos [sigla=" + sigla + ", instituicao=" + instituicao + ", nome=" + nome;
    }
}

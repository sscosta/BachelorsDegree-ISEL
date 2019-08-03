package com.isel.si1.Trabalho.model;

public class Instituicoes {

    private Integer idInstituicao;
    private String nome;
    private String enderecoMorada;
    private String enderecoPostal;
    private String enderecoLocalidade;
    private String email;
    private String telefone;


    /**
     * @return the ID
     */
    public Integer getIdInstituicao(){
        return idInstituicao;
    }
    /**
     * @param idInstituicao the idInstituicao to set
     */
    public void setidInstituicao(int idInstituicao) {
        this.idInstituicao = idInstituicao;
    }


    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public String getEnderecoMorada() {
        return enderecoMorada;
    }

    public void setEnderecoMorada(String enderecoMorada) {
        this.enderecoMorada = enderecoMorada;
    }

    public String getEnderecoPostal() {
        return enderecoPostal;
    }

    public void setEnderecoPostal(String enderecoPostal) {
        this.enderecoPostal = enderecoPostal;
    }

    public String getEnderecoLocalidade() {
        return enderecoLocalidade;
    }

    public void setEnderecoLocalidade(String enderecoLocalidade) {
        this.enderecoLocalidade = enderecoLocalidade;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getTelefone() {
        return telefone;
    }

    public void setTelefone(String telefone) {
        this.telefone = telefone;
    }


    @Override
    public String toString() {
        return "Instituicoes [idInstituicao=" + idInstituicao + ", nome=" + nome + ", enderecoMorada=" + enderecoMorada + ", enderecoPostal=" +enderecoPostal +
                ", enderecoLocalidade=" +enderecoLocalidade +", email=" + email
                + ", telefone=" + telefone + "]";
    }

}

package com.isel.si1.Trabalho.business.Instituicoes;

public class InstituicoesFactory {

    public static InstituicoesService getNewInstance(){
        return new InstituicoesService();
    }
}

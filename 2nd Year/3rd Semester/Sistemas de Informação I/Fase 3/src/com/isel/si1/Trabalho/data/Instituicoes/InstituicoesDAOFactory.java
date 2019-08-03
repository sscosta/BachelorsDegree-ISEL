package com.isel.si1.Trabalho.data.Instituicoes;

import java.sql.Connection;

public class InstituicoesDAOFactory {

    public static IInstituicoesDAO getNewInstance(Connection theConnection){
        return new InstituicoesDAO(theConnection);
    }
}

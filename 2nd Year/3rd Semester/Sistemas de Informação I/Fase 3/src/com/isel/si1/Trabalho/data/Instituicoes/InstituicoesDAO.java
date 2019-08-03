package com.isel.si1.Trabalho.data.Instituicoes;

import com.isel.si1.Trabalho.data.IBaseDAO;
import com.isel.si1.Trabalho.model.Instituicoes;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class InstituicoesDAO extends IBaseDAO implements IInstituicoesDAO{

    private static Logger logger = Logger.getLogger(InstituicoesDAO.class.getName());
    private final String GET_ALL_Instituicoes = "select ID_Instituicao, Nome, EnderecoMorada, EnderecoPostal, EnderecoLocalidade, Email, Telefone from Instituicoes";


    InstituicoesDAO (Connection theConnection){
        super(theConnection);
    }

    @Override
    public List<Instituicoes> getAllInstituicoes() {
        List<Instituicoes> allInstituicoes = new ArrayList<>();
        Connection         aCon        = getConnection();

        // using try with resources, check this cool feature
        try (Statement aStatement = aCon.createStatement();
             ResultSet aResultSet = aStatement.executeQuery(GET_ALL_Instituicoes);) {
            processResult(allInstituicoes, aResultSet);

        } catch (SQLException theCause) {
            logger.log(Level.SEVERE, "an exception was thrown", theCause);
        }

        return allInstituicoes;
    }

    public void processResult(List<Instituicoes> allInstituicoes, ResultSet aResultSet) throws SQLException {
        while (aResultSet.next()) {
            Instituicoes anInstituicao = new Instituicoes();
            allInstituicoes.add(anInstituicao);
            anInstituicao.setidInstituicao(aResultSet.getInt("ID_Instituicao"));
            anInstituicao.setNome(aResultSet.getString("Nome"));
            anInstituicao.setEnderecoMorada(aResultSet.getString("EnderecoMorada"));
            anInstituicao.setEnderecoLocalidade(aResultSet.getString("EnderecoLocalidade"));
            anInstituicao.setEnderecoPostal(aResultSet.getString("EnderecoPostal"));
            anInstituicao.setEmail(aResultSet.getString("Email"));
            anInstituicao.setTelefone(aResultSet.getString("Telefone"));
        }
    }

}
